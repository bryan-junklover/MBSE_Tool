GOOD <- "When the bus drops, the power system shall report status within 5 s."

test_that("the sample's stored gate results are what the gate computes", {
  con <- local_sample()
  dictionary <- model_terms(con)
  rows <- DBI::dbGetQuery(con, "SELECT id, text, gate_score, gate_pass FROM requirement")
  expect_identical(nrow(rows), 13L)
  for (i in seq_len(nrow(rows))) {
    gate <- check_requirement(rows$text[i], dictionary)
    expect_equal(gate$score, rows$gate_score[i], info = rows$id[i])
    expect_identical(as.integer(gate$passes_gate), rows$gate_pass[i], info = rows$id[i])
  }
})

test_that("commit stores the gate result, terms and uncoached state", {
  con <- local_sample()
  written <- commit_requirement(con, GOOD, fields = list(parent_id = "STK-003", req_type = "functional"))
  expect_identical(written$id, "SYS-032")
  row <- read_entity(con, "requirement", written$id)
  expect_identical(row$status, "committed")
  expect_identical(row$coach_state, "uncoached")
  expect_identical(row$gate_pass, 1L)
  expect_identical(row$actor_element_id, "C-003")
  expect_identical(row$gate_score, 100L)
  expect_true(jsonlite::validate(row$gate_json))
  terms <- DBI::dbGetQuery(con, "SELECT term, role, resolution, element_id FROM requirement_term WHERE requirement_id = 'SYS-032'")
  expect_identical(terms$element_id[terms$role == "actor"], "C-003")
})

test_that("commit is refused at every layer for gate failures and placeholders", {
  con <- local_sample()
  # UI rule
  failing <- check_requirement("The system should be reliable.", model_terms(con))
  expect_false(commit_state(failing, "The system should be reliable.")$enabled)
  placeholder <- "When the bus drops, the power system shall report status within [value] s."
  state <- commit_state(check_requirement(placeholder, model_terms(con)), placeholder)
  expect_false(state$enabled)
  expect_match(state$reason, "[value]", fixed = TRUE)
  # Core, through the workflow and through the writer
  expect_error(commit_requirement(con, placeholder), class = "modelbench_refused")
  expect_error(commit_requirement(con, "The system should be reliable."), class = "modelbench_refused")
  expect_error(write_requirement(con, "SYS-050", list(level = "system", text = "The system should be reliable.",
                                                      status = "committed", gate_pass = 1L, coach_state = "uncoached")),
               class = "modelbench_refused")
  # Database, with core out of the way
  expect_db_refuses(con, sprintf("INSERT INTO requirement (id, level, text, status, gate_pass, coach_state) VALUES ('X-1', 'system', '%s', 'committed', 1, 'uncoached')", placeholder))
  expect_null(read_entity(con, "requirement", "SYS-050"))
})

test_that("commit needs a current review once a coach is configured", {
  gate <- check_requirement(GOOD)
  expect_true(commit_state(gate, GOOD)$enabled)
  expect_match(commit_state(gate, GOOD)$reason, "uncoached")
  expect_false(commit_state(gate, GOOD, configured = TRUE)$enabled)
  expect_false(commit_state(gate, GOOD, configured = TRUE, assessed_text = "older text")$enabled)
  expect_true(commit_state(gate, GOOD, configured = TRUE, assessed_text = GOOD)$enabled)
  expect_false(commit_state(gate, "  ")$enabled)
  expect_false(coach_configured(local_sample()))
})

test_that("drafts hold anything, and a committed requirement stays committed", {
  con <- local_sample()
  draft <- save_draft(con, "The system should be reliable [maybe].", fields = list(level = "system"))
  row <- read_entity(con, "requirement", draft$id)
  expect_identical(row$status, "draft")
  expect_identical(row$gate_pass, 0L)
  revised <- commit_requirement(con, GOOD, id = draft$id)
  expect_identical(revised$rev, 2L)
  expect_identical(read_entity(con, "requirement", draft$id)$status, "committed")
  expect_error(save_draft(con, "Anything", id = draft$id), class = "modelbench_refused")
})

test_that("revising a committed requirement re-runs the gate", {
  con <- local_sample()
  expect_error(commit_requirement(con, "The UAV should fly.", id = "SYS-001"), class = "modelbench_refused")
  expect_identical(read_entity(con, "requirement", "SYS-001")$rev, 1L)
  expect_identical(commit_requirement(con, GOOD, id = "SYS-001")$rev, 2L)
})

test_that("committed details stay editable, and only real changes make a revision", {
  con <- local_sample()
  expect_null(save_requirement_details(con, "SYS-001", list(short_title = "Endurance")))
  edited <- save_requirement_details(con, "SYS-001", list(rationale = "Survey needs it"))
  expect_identical(edited$rev, 2L)
  expect_null(save_requirement_details(con, "SYS-001", list(rationale = "Survey needs it")))
  cleared <- save_requirement_details(con, "SYS-001", list(rationale = "  "))
  expect_true(is.na(read_entity(con, "requirement", cleared$id)$rationale))
  expect_error(save_requirement_details(con, "SYS-001", list(text = "x")), "Not an inspector field")
  expect_error(save_requirement_details(con, "STK-001", list(parent_id = "SYS-001")), class = "modelbench_refused")
})

test_that("the outline lists parents before children, with depth", {
  outline <- requirement_outline(local_sample())
  expect_identical(nrow(outline), 13L)
  expect_identical(outline$id[1:4], c("STK-001", "SYS-001", "SYS-031", "SYS-002"))
  expect_identical(outline$depth[outline$id == "STK-001"], 0L)
  expect_identical(outline$depth[outline$id == "SYS-001"], 1L)
  expect_identical(outline$depth[outline$id == "SYS-031"], 2L)
  expect_lt(which(outline$id == "SYS-001"), which(outline$id == "SYS-031"))
  expect_lt(which(outline$id == "SYS-031"), which(outline$id == "SYS-002"))
})

test_that("the table carries allocation and the filters work", {
  rows <- requirement_table(local_sample())
  expect_identical(rows$allocated_to[rows$id == "SYS-012"] |> strsplit(", ") |> unlist() |> sort(),
                   c("Flight Controller", "Navigation Sensors"))
  expect_true(is.na(rows$allocated_to[rows$id == "SYS-021"]))
  expect_identical(filter_requirements(rows, "geofence")$id, "SYS-021")
  expect_identical(filter_requirements(rows, "sys-012")$id, "SYS-012")
  expect_true(all(filter_requirements(rows, type = "nfr")$req_type == "nfr"))
  expect_identical(filter_requirements(rows, low_first = TRUE)$id[1], "SYS-003")
})

test_that("findings state gate problems and missing details", {
  con <- local_sample()
  row <- as.list(requirement_table(con)[requirement_table(con)$id == "SYS-007", ])
  out <- requirement_findings(row, model_terms(con))
  expect_identical(out$score, 20L)
  expect_true(any(grepl("\"easy\"", out$findings, fixed = TRUE)))
  expect_true(any(grepl("No verification method", out$findings)))
})

test_that("csv, txt and xlsx rows are read, and the text column is found", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "reqs.csv")
  writeLines(c("ID,Requirement,Owner", "A1,The UAV shall fly.,me", "A2,,you", "A3,\"Says, with comma\",them"), csv)
  rows <- read_import_file(csv)
  expect_identical(rows$text, c("The UAV shall fly.", "Says, with comma"))
  expect_identical(rows$row_no, c(1L, 3L))
  expect_match(rows$raw_json[1], "\"Owner\":\"me\"", fixed = TRUE)

  txt <- file.path(dir, "reqs.txt")
  writeLines(c("One line", "", "Two line"), txt)
  expect_identical(read_import_file(txt)$text, c("One line", "Two line"))

  xlsx <- file.path(dir, "reqs.xlsx")
  openxlsx2::write_xlsx(data.frame(Text = c("From excel"), Other = c("x")), xlsx)
  expect_identical(read_import_file(xlsx)$text, "From excel")

  bad <- file.path(dir, "reqs.pdf")
  writeLines("x", bad)
  expect_error(read_import_file(bad), class = "modelbench_refused")
  # An upload arrives under a temporary name.
  expect_identical(read_import_file(txt, "upload.txt")$text, c("One line", "Two line"))
})

test_that("the fixture imports to 10 drafts and a 9-row queue, worked one at a time", {
  con <- local_project()
  texts <- readLines(test_path("fixtures", "sample_requirements.txt"), warn = FALSE)
  rows <- data.frame(row_no = seq_along(texts), text = texts, raw_json = "{}", stringsAsFactors = FALSE)
  result <- import_requirements(con, rows, "sample_requirements.txt")
  expect_length(result$added, 10L)
  expect_identical(result$queued, 9L)
  expect_true(all(DBI::dbGetQuery(con, "SELECT status FROM requirement")$status == "draft"))

  pending <- triage_pending(con)
  expect_identical(nrow(pending), 9L)
  expect_error(triage_fix(con, pending$id[1], "The trap should still be easy."), class = "modelbench_refused")
  expect_identical(nrow(triage_pending(con)), 9L)
  fixed <- triage_fix(con, pending$id[1], "When cleaned, the trap shall reassemble within 30 s.")
  expect_identical(read_entity(con, "requirement", fixed$id)$status, "draft")
  triage_skip(con, pending$id[2])
  expect_identical(nrow(triage_pending(con)), 7L)
  expect_identical(triage_pending(con)$id[1], pending$id[3])
})

test_that("id prefixes can be set, and reserved ones can't", {
  con <- local_project()
  set_id_prefix(con, "system", "ra")
  expect_identical(commit_requirement(con, GOOD)$id, "RA-001")
  expect_error(set_id_prefix(con, "system", "IF"), class = "modelbench_refused")
  expect_error(set_id_prefix(con, "system", "A1"), class = "modelbench_refused")
  expect_no_error(set_id_prefix(con, "system", "SYS"))
})

test_that("status counts and the project context match the sample", {
  con <- local_built_sample()
  expect_identical(status_counts(con), list(unreviewed = 3L, placeholders = 4L))
  expect_identical(project_context(con), "Survey UAV · working copy · based on BL-03")
  expect_identical(project_context(local_project()), "Untitled project · working copy")
})
