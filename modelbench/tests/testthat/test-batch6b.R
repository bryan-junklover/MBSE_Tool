test_that("an AI proposal reopens when what it made is undone, and goes when its target does", {
  con <- local_built_sample()
  id <- insert_proposal(con, list(task = "gap_inference", proposal_kind = "sketch_element", title = "What?",
                                  payload_json = canonical_json(list(name = "Watchdog", stereotype = "HW", ports = list()))))
  kept <- keep_sketch(con, id, "Needed")
  revert_change_set(con, kept$change_set, "engineer", "Undo")
  proposal_list(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT status FROM ai_proposal WHERE id = ?", params = list(id))$status, "open")
  question <- insert_proposal(con, list(task = "trace_coach", proposal_kind = "question", source = "ai", target_kind = "element", target_id = "C-007",
                                        title = "Which?", payload_json = "{}"))
  delete_entity(con, "element", "C-007")
  proposal_list(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT status FROM ai_proposal WHERE id = ?", params = list(question))$status, "superseded")
})

test_that("rollback puts a requirement committed since back to its draft, and cards lead with what was acted on", {
  con <- local_built_sample()
  draft <- read_entity(con, "requirement", "SYS-003")$text
  commit_requirement(con, "The power system shall supply 28 V to the flight controller.", "SYS-003")
  rollback_to_baseline(con, "BL-03")
  row <- read_entity(con, "requirement", "SYS-003")
  expect_identical(c(row$status, row$text), c("draft", draft))
  delete_entity(con, "element", "C-008")
  cards <- compare_baselines(con, "BL-03", "working")
  expect_identical(cards[[length(cards)]]$title, "FC Hardware")
  expect_identical(cards[[length(cards)]]$kind, "element")
})

test_that("import maps an export's columns, keeps [value] rows as drafts and skips repeats", {
  con <- local_built_sample()
  exported <- withr::local_tempfile(fileext = ".csv")
  export_csv(con, exported)
  again <- import_requirements(con, read_import_file(exported), "export.csv")
  expect_identical(length(again$added), 0L)
  expect_gt(again$skipped, 0L)
  fresh <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("text,level,verify by,short title", "\"The camera shall take a photo every [value] s.\",subsystem,test,Photo rate"), fresh)
  added <- import_requirements(con, read_import_file(fresh), "new.csv")$added
  row <- read_entity(con, "requirement", added)
  expect_identical(c(row$status, row$level, row$verification_method, row$short_title), c("draft", "subsystem", "test", "Photo rate"))
})

test_that("requirements trace to each other, and a child above its parent's level is flagged", {
  con <- local_built_sample()
  link_requirements(con, "derives_from", "SYS-012", "SYS-001")
  expect_identical(requirement_links(con, "SYS-012")$target_id, "SYS-001")
  expect_error(link_requirements(con, "refines", "SYS-012", "SYS-012"), "trace to itself")
  row <- read_entity(con, "requirement", "SYS-012")
  row$level <- "stakeholder"
  expect_true(any(grepl("sits under a system one", requirement_findings(row, EMPTY_TERMS, "system")$findings, fixed = TRUE)))
})

test_that("Software only reads the domain, and stereotypes and the Specification score are current", {
  con <- local_project()
  root <- write_element(con, fields = list(kind = "component", name = "Station"))$id
  app <- write_element(con, fields = list(kind = "component", name = "Logger app", parent_id = root, domain = "sw"))$id
  write_element(con, fields = list(kind = "component", name = "Board", parent_id = root, domain = "hw"))
  expect_identical(structure_view(con, "VW-software")$nodes$id, app)
  added <- add_stereotype(con, "Avionics")
  expect_error(add_stereotype(con, "avionics"), "already a stereotype")
  write_element(con, app, list(stereotype_id = added$id))
  expect_error(delete_stereotype(con, added$id), "used by 1 component")
  save_draft(con, "The Pump shall deliver 2 L per minute.")
  write_element(con, fields = list(kind = "component", name = "Pump", parent_id = root))
  table <- requirement_table(con)
  expect_identical(table$gate_score, as.integer(check_requirement(table$text, model_terms(con))$score))
})

test_that("set values take their variable's type, and an unanswered detection is flagged", {
  con <- local_built_sample()
  machine <- load_machine(con, "SM-001")
  sim <- sim_fire(machine, sim_start(machine, "S-MIS"), "gnss_lost", list(gnss_valid = "false"))
  expect_false(isTRUE(sim$variables$gnss_valid))
  bad <- sim_fire(machine, sim_start(machine, "S-MIS"), "gnss_lost", list(gnss_valid = "maybe"))
  expect_match(bad$error, "gnss_valid is a bool")
  battery <- DBI::dbGetQuery(con, "SELECT id FROM sm_event WHERE name = 'battery_low'")$id
  link_failure(con, "FM-0004", "detected_by", paste0("sm_event|", battery))
  expect_false(any(grepl("no transition responds", failure_findings(con)$text)))
  for (t in DBI::dbGetQuery(con, "SELECT id FROM sm_transition WHERE event_id = ?", params = list(battery))$id) delete_entity(con, "sm_transition", t)
  expect_true(any(grepl("FM-0004 is detected by battery_low in Flight modes, but no transition responds", failure_findings(con)$text, fixed = TRUE)))
})
