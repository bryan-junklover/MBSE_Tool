test_that("the sample's failure modes carry their links and responses", {
  con <- local_built_sample()
  modes <- failure_mode_list(con)
  expect_identical(modes$id, sprintf("FM-%04d", 1:4))
  expect_identical(modes$subject[modes$id == "FM-0001"], "NavSolution.gnss_valid")
  expect_identical(modes$subject[modes$id == "FM-0003"], "Flight Controller.pwr_in")
  expect_match(modes$detected_by[modes$id == "FM-0001"], "Flight modes: gnss_lost", fixed = TRUE)
  expect_match(modes$handled_by[modes$id == "FM-0001"], "Survey mission → gnss_lost → Return to home", fixed = TRUE)
  expect_match(modes$handled_by[modes$id == "FM-0001"], "SYS-012", fixed = TRUE)
})

test_that("a failure mode is added, edited and linked through the write path, and can be undone", {
  con <- local_built_sample()
  written <- add_failure_mode(con, "port|P-0016")
  id <- written$id
  row <- read_entity(con, "failure_mode", id)
  expect_identical(c(row$subject_kind, row$cause_kind, row$response_state), c("port", "lost", "placeholder"))
  update_failure_mode(con, id, "system_effect", "  The drone can't climb  ")
  expect_identical(read_entity(con, "failure_mode", id)$system_effect, "The drone can't climb")
  expect_error(update_failure_mode(con, id, "response_state", "accepted"), "Say why it's accepted")
  update_failure_mode(con, id, "response_note", "Four motors; one lost still lands")
  update_failure_mode(con, id, "response_state", "accepted")
  expect_error(update_failure_mode(con, id, "response_note", ""), "needs its reason")
  expect_error(update_failure_mode(con, id, "rev", "9"), "no rev to change")

  update_failure_mode(con, id, "subject", "interface|IF-002")
  update_failure_mode(con, id, "field_name", "lat")
  expect_identical(read_entity(con, "failure_mode", id)$field_name, "lat")
  expect_error(update_failure_mode(con, id, "field_name", "nope"), "has no field nope")
  # Moving to another subject drops a field that belonged to the old one.
  update_failure_mode(con, id, "subject", "element|C-003")
  expect_true(is.na(read_entity(con, "failure_mode", id)$field_name))

  linked <- link_failure(con, id, "handled_by", "requirement|SYS-001")
  expect_identical(failure_links(con, id)$target_id, "SYS-001")
  expect_error(link_failure(con, id, "handled_by", "sm_event|E-04"), "Handled by takes a transition, requirement, component")
  expect_error(link_failure(con, id, "detected_by", "requirement|SYS-999"), "doesn't exist")
  revert_change_set(con, linked$change_set, "engineer", "Undo")
  expect_identical(nrow(failure_links(con, id)), 0L)
})

test_that("a failure mode goes with what it's about, and its links go with it", {
  con <- local_built_sample()
  delete_entity(con, "port", "P-0003")
  expect_false("FM-0003" %in% failure_mode_list(con)$id)
  delete_entity(con, "failure_mode", "FM-0001")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM link WHERE source_kind = 'failure_mode' AND source_id = 'FM-0001'")$n, 0L)
  # A state machine's transition going takes its handled_by link.
  delete_entity(con, "sm_transition", "T-04")
  expect_identical(nrow(failure_links(con, "FM-0002")[failure_links(con, "FM-0002")$link_type == "handled_by", ]), 0L)
})

test_that("the AI can draft a cause and effect but never a response", {
  con <- local_built_sample()
  expect_error(DBI::dbExecute(con, "INSERT INTO failure_mode (id, subject_kind, subject_id, cause_kind, response_state, origin, review_state)
                                    VALUES ('FM-0099', 'element', 'C-003', 'failed', 'handled', 'ai', 'unreviewed')"), "CHECK")
  expect_no_error(DBI::dbExecute(con, "INSERT INTO failure_mode (id, subject_kind, subject_id, cause_kind, origin, review_state)
                                       VALUES ('FM-0099', 'element', 'C-003', 'failed', 'ai', 'unreviewed')"))
})

test_that("the six checks find the sample's gaps, and fixes or reasons clear them", {
  con <- local_built_sample()
  findings <- failure_findings(con)
  count <- function(rule) sum(failure_findings(con)$rule == rule)
  expect_identical(sort(findings$subject_id[findings$rule == "open_response"]), c("FM-0003", "FM-0004"))
  expect_identical(findings$subject_id[findings$rule == "missing_signal_loss"], "P-0006")
  expect_identical(count("missing_power_loss"), 3L)
  expect_identical(count("missing_over_temperature"), 4L)
  expect_identical(count("missing_out_of_range"), 5L)
  expect_identical(count("handled_unlinked"), 0L)

  # Covering a case clears it; an out-of-range failure on the whole interface covers every field.
  add_failure_mode(con, "port|P-0006", "stale")
  expect_identical(count("missing_signal_loss"), 0L)
  add_failure_mode(con, "interface|IF-002", "out_of_range")
  expect_identical(count("missing_out_of_range"), 1L)
  # Setting a check aside needs a reason, and can be undone.
  expect_error(waive_failure_check(con, "missing_power_loss", "port", "P-0016", " "), "Say why")
  expect_error(waive_failure_check(con, "open_response", "failure_mode", "FM-0003", "no"), "Only the missing-case checks")
  waived <- waive_failure_check(con, "missing_power_loss", "port", "P-0016", "Each motor has its own ESC fuse")
  expect_identical(count("missing_power_loss"), 2L)
  revert_change_set(con, waived$change_set, "engineer", "Undo")
  expect_identical(count("missing_power_loss"), 3L)

  # Handled with nothing linked, then handled by a requirement nothing verifies.
  update_failure_mode(con, "FM-0003", "response_state", "handled")
  expect_identical(count("handled_unlinked"), 1L)
  link_failure(con, "FM-0003", "handled_by", "requirement|SYS-002")
  expect_identical(count("handled_unlinked"), 0L)
  expect_identical(failure_findings(con)$subject_id[failure_findings(con)$rule == "handled_unverified"], "FM-0003")
  # FM-0004 and the two added above are still undecided, and FM-0003 is unverified.
  expect_identical(model_health(con)$failures$open, 4L)
})
