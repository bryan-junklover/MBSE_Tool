count <- function(con, sql) DBI::dbGetQuery(con, sql)[[1]]

test_that("a component anything refers to can be deleted, and Undo brings it all back", {
  con <- local_built_sample()
  # C-005 is SYS-002's actor; C-001 is a lifeline; C-004 holds parts and runs a state machine.
  before <- list(actor = read_entity(con, "requirement", "SYS-002")$actor_element_id,
                 lifelines = count(con, "SELECT count(*) FROM seq_lifeline"), budgets = count(con, "SELECT count(*) FROM budget"))
  expect_identical(before$actor, "C-005")
  gone <- delete_entity(con, "element", "C-005")
  expect_true(is.na(read_entity(con, "requirement", "SYS-002")$actor_element_id))
  expect_identical(read_entity(con, "requirement", "SYS-002")$status, "committed")
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(read_entity(con, "requirement", "SYS-002")$actor_element_id, "C-005")

  gone <- delete_entity(con, "element", "C-001")
  expect_lt(count(con, "SELECT count(*) FROM seq_lifeline"), before$lifelines)
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(count(con, "SELECT count(*) FROM seq_lifeline"), before$lifelines)

  # The whole system: its parts, budgets and everything under it, all restored by Undo.
  hash <- model_hash(con)
  gone <- delete_entity(con, "element", "C-000")
  expect_identical(count(con, "SELECT count(*) FROM element WHERE kind = 'component'"), 0L)
  expect_identical(count(con, "SELECT count(*) FROM budget"), 0L)
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(count(con, "SELECT count(*) FROM budget"), before$budgets)
  expect_identical(count(con, "SELECT count(*) FROM element WHERE kind = 'component'"), 11L)
  expect_identical(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0L)
})

test_that("a deleted component's glossary alias and resolved terms are kept, unlinked", {
  con <- local_built_sample()
  expect_identical(DBI::dbGetQuery(con, "SELECT element_id FROM glossary WHERE term = 'battery'")$element_id, "C-003")
  delete_entity(con, "element", "C-003")
  expect_true(is.na(DBI::dbGetQuery(con, "SELECT element_id FROM glossary WHERE term = 'battery'")$element_id))
  expect_identical(count(con, "SELECT count(*) FROM requirement_term WHERE element_id = 'C-003'"), 0L)
})

test_that("deleting an interface leaves its ports, steps and events without one", {
  con <- local_built_sample()
  ports <- DBI::dbGetQuery(con, "SELECT id FROM port WHERE interface_id = 'IF-002'")$id
  events <- DBI::dbGetQuery(con, "SELECT id FROM sm_event WHERE interface_id = 'IF-002'")$id
  expect_gt(length(events), 0L)
  gone <- delete_entity(con, "interface", "IF-002")
  expect_true(all(is.na(DBI::dbGetQuery(con, sprintf("SELECT interface_id FROM port WHERE id IN ('%s')", paste(ports, collapse = "','")))$interface_id)))
  expect_true(all(is.na(DBI::dbGetQuery(con, sprintf("SELECT field_name FROM sm_event WHERE id IN ('%s')", paste(events, collapse = "','")))$field_name)))
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(DBI::dbGetQuery(con, sprintf("SELECT interface_id FROM sm_event WHERE id = '%s'", events[1]))$interface_id, "IF-002")
})

test_that("deleting a requirement moves its children up and keeps recorded evidence", {
  con <- local_built_sample()
  expect_error(delete_entity(con, "requirement", "SYS-001"), "SYS-001 has recorded test results")
  expect_error(delete_entity(con, "test_case", "TC-007"), "TC-007 has recorded results")
  parent <- read_entity(con, "requirement", "SYS-012")$parent_id
  child <- write_requirement(con, fields = list(text = "The flight controller shall log GNSS loss events.", level = "subsystem", parent_id = "SYS-012"))$id
  gone <- delete_entity(con, "requirement", "SYS-012")
  expect_identical(read_entity(con, "requirement", child)$parent_id, parent)
  expect_true(is.na(read_entity(con, "sm_assertion", "A-001")$requirement_id))
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(read_entity(con, "requirement", child)$parent_id, "SYS-012")
})

test_that("a state an assertion uses can't be deleted, and waivers go with their subject", {
  con <- local_built_sample()
  expect_error(delete_entity(con, "sm_state", "S-RTH"), "Assertion A-001 starts or ends in S-RTH")
  effects <- delete_effects(con, "sm_state", "S-RTH")
  expect_match(effects$refusal, "A-001")
  waive_failure_check(con, "missing_power_loss", "port", "P-0016", "Fused")
  delete_entity(con, "port", "P-0016")
  expect_identical(nrow(failure_waivers(con)), 0L)
})

test_that("delete_effects names what goes and what's only unlinked", {
  con <- local_built_sample()
  effects <- delete_effects(con, "element", "C-004")
  expect_true(all(c("element", "port", "state_machine", "seq_lifeline") %in% names(effects$removed)))
  expect_true("requirement" %in% names(effects$cleared))
  expect_null(effects$refusal)
})

test_that("rollback puts kept items back before it deletes what was added", {
  con <- local_built_sample()
  bay <- write_element(con, fields = list(kind = "component", name = "Avionics bay", parent_id = "C-000"))$id
  write_element(con, "C-001", list(parent_id = bay))
  expect_no_error(rollback_to_baseline(con, "BL-03"))
  expect_identical(read_entity(con, "element", "C-001")$parent_id, "C-000")
  expect_null(read_entity(con, "element", bay))
  expect_length(compare_baselines(con, "BL-03", "working"), 0L)
})

test_that("a largest or smallest budget with no values yet passes and says so", {
  con <- local_project()
  station <- write_element(con, fields = list(kind = "component", name = "Station"))$id
  peak <- add_property_def(con, "Peak current", "number", "A", "max")
  add_budget(con, peak$id, station, "5", "A")
  status <- budget_status(con)
  expect_identical(status$text, "no values yet / 5 A")
  expect_true(status$pass)
  expect_identical(stakeholder_meters(con)[[1]]$value_text, "Not known yet")
  expect_no_error(model_health(con))
})
