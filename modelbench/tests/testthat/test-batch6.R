test_that("renaming a field, component or interface carries through, and Undo reverses it", {
  con <- local_built_sample()
  field <- DBI::dbGetQuery(con, "SELECT id FROM interface_field WHERE interface_id = 'IF-002' AND name = 'gnss_valid'")$id
  renamed <- write_interface_field(con, field, list(name = "gnss_ok"))
  expect_identical(read_entity(con, "failure_mode", "FM-0001")$field_name, "gnss_ok")
  expect_identical(read_entity(con, "sm_event", "E-04")$field_name, "gnss_ok")
  expect_no_error(update_failure_mode(con, "FM-0001", "cause", "Jamming"))
  revert_change_set(con, renamed$change_set, "engineer", "Undo")
  expect_identical(read_entity(con, "failure_mode", "FM-0001")$field_name, "gnss_valid")

  write_sm_transition(con, "T-06", list(guard_expr = "prop(Power System.battery_capacity) > 4000"))
  write_element(con, "C-003", list(name = "Power Unit"))
  expect_identical(read_entity(con, "sm_transition", "T-06")$guard_expr, "prop(Power Unit.battery_capacity) > 4000")
  write_interface(con, "IF-002", list(name = "NavSol"))
  expect_identical(read_entity(con, "connector", "K-0001")$label, "NavSol")
})

test_that("deleting a field clears the names that pointed at it", {
  con <- local_built_sample()
  field <- DBI::dbGetQuery(con, "SELECT id FROM interface_field WHERE interface_id = 'IF-002' AND name = 'gnss_valid'")$id
  gone <- delete_entity(con, "interface_field", field)
  expect_true(is.na(read_entity(con, "failure_mode", "FM-0001")$field_name))
  revert_change_set(con, gone$change_set, "engineer", "Undo")
  expect_identical(read_entity(con, "failure_mode", "FM-0001")$field_name, "gnss_valid")
})

test_that("test cases are edited and unlinked, and unlinked evidence stops blocking", {
  con <- local_built_sample()
  expect_identical(sum(baseline_blockers(con)$group == "stale"), 1L)
  update_test_case(con, "TC-007", "name", "Endurance flight, second run")
  expect_error(update_test_case(con, "TC-007", "name", " "), "needs a name")
  unlinked <- unlink_test_case(con, "SYS-001", "TC-007")
  expect_identical(sum(baseline_blockers(con)$group == "stale"), 0L)
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) FROM verification_record WHERE test_case_id = 'TC-007'")[[1]], 1L)
  expect_error(unlink_test_case(con, "SYS-001", "TC-007"), "doesn't verify")
  revert_change_set(con, unlinked$change_set, "engineer", "Undo")
  expect_identical(sum(baseline_blockers(con)$group == "stale"), 1L)
})

test_that("a field name resolves as a term, and an empty draft is refused", {
  con <- local_built_sample()
  gate <- check_requirement("The flight controller shall report heading accuracy to the comms link at 5 Hz.", model_terms(con))
  expect_identical(gate$terms$resolution[gate$terms$term == "heading accuracy"], "model")
  expect_error(save_draft(con, "  "), "Write something")
})

test_that("outside equipment on Flows is drawn once at the top level", {
  con <- local_project()
  base <- write_element(con, fields = list(kind = "external", name = "Base"))$id
  save_layout(con, "VW-flows", layout_items(data.frame(id = base, x = 10, y = 10)))
  expect_identical(flows_view(con)$nodes$id, base)
})
