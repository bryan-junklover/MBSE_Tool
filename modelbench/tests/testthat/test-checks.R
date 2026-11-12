test_that("new events and states are in scope by default, and unscoped events are named", {
  con <- local_project()
  logger <- write_element(con, fields = list(kind = "component", name = "Logger"))$id
  sm <- create_state_machine(con, "Modes", logger, "Sleeping")$id
  add_sm_state(con, sm, "Measuring")
  add_sm_event(con, sm, "timer")
  expect_setequal(unhandled_pairs(con, sm)$state_name, c("Sleeping", "Measuring"))
  add_sm_state(con, sm, "Sending")
  expect_identical(nrow(unhandled_pairs(con, sm)), 3L)
  expect_error(add_sm_event(con, sm, "timer"), "already an event called timer")
  # An event written without scope, as the sample's and older projects' may be, is named.
  write_sm_event(con, fields = list(sm_id = sm, name = "loose"))
  expect_identical(unscoped_events(con, sm)$name, "loose")
})

test_that("a budget that leaves out placeholders says so", {
  con <- local_built_sample()
  status <- budget_status(con)
  expect_identical(status$note[status$label == "Mass"], "excludes 1 placeholder")
  expect_identical(status$scope_name[1], "Survey UAV")
})

test_that("a requirement is verified only by a passing result with nothing failing", {
  con <- local_built_sample()
  checks <- requirement_assertions(con, "SYS-012")
  expect_identical(checks$id, "A-001")
  expect_identical(checks$live, "pass")
  expect_true(requirement_verification(con, "SYS-012")$verified)
  # Break the transition A-001 relies on: the assertion now fails, so SYS-012 isn't verified,
  # and FM-0001, which it handles, says why.
  write_sm_transition(con, "T-05", list(to_state_id = "S-HLD"))
  expect_identical(requirement_assertions(con, "SYS-012")$live, "fail")
  expect_false(requirement_verification(con, "SYS-012")$verified)
  findings <- failure_findings(con)
  expect_match(findings$text[findings$rule == "handled_unverified"], "SYS-012, which has a failing check", fixed = TRUE)
  # A test case with no result yet doesn't verify anything.
  add_test_case(con, "SYS-002", "Range test", "test")
  status <- requirement_verification(con, "SYS-002")
  expect_false(status$verified)
  expect_identical(status$linked, 1L)
})

test_that("baseline warnings list what doesn't block, and the baseline's note keeps them", {
  con <- local_built_sample()
  warnings <- baseline_warnings(con)
  expect_true(all(c("budget", "analysis", "state_machine", "failure", "requirement") %in% warnings$kind))
  expect_true(any(grepl("Endurance misses its limit", warnings$text, fixed = TRUE)))
  fresh <- local_project()
  station <- write_element(fresh, fields = list(kind = "component", name = "Station"))$id
  power <- add_property_def(fresh, "Power", "number", "W", "sum")
  add_budget(fresh, power$id, station, "5", "W")
  add_property_to_element(fresh, station, power$id)
  value <- DBI::dbGetQuery(fresh, "SELECT id FROM property_value")$id
  write_property_value(fresh, value, list(value_state = "set", num_value = 9, unit = "W"))
  made <- create_baseline(fresh, "First", "Ready for review")
  expect_true(made$ok)
  note <- DBI::dbGetQuery(fresh, "SELECT note FROM baseline WHERE id = ?", params = list(made$id))$note
  expect_match(note, "Ready for review\n\nTaken with 1 known issue:\n- Power over Station is over its limit: 9 / 5 W.", fixed = TRUE)
})
