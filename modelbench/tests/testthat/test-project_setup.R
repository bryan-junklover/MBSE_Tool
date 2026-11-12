test_that("every project starts with the standard units, and they convert", {
  con <- local_project()
  units <- unit_list(con)
  expect_gt(nrow(units), 50L)
  expect_true(all(c("kg", "W", "°C", "Hz", "mAh", "m/s", "bit/s") %in% units$symbol))
  expect_equal(unit_conversion(con, 20, "°C", "K"), 293.15)
  expect_equal(unit_conversion(con, 2, "h", "min"), 120)
})

test_that("a unit of your own needs a symbol, a quantity and an SI factor, and converts", {
  con <- local_project()
  add_unit(con, "kn", "Speed", "m/s", "0.514444")
  expect_equal(unit_conversion(con, 10, "kn", "km/h"), 18.52, tolerance = 1e-3)
  expect_error(add_unit(con, "kn", "speed", "m/s", 1), "already a unit kn")
  expect_error(add_unit(con, "mph", "speed", "ft/s", 1.47), "measured against m/s")
  expect_error(add_unit(con, "", "speed", "m/s", 1), "needs a symbol")
  expect_error(add_unit(con, "a b", "speed", "m/s", 1), "spaces")
  expect_error(add_unit(con, "zz", "speed", "m/s", "-1"), "above zero")
  expect_error(add_unit(con, "zz", "", "m/s", 1), "what the unit measures")
  add_unit(con, "lux", "illuminance", "lx", 1)
  expect_true("lux" %in% unit_list(con)$symbol)
})

test_that("property definitions and budgets are added through the write path", {
  con <- local_project()
  system <- write_element(con, fields = list(kind = "component", name = "Weather station"))$id
  battery <- write_element(con, fields = list(kind = "component", name = "Battery", parent_id = system))$id
  mass <- add_property_def(con, "Mass", "number", "kg", "sum")
  expect_identical(property_def_list(con)$name, "mass")
  expect_error(add_property_def(con, "mass", "number", "kg"), "already a property called Mass")
  expect_error(add_property_def(con, "Colour", "text", "kg"), "A text property has no unit")
  expect_error(add_property_def(con, "Colour", "text", "", "sum"), "Only a number property can roll up")
  expect_error(add_property_def(con, "Range", "number", "furlong"), "isn't in the unit registry")
  supplier <- add_property_def(con, "Supplier", "text")

  add_property_to_element(con, battery, mass$id)
  value <- DBI::dbGetQuery(con, "SELECT id FROM property_value WHERE owner_id = ?", params = list(battery))$id
  write_property_value(con, value, list(value_state = "set", num_value = 2.5, unit = "kg"))
  budget <- add_budget(con, mass$id, system, "2000", "g", "max")
  status <- budget_list(con)
  expect_identical(status$text, "2500 / 2000 g")
  expect_false(status$pass)
  expect_identical(nrow(model_health(con)$budgets), 1L)
  expect_error(add_budget(con, mass$id, system, "3", "kg"), "already has a mass budget")
  expect_error(add_budget(con, mass$id, battery, "3", "W"), "can't be in W")
  expect_error(add_budget(con, mass$id, battery, "lots", "kg"), "has to be a number")
  expect_error(add_budget(con, supplier$id, battery, "3", ""), "doesn't roll up")
  revert_change_set(con, budget$change_set, "engineer", "Undo")
  expect_identical(nrow(budget_list(con)), 0L)
})

test_that("a new state machine starts in its first state and can be undone", {
  con <- local_project()
  logger <- write_element(con, fields = list(kind = "component", name = "Logger firmware"))$id
  expect_error(create_state_machine(con, "", logger), "needs a name")
  expect_error(create_state_machine(con, "Modes", "C-999"), "Choose the component")
  expect_error(create_state_machine(con, "Modes", logger, " "), "Name the state")
  made <- create_state_machine(con, "Logger modes", logger, "Sleeping")
  machine <- load_machine(con, made$id)
  expect_identical(machine$states$name[machine$states$id == machine$initial_state_id], "Sleeping")
  expect_identical(sim_start(machine)$current, machine$initial_state_id)
  revert_change_set(con, made$change_set, "engineer", "Undo")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM state_machine")$n, 0L)
})
