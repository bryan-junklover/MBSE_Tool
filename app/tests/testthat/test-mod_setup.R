test_that("Project › Units & properties adds properties, budgets and units", {
  state <- local_sample_state()
  shiny::testServer(mod_project_server, args = list(state = state, workspace_dir = withr::local_tempdir()), {
    session$setInputs(p_label = "Cost", p_type = "number", p_unit = "", p_rollup = "sum", p_add = 1)
    expect_true("cost" %in% property_def_list(con())$name)
    expect_match(as.character(output$properties$html), "Cost", fixed = TRUE)

    pd <- DBI::dbGetQuery(con(), "SELECT id FROM property_def WHERE name = 'power'")$id
    session$setInputs(b_property = pd, b_scope = "C-004", b_comparison = "max", b_limit = "10", b_unit = "W", b_requirement = "", b_add = 1)
    expect_true(any(budget_list(con())$scope == "C-004"))
    expect_match(as.character(output$budgets$html), "Flight Controller", fixed = TRUE)
    added <- budget_list(con())$id[budget_list(con())$scope == "C-004"]
    session$setInputs(b_delete = added)
    expect_false(any(budget_list(con())$scope == "C-004"))

    session$setInputs(u_symbol = "kn", u_quantity = "speed", u_si = "m/s", u_factor = "0.514444", u_offset = "", u_add = 1)
    expect_true("kn" %in% unit_list(con())$symbol)
    expect_match(as.character(output$units$html), "kn", fixed = TRUE)
  })
})

test_that("New state machine creates one and opens it", {
  state <- local_sample_state()
  shiny::testServer(mod_machines_server, args = list(state = state), {
    session$setInputs(new_machine = 1)
    session$setInputs(machine_name = "Camera modes", machine_owner = "C-007", machine_first = "Off", machine_confirm = 1)
    made <- DBI::dbGetQuery(con(), "SELECT id FROM state_machine WHERE name = 'Camera modes'")$id
    expect_length(made, 1L)
    session$setInputs(machine = made)
    expect_identical(machine()$states$name, "Off")
  })
})

test_that("the Outside equipment tool on Flows adds outside equipment", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state, mode = "flows"), {
    session$setInputs(set_tool = "component")
    session$setInputs(diagram_event = list(type = "add_component", x = 40, y = 40))
    added <- DBI::dbGetQuery(con(), "SELECT id, kind, parent_id FROM element WHERE name = 'New outside equipment'")
    expect_identical(added$kind, "external")
    expect_true(is.na(added$parent_id))
    expect_true(added$id %in% session$getReturned()$view_data()$nodes$id)
  })
})

test_that("a project can be copied, and another one deleted, but not the open one", {
  dir <- withr::local_tempdir()
  path <- create_project(dir, "Weather station")
  con <- init_project_db(path)
  withr::defer(DBI::dbDisconnect(con))
  write_element(con, fields = list(kind = "component", name = "Station"))
  copy <- copy_project(dir, con, "Weather station v2")
  expect_true(file.exists(copy))
  other <- init_project_db(copy)
  expect_identical(get_setting(other, "project_name"), "Weather station v2")
  expect_identical(DBI::dbGetQuery(other, "SELECT name FROM element")$name, "Station")
  DBI::dbDisconnect(other)
  expect_error(copy_project(dir, con, "Weather station v2"), "already a project")
  expect_error(delete_project(dir, basename(path), path), "Switch to another project")
  delete_project(dir, basename(copy), path)
  expect_false(file.exists(copy))
})
