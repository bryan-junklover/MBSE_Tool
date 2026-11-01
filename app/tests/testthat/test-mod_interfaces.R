test_that("the dictionary lists, filters and edits interfaces", {
  state <- local_sample_state()
  shiny::testServer(mod_interfaces_server, args = list(state = state), {
    session$setInputs(filter = "")
    expect_identical(nrow(session$getReturned()$rows()), 10L)
    session$setInputs(filter = "nav")
    expect_identical(session$getReturned()$rows()$name, "NavSolution")
    session$setInputs(filter = "", select = "IF-002")
    expect_identical(session$getReturned()$current()$name, "NavSolution")

    session$setInputs(interface_change = list(id = "IF-002", field = "rate", value = "20 Hz", n = 1))
    expect_equal(read_entity(con(), "interface", "IF-002")$rate_value, 20)
    session$setInputs(interface_change = list(id = "IF-002", field = "rate", value = "20 furlongs", n = 2))
    expect_equal(read_entity(con(), "interface", "IF-002")$rate_value, 20)
    session$setInputs(interface_change = list(id = "IF-002", field = "rate", value = "", n = 3))
    expect_true(is.na(read_entity(con(), "interface", "IF-002")$rate_value))
  })
})

test_that("field ranges take a range, a placeholder or n/a, and refuse nonsense", {
  state <- local_sample_state()
  shiny::testServer(mod_interfaces_server, args = list(state = state), {
    session$setInputs(select = "IF-002")
    before <- status_counts(con())$placeholders
    session$setInputs(field_change = list(id = "FLD-0005", field = "range", value = "0 … 5", n = 1))
    expect_identical(status_counts(con())$placeholders, before - 1L)
    session$setInputs(field_change = list(id = "FLD-0005", field = "range", value = "5 … 0", n = 2))
    expect_equal(read_entity(con(), "interface_field", "FLD-0005")$max_value, 5)
    session$setInputs(field_change = list(id = "FLD-0005", field = "range", value = "n/a", n = 3))
    expect_identical(read_entity(con(), "interface_field", "FLD-0005")$range_state, "not_applicable")
    session$setInputs(field_change = list(id = "FLD-0005", field = "range", value = "", n = 4))
    expect_identical(status_counts(con())$placeholders, before)
    session$setInputs(field_change = list(id = "FLD-0005", field = "unit", value = "", n = 5))
    expect_true(is.na(read_entity(con(), "interface_field", "FLD-0005")$unit))
  })
})

test_that("adding starts with placeholders, deleting checks what's in use", {
  state <- local_sample_state()
  shiny::testServer(mod_interfaces_server, args = list(state = state), {
    session$setInputs(add = 1)
    id <- selected()
    expect_identical(read_entity(con(), "interface", id)$name, "NewInterface1")
    before <- status_counts(con())$placeholders
    session$setInputs(add_field = id)
    expect_identical(status_counts(con())$placeholders, before + 1L)
    session$setInputs(delete = id)
    expect_null(read_entity(con(), "interface", id))
    expect_identical(status_counts(con())$placeholders, before)
    # Ports use this one, so the database says no.
    session$setInputs(delete = "IF-002")
    expect_false(is.null(read_entity(con(), "interface", "IF-002")))
  })
})
