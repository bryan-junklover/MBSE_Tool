fire <- function(session, ...) session$setInputs(diagram_event = list(...))

test_that("the diagram shows the top level with the sample's gaps", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    view <- session$getReturned()$view_data()
    expect_identical(nrow(view$nodes), 8L)
    expect_identical(session$getReturned()$root(), "C-000")
  })
})

test_that("a diagram connect and a refused one reach the same write path", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    before <- count_of(con(), "SELECT count(*) AS n FROM connector")
    fire(session, type = "connect", source = "P-0004", target = "P-0016")
    expect_identical(count_of(con(), "SELECT count(*) AS n FROM connector"), before)
    expect_identical(redraw(), 1L)

    write_port(con(), "P-T", list(element_id = "C-007", name = "t_out", direction = "out", interface_id = "IF-010"))
    fire(session, type = "connect", source = "P-T", target = "P-0019")
    row <- DBI::dbGetQuery(con(), "SELECT id, label FROM connector WHERE source_port_id = 'P-T'")
    expect_identical(nrow(row), 0L)
  })
})

test_that("a valid connect persists, labels itself and can be undone", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    write_port(con(), "P-T", list(element_id = "C-004", name = "trig", direction = "out", interface_id = "IF-010"))
    fire(session, type = "connect", source = "P-T", target = "P-0019")
    row <- DBI::dbGetQuery(con(), "SELECT id, label FROM connector WHERE source_port_id = 'P-T'")
    expect_identical(row$label, "CameraTrigger")
    expect_identical(count_of(con(), "SELECT count(*) AS n FROM v_unconnected_ports"), 0L)
    session$setInputs(undo = 1)
    expect_identical(count_of(con(), "SELECT count(*) AS n FROM connector WHERE source_port_id = 'P-T'"), 0L)
    session$setInputs(redo = 1)
    expect_identical(count_of(con(), "SELECT count(*) AS n FROM connector WHERE source_port_id = 'P-T'"), 1L)
  })
})

test_that("edits in the diagram and the inspector reach the same rows", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    fire(session, type = "select", kind = "element", ids = list("C-003"))
    expect_identical(selection()$ids, "C-003")
    session$setInputs(element_change = list(field = "name", value = "Power", n = 1))
    expect_identical(read_entity(con(), "element", "C-003")$name, "Power")
    expect_identical(session$getReturned()$view_data()$nodes$name[session$getReturned()$view_data()$nodes$id == "C-003"], "Power")
    # The tree selects the same thing.
    session$setInputs(tree_select = "C-001")
    expect_identical(selection()$ids, "C-001")
    session$setInputs(property_change = list(id = "V-0014", value = "1250", n = 2))
    expect_identical(read_entity(con(), "property_value", "V-0014")$value_state, "set")
    session$setInputs(property_change = list(id = "V-0014", value = "lots", n = 3))
    expect_equal(read_entity(con(), "property_value", "V-0014")$num_value, 1250)
  })
})

test_that("adding a component, a port and moving a block all work from the diagram", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    fire(session, type = "add_component", x = 40, y = 60)
    new_id <- selection()$ids
    expect_identical(read_entity(con(), "element", new_id)$parent_id, "C-000")
    expect_equal(layout_rows(con(), "VW-all", new_id)$x, 40)
    fire(session, type = "add_port", element = new_id, side = "left")
    session$setInputs(port_name = "p", port_direction = "in", port_interface = "IF-001", port_confirm = 1)
    port <- DBI::dbGetQuery(con(), "SELECT side, interface_id FROM port WHERE element_id = ?", params = list(new_id))
    expect_identical(port$side, "left")
    expect_identical(port$interface_id, "IF-001")
    hash <- model_hash(con())
    fire(session, type = "move", id = new_id, x = 100, y = 120)
    expect_equal(layout_rows(con(), "VW-all", new_id)$x, 100)
    expect_identical(model_hash(con()), hash)
    session$setInputs(undo = 1)
    expect_equal(layout_rows(con(), "VW-all", new_id)$x, 40)
  })
})

test_that("double-click drills down, and an empty level still opens", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    fire(session, type = "open", id = "C-004")
    view <- session$getReturned()$view_data()
    expect_setequal(view$nodes$id, c("C-008", "C-009"))
    expect_identical(nrow(view$boundary_ports), 5L)
    session$setInputs(open = "C-000")
    expect_identical(session$getReturned()$root(), "C-000")
  })
})

test_that("group, delete and review work from the inspector", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    session$setInputs(group = 1)
    fire(session, type = "select", kind = "element", ids = list("C-005", "C-007"))
    session$setInputs(group = 2)
    group <- selection()$ids
    expect_identical(read_entity(con(), "element", "C-005")$parent_id, group)

    session$setInputs(tree_select = "C-010")
    session$setInputs(review = list(note = "  ", n = 1))
    expect_identical(read_entity(con(), "element", "C-010")$review_state, "unreviewed")
    session$setInputs(review = list(note = "Ops manual needs it", n = 2))
    expect_identical(read_entity(con(), "element", "C-010")$review_state, "reviewed")
    session$setInputs(delete_element = "C-010")
    expect_null(read_entity(con(), "element", "C-010"))
    expect_length(selection()$ids, 0L)
  })
})

test_that("bad inspector values are refused", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all", tree_select = "C-001")
    session$setInputs(element_change = list(field = "name", value = " ", n = 1))
    session$setInputs(element_change = list(field = "multiplicity", value = "0", n = 2))
    row <- read_entity(con(), "element", "C-001")
    expect_identical(row$name, "Navigation Sensors")
    expect_identical(row$multiplicity, 1L)
  })
})

test_that("auto-layout positions save, and only a requested one can be undone", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state), {
    session$setInputs(view = "VW-all")
    fire(session, type = "layout", user = FALSE, positions = list(list(id = "C-001", x = 1, y = 2)))
    expect_length(state$commands()$undo, 0L)
    fire(session, type = "layout", user = TRUE, positions = list(list(id = "C-001", x = 5, y = 6)))
    expect_length(state$commands()$undo, 1L)
    session$setInputs(undo = 1)
    expect_equal(layout_rows(con(), "VW-all", "C-001")$x, 1)
  })
})
