GOOD <- "When the bus drops, the power system shall report status within 5 s."

# Types a draft and lets the gate's debounce run out.
type_draft <- function(session, text) {
  session$setInputs(text = text)
  session$elapse(350)
  session$flushReact()
}

test_that("the gate runs on the debounce, and only then", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    session$setInputs(text = "")
    expect_identical(session$getReturned()$gate()$text, "")
    session$setInputs(text = GOOD)
    session$flushReact()
    # Not yet: the gate result still belongs to the empty draft.
    expect_identical(session$getReturned()$gate()$text, "")
    expect_false(session$getReturned()$commit_info()$enabled)
    session$elapse(350)
    session$flushReact()
    expect_true(session$getReturned()$gate()$passes_gate)
    expect_true(session$getReturned()$commit_info()$enabled)
  })
})

test_that("editing after the gate turns Commit off until the gate catches up", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    type_draft(session, GOOD)
    expect_true(session$getReturned()$commit_info()$enabled)
    session$setInputs(text = paste(GOOD, "More"))
    session$flushReact()
    expect_false(session$getReturned()$commit_info()$enabled)
  })
})

test_that("a failing draft and a placeholder keep Commit off, and name the reason", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    type_draft(session, "The system should be reliable.")
    expect_false(session$getReturned()$commit_info()$enabled)
    expect_match(session$getReturned()$commit_info()$reason, "should")
    type_draft(session, "When the bus drops, the power system shall report status within [value] s.")
    expect_false(session$getReturned()$commit_info()$enabled)
    expect_match(session$getReturned()$commit_info()$reason, "[value]", fixed = TRUE)
  })
})

test_that("commit writes the requirement, marks it uncoached and clears the form", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    type_draft(session, GOOD)
    session$setInputs(parent = "STK-003", level = "system", type = "functional", verify = "test", title = "Bus report", commit_request = list(text = GOOD))
    row <- read_entity(con(), "requirement", "SYS-032")
    expect_identical(row$status, "committed")
    expect_identical(row$coach_state, "uncoached")
    expect_identical(row$parent_id, "STK-003")
    expect_identical(row$verification_method, "test")
    expect_identical(row$short_title, "Bus report")
    expect_identical(shiny::isolate(state$revisions$spec), 1L)
    expect_null(session$getReturned()$editing())
  })
})

test_that("a forced commit of a failing draft is refused, and nothing is written", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    before <- DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n
    placeholder <- "When the bus drops, the power system shall report status within [value] s."
    type_draft(session, placeholder)
    session$setInputs(commit_request = list(text = placeholder))
    expect_identical(DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n, before)
  })
})

test_that("a click commits the words on screen, not the last ones the server heard", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    before <- DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n
    type_draft(session, GOOD)
    expect_true(session$getReturned()$commit_info()$enabled)
    # The edit hasn't reached input$text yet when the click arrives.
    session$setInputs(commit_request = list(text = "When the bus drops, the power system shall report status within [value] s."))
    expect_identical(DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n, before)

    edited <- "When the bus drops, the power system shall report status within 9 s."
    session$setInputs(commit_request = list(text = edited))
    expect_identical(read_entity(con(), "requirement", "SYS-032")$text, edited)
  })
})

test_that("saving a draft keeps it open, and committing later revises it", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    type_draft(session, "When the bus drops, the power system shall report status within [value] s.")
    session$setInputs(level = "system", save_request = list(text = "When the bus drops, the power system shall report status within [value] s."))
    id <- session$getReturned()$editing()
    expect_identical(read_entity(con(), "requirement", id)$status, "draft")
    type_draft(session, GOOD)
    session$setInputs(commit_request = list(text = GOOD))
    row <- read_entity(con(), "requirement", id)
    expect_identical(row$status, "committed")
    expect_identical(row$rev, 2L)
  })
})

test_that("opening a requirement from the outline edits that one", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    session$setInputs(open = "SYS-007")
    expect_identical(session$getReturned()$editing(), "SYS-007")
    session$setInputs(new = 1)
    expect_null(session$getReturned()$editing())
  })
})

test_that("'Not a model term' takes the term out of the questions", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    type_draft(session, "When armed, the gimbal shall hold the camera level within 2 s.")
    expect_identical(session$getReturned()$gate()$score, 90)
    session$setInputs(term_action = list(term = "gimbal", role = "actor", action = "not_model"))
    session$flushReact()
    expect_identical(session$getReturned()$gate()$score, 100)
  })
})

test_that("adding a term as a property creates a placeholder and resolves the term", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    before <- status_counts(con())$placeholders
    type_draft(session, "The payload camera shall limit the gimbal mass to 1 kg.")
    terms <- session$getReturned()$gate()$terms
    expect_identical(terms$resolution[terms$term == "gimbal mass"], "unresolved")
    session$setInputs(term_action = list(term = "gimbal mass", role = "object", action = "property"))
    session$setInputs(term_element = "C-007", term_unit = "kg", term_confirm = 1)
    session$flushReact()
    expect_identical(status_counts(con())$placeholders, before + 1L)
    terms <- session$getReturned()$gate()$terms
    expect_identical(terms$element_id[terms$term == "gimbal mass"], "C-007")
  })
})

test_that("a term can't be added until its owner is chosen", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    before <- status_counts(con())$placeholders
    session$setInputs(term_action = list(term = "gimbal mass", role = "object", action = "property"))
    session$setInputs(term_element = "", term_unit = "kg", term_confirm = 1)
    expect_identical(status_counts(con())$placeholders, before)
  })
})

test_that("adding an interface field and a component reach the model", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    session$setInputs(term_action = list(term = "flight time", role = "object", action = "field"))
    session$setInputs(term_interface = "IF-003", term_type = "float32", term_unit = "s", term_confirm = 1)
    expect_identical(DBI::dbGetQuery(con(), "SELECT range_state FROM interface_field WHERE name = 'flight_time'")$range_state, "placeholder")
    session$setInputs(term_action = list(term = "gimbal", role = "actor", action = "component"))
    session$setInputs(term_parent = "C-000", term_confirm = 2)
    expect_identical(DBI::dbGetQuery(con(), "SELECT parent_id FROM element WHERE name = 'Gimbal'")$parent_id, "C-000")
  })
})

test_that("the EARS builder feeds the same text area", {
  state <- local_sample_state()
  shiny::testServer(mod_write_server, args = list(state = state), {
    session$setInputs(set_mode = "ears")
    expect_identical(mode(), "ears")
    session$setInputs(ears_pattern = "event", ears_actor = "Power System", ears_response = "report status",
                      ears_trigger = "the bus drops", ears_bound = "within", ears_value = "5", ears_unit = "s", ears_use = 1)
    # updateTextAreaInput only reaches the browser, so the sentence itself is checked in core and the browser pass.
    expect_identical(build_ears(input$ears_pattern, input$ears_actor, input$ears_response, input$ears_trigger,
                                input$ears_bound, input$ears_value, input$ears_unit), GOOD)
  })
})
