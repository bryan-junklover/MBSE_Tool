options(modelbench.no_keyring = TRUE)

test_that("a hand link from the matrix persists, and unlinking removes it", {
  state <- local_sample_state()
  shiny::testServer(mod_trace_server, args = list(state = state), {
    expect_identical(session$getReturned()$matrix()$rows$id[session$getReturned()$matrix()$rows$unallocated], "SYS-021")
    session$setInputs(ask_link = list(source = "SYS-021", target = "C-004", name = "Flight Controller", kind = "requirement"))
    session$setInputs(link_note = "", link_confirm = 1)
    session$flushReact()
    expect_identical(count_rows(con(), "v_unallocated_requirements"), 0L)
    link <- DBI::dbGetQuery(con(), "SELECT id FROM link WHERE source_id = 'SYS-021' AND link_type = 'satisfied_by'")$id
    session$setInputs(unlink = link)
    expect_identical(count_rows(con(), "v_unallocated_requirements"), 1L)
  })
})

test_that("the allocation matrix allocates a function", {
  state <- local_sample_state()
  shiny::testServer(mod_trace_server, args = list(state = state), {
    session$setInputs(set_tab = "functions")
    expect_identical(session$getReturned()$allocation()$rows$id[session$getReturned()$allocation()$rows$unallocated], "F8")
    session$setInputs(ask_link = list(source = "F8", target = "C-004", name = "Flight Controller", kind = "function"))
    session$setInputs(link_note = "Watchdog", link_confirm = 1)
    expect_identical(count_rows(con(), "v_unallocated_functions"), 0L)
  })
})

test_that("a link chosen from candidates needs a note, and the button starts off", {
  state <- local_sample_state()
  connection <- shiny::isolate(state$con())
  pid <- insert_proposal(connection, list(task = "trace_coach", proposal_kind = "candidate_link", target_kind = "requirement", target_id = "SYS-021", title = "Who owns the fence?",
                                          payload_json = "{}", candidates_json = canonical_json(list(list(n = "C2", kind = "element", id = "C-004", name = "Flight Controller", why = "Holds position")))))
  html <- html_of(render_proposal_card(proposal_list(connection, "open", "ai")[proposal_list(connection, "open", "ai")$id == pid, ], shiny::NS("t")))
  expect_match(html, "Link to Flight Controller", fixed = TRUE)
  expect_match(html, "mb-candidate\" disabled", fixed = TRUE)
  expect_match(html, "Your rationale, required to link", fixed = TRUE)
  expect_false(grepl("C-004", html, fixed = TRUE))
  shiny::testServer(mod_trace_server, args = list(state = state), {
    session$setInputs(select = "SYS-021")
    expect_identical(session$getReturned()$open_links()$id, pid)
    session$setInputs(keep_link = list(id = pid, handle = "C2", note = "  ", n = 1))
    expect_identical(count_rows(con(), "v_unallocated_requirements"), 1L)
    session$setInputs(keep_link = list(id = pid, handle = "C2", note = "It holds position", n = 2))
    expect_identical(count_rows(con(), "v_unallocated_requirements"), 0L)
    expect_identical(DBI::dbGetQuery(con(), "SELECT origin, rationale FROM link WHERE source_id = 'SYS-021'")$origin, "ai_candidate")
  })
})

test_that("the coach stores its questions, and a blocked one says why", {
  state <- local_sample_state()
  connection <- shiny::isolate(state$con())
  set_setting(connection, "local_only", "1")
  shiny::testServer(mod_trace_server, args = list(state = state), {
    session$setInputs(select = "SYS-021", ask_coach = 1)
    for (i in 1:40) {
      later::run_now(0.01)
      session$flushReact()
      if (!is.null(coach_message())) break
    }
    expect_match(coach_message(), "Local only is on")
  })
})

test_that("Flows and Functional modes draw their own views and edit the same rows", {
  state <- local_sample_state()
  shiny::testServer(mod_model_server, args = list(state = state, mode = "flows"), {
    session$setInputs(flow_types = c("data", "energy"), quantities = TRUE)
    view <- session$getReturned()$view_data()
    expect_true("EXT-2" %in% view$nodes$id)
    expect_false("matter" %in% view$edges$flow_type)
    expect_identical(view$edges$label[view$edges$id == "K-0006"], "28 V · 380 W")
    session$setInputs(diagram_event = list(type = "move", id = "C-001", x = 5, y = 6))
    expect_equal(layout_rows(con(), "VW-flows", "C-001")$x, 5)
    expect_identical(nrow(layout_rows(con(), "VW-all", "C-001")), 0L)
  })
  shiny::testServer(mod_model_server, args = list(state = state, mode = "functional"), {
    session$flushReact()
    expect_identical(nrow(session$getReturned()$view_data()$nodes), 8L)
    session$setInputs(diagram_event = list(type = "add_component", x = 5, y = 6))
    new_id <- selection()$ids
    expect_identical(read_entity(con(), "element", new_id)$kind, "function")
    session$setInputs(allocate = list(value = "C-004", n = 1))
    expect_identical(function_allocations(con(), new_id)$component_id, "C-004")
    session$setInputs(remove_allocation = function_allocations(con(), new_id)$id)
    expect_identical(nrow(function_allocations(con(), new_id)), 0L)
    # A flow can name the connector that carries it.
    session$setInputs(realize = list(id = "K-0019", value = "K-0001", n = 1))
    expect_false(realisation_check(con())$realised[realisation_check(con())$id == "K-0019"])
    session$setInputs(undo = 1)
    expect_true(realisation_check(con())$realised[realisation_check(con())$id == "K-0019"])
  })
})

test_that("the realisation panel states the result in words", {
  fine <- data.frame(id = "K", label = "a", realised = TRUE, carried_by = "K-1", problem = NA_character_)
  expect_match(html_of(render_realisation(fine)), "All 1 functional flows map", fixed = TRUE)
  bad <- data.frame(id = "K", label = "a", realised = FALSE, carried_by = NA_character_, problem = "no physical connector joins the allocated components")
  expect_match(html_of(render_realisation(bad)), "Gap: a: no physical connector", fixed = TRUE)
})

test_that("evidence shows on the requirement, and recording makes it current", {
  state <- local_sample_state()
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(select = "SYS-001")
    html <- html_of(render_evidence(requirement_evidence(con(), "SYS-001"), session$ns))
    expect_match(html, "Stale evidence", fixed = TRUE)
    expect_match(html, "4200 mAh", fixed = TRUE)
    session$setInputs(record = "TC-007", record_result = "fail", record_ref = "FT-2", record_note = "", record_confirm = 1)
    expect_identical(nrow(stale_evidence(con())), 0L)
    session$setInputs(add_case = "new", case_name = "Bench run", case_method = "test", case_confirm = 1)
    expect_true("Bench run" %in% requirement_evidence(con(), "SYS-001")$name)
  })
})
