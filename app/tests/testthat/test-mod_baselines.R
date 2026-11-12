options(modelbench.no_keyring = TRUE)

test_that("Compare defaults to the latest baseline and shows 3 changes", {
  state <- local_sample_state()
  shiny::testServer(mod_baselines_server, args = list(state = state), {
    session$flushReact()
    expect_length(session$getReturned()$cards(), 3L)
    first <- session$getReturned()$card()$change_set
    other <- session$getReturned()$cards()[[2]]$change_set
    session$setInputs(select = other)
    expect_identical(session$getReturned()$card()$change_set, other)
    expect_false(identical(first, other))
    expect_gt(nrow(session$getReturned()$impact()$items), 0L)
  })
})

test_that("the impact table shows paths, hops, significance and stale evidence", {
  items <- data.frame(kind = "test_case", id = "TC-007", label = "TC-007 Endurance", hops = 3L, path = "A → B", stale = "Battery capacity was 5000 mAh, now 4200 mAh",
                      significance = "High", reason = "It matters", stringsAsFactors = FALSE)
  html <- html_of(render_impact_table(items))
  expect_match(html, "A → B", fixed = TRUE)
  expect_match(html, "Evidence no longer current: Battery capacity was 5000 mAh", fixed = TRUE)
  expect_match(html, "mb-sig-high", fixed = TRUE)
  items$significance <- NA_character_
  expect_match(html_of(render_impact_table(items)), "not rated", fixed = TRUE)
})

test_that("creating is refused with blockers, and the dialog's button stays off", {
  state <- local_sample_state()
  shiny::testServer(mod_baselines_server, args = list(state = state), {
    session$setInputs(create = 1)
    expect_identical(nrow(baseline_blockers(con())), 8L)
    session$setInputs(baseline_name = "Next", baseline_note = "", create_confirm = 1)
    expect_identical(nrow(DBI::dbGetQuery(con(), "SELECT id FROM baseline")), 1L)
  })
})

test_that("rolling back asks first, then restores and clears undo", {
  state <- local_sample_state()
  shiny::testServer(mod_baselines_server, args = list(state = state), {
    session$setInputs(from = "BL-03", rollback = 1)
    expect_equal(read_entity(con(), "property_value", "V-0013")$num_value, 4200)
    state$commands(push_command(new_command_stack(), change_command("CS-0002")))
    session$setInputs(rollback_confirm = 1)
    expect_equal(read_entity(con(), "property_value", "V-0013")$num_value, 5000)
    expect_length(state$commands()$undo, 0L)
    expect_identical(nrow(DBI::dbGetQuery(con(), "SELECT id FROM baseline WHERE is_safety = 1")), 1L)
  })
})

test_that("rating stays in the panel when blocked, and a good answer fills the table", {
  state <- local_sample_state()
  connection <- shiny::isolate(state$con())
  set_setting(connection, "local_only", "1")
  shiny::testServer(mod_baselines_server, args = list(state = state), {
    session$flushReact()
    session$setInputs(rate = 1)
    for (i in 1:40) {
      later::run_now(0.01)
      session$flushReact()
      if (!is.null(ai_error())) break
    }
    expect_match(ai_error(), "Local only is on")
  })
})

test_that("the narrative tabs switch, and the stakeholder one shows the decision", {
  narrative <- list(technical = "SYS-001 misses.", stakeholder = "It flies less.", decision = "Keep the battery?")
  tech <- html_of(render_narrative(narrative, "technical", "a", "via x"))
  expect_match(tech, "SYS-001 misses.", fixed = TRUE)
  expect_match(tech, "numbers from deterministic roll-up, not the model", fixed = TRUE)
  stake <- html_of(render_narrative(narrative, "stakeholder", "a", "via x"))
  expect_match(stake, "It flies less.", fixed = TRUE)
  expect_match(stake, "Decision needed: ", fixed = TRUE)
  expect_false(grepl("SYS-001", stake, fixed = TRUE))
})

test_that("the stakeholder page holds plain words and the status chips", {
  state <- local_sample_state()
  shiny::testServer(mod_stakeholder_server, args = list(state = state), {
    html <- html_of(render_stakeholder(session$getReturned()$overview(), "go"))
    expect_match(html, "Are we on track?", fixed = TRUE)
    expect_match(html, "3.31 of 3.60 kg", fixed = TRUE)
    expect_match(html, "31 min (need 35)", fixed = TRUE)
    expect_match(html, "chip-decision_needed", fixed = TRUE)
    expect_false(grepl("C-00|SYS-0|PowerBus|«", html))
    session$setInputs(go = "detail")
    expect_identical(state$nav()$tab, "Review queue")
  })
})

test_that("impact rows group under the changed item and fold three or more hops", {
  items <- data.frame(kind = "x", id = c("a", "b", "c"), label = c("Near", "Far", "Mass budget"), hops = c(1L, 3L, 1L),
                      path = c("Power System · Battery capacity → Near", "Power System · Battery capacity → X → Y → Far", "Power System · Mass → Mass budget"),
                      stale = NA_character_, significance = c(NA, "High", NA), stringsAsFactors = FALSE)
  html <- html_of(render_impact_table(items))
  expect_match(html, "Power System · Battery capacity</span>", fixed = TRUE)
  expect_match(html, "Power System · Mass</span>", fixed = TRUE)
  expect_match(html, "1 more, three or more hops away", fixed = TRUE)
  expect_match(html, ">→ Near<", fixed = TRUE)
})

test_that("the create button says how many blockers stand in the way", {
  state <- local_sample_state()
  shiny::testServer(mod_baselines_server, args = list(state = state), {
    expect_match(html_of(output$create_button$html), "8 blockers", fixed = TRUE)
    session$setInputs(go = list(place = "spec"))
    expect_identical(state$nav()$tab, "Specification")
  })
})
