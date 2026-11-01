GOOD <- "When the bus drops, the power system shall report status within 5 s."
options(modelbench.no_keyring = TRUE)

fake_builder <- function(answer, log = new.env()) {
  log$n <- 0L
  function(provider, model, system_prompt) {
    list(
      chat_structured_async = function(message, type, convert) {
        log$n <- log$n + 1L
        if (inherits(answer, "error")) promises::promise_reject(answer) else promises::promise_resolve(answer)
      },
      get_tokens = function() data.frame(input = 10, output = 5), get_cost = function() 0
    )
  }
}

coach_answer <- function(rewrite = "") {
  list(criteria = list(singular = "pass", verifiable = "pass", design_independent = "question", conflicts = "none", fits_scope = "pass"),
       questions = list(list(text = "Is the bus monitored?", context = "")), example_rewrite = rewrite, conflicts = list())
}

# Runs promises and reactive flushes until `done` says so.
pump <- function(session, done) {
  for (i in 1:60) {
    later::run_now(0.01)
    session$flushReact()
    if (isTRUE(done())) return(TRUE)
  }
  FALSE
}

type_and_wait <- function(session, text) {
  session$setInputs(text = text)
  session$elapse(10)
  session$flushReact()
}

configured_state <- function(env = parent.frame()) {
  state <- local_sample_state(env)
  set_task_route(shiny::isolate(state$con()), "req_light_check", list(model = "local-test"))
  state
}

test_that("the coach holds Commit until its review matches the exact text", {
  state <- configured_state()
  log <- new.env()
  local_mocked_bindings(build_llm_client = fake_builder(coach_answer(), log), .package = "modelbench")
  shiny::testServer(mod_write_server, args = list(state = state, coach_ms = 1000), {
    session$setInputs(text = "")
    session$elapse(1100)
    type_and_wait(session, GOOD)
    # The gate has passed, but the coach's wait hasn't run out.
    session$elapse(100)
    session$flushReact()
    expect_true(session$getReturned()$gate()$passes_gate)
    expect_false(session$getReturned()$commit_info()$enabled)
    expect_identical(log$n, 0L)
    session$elapse(1000)
    expect_true(pump(session, function() !is.null(session$getReturned()$review())))
    expect_true(session$getReturned()$commit_info()$enabled)
    expect_identical(log$n, 1L)

    # An edit puts it back: the old review doesn't count for new words.
    session$setInputs(text = paste(GOOD, "Also"))
    session$elapse(100)
    session$flushReact()
    expect_null(session$getReturned()$review())
    expect_false(session$getReturned()$commit_info()$enabled)
  })
})

test_that("committing with a current review marks it reviewed, and the gate failing never calls the coach", {
  state <- configured_state()
  log <- new.env()
  local_mocked_bindings(build_llm_client = fake_builder(coach_answer(), log), .package = "modelbench")
  shiny::testServer(mod_write_server, args = list(state = state, coach_ms = 5), {
    session$setInputs(text = "")
    type_and_wait(session, "The system should be reliable.")
    session$elapse(400)
    session$flushReact()
    pump(session, function() FALSE)
    expect_identical(log$n, 0L)

    type_and_wait(session, GOOD)
    session$elapse(400)
    session$flushReact()
    expect_true(pump(session, function() !is.null(session$getReturned()$review())))
    session$setInputs(level = "system", commit_request = list(text = GOOD))
    row <- read_entity(con(), "requirement", "SYS-032")
    expect_identical(row$coach_state, "reviewed")
    expect_false(is.na(row$coach_call_id))
  })
})

test_that("a repeat of the same text uses the guard and the cache, not the model", {
  state <- configured_state()
  log <- new.env()
  local_mocked_bindings(build_llm_client = fake_builder(coach_answer(), log), .package = "modelbench")
  shiny::testServer(mod_write_server, args = list(state = state, coach_ms = 5), {
    session$setInputs(text = "")
    type_and_wait(session, GOOD)
    session$elapse(400)
    session$flushReact()
    expect_true(pump(session, function() !is.null(session$getReturned()$review())))
    type_and_wait(session, "Something else")
    session$elapse(400)
    session$flushReact()
    type_and_wait(session, GOOD)
    session$elapse(400)
    session$flushReact()
    expect_true(pump(session, function() isTRUE(session$getReturned()$review()$cached)))
    expect_identical(log$n, 1L)
  })
})

test_that("a stopped server shows a plain-word error and doesn't lock out commit", {
  state <- configured_state()
  local_mocked_bindings(build_llm_client = fake_builder(simpleError("Failed to perform HTTP request")), .package = "modelbench")
  shiny::testServer(mod_write_server, args = list(state = state, coach_ms = 5), {
    session$setInputs(text = "")
    type_and_wait(session, GOOD)
    session$elapse(400)
    session$flushReact()
    expect_true(pump(session, function() !is.null(session$getReturned()$failure())))
    expect_match(session$getReturned()$failure()$error, "LM Studio isn't running")
    expect_true(session$getReturned()$commit_info()$enabled)
    session$setInputs(level = "system", commit_request = list(text = GOOD))
    expect_identical(read_entity(con(), "requirement", "SYS-032")$coach_state, "uncoached")
  })
})

test_that("local only blocks a deep check, and the panel says why", {
  state <- configured_state()
  set_setting(shiny::isolate(state$con()), "local_only", "1")
  set_task_route(shiny::isolate(state$con()), "req_light_check", list(model = NA_character_))
  shiny::testServer(mod_write_server, args = list(state = state, coach_ms = 5), {
    session$setInputs(text = "")
    type_and_wait(session, GOOD)
    session$setInputs(deep_check = 1)
    expect_true(pump(session, function() !is.null(session$getReturned()$failure())))
    expect_match(session$getReturned()$failure()$error, "Local only is on")
  })
})

test_that("the coach panel folds the example, never applies it, and shows its source", {
  review <- list(answer = list(
    criteria = list(singular = "pass", verifiable = "pass", design_independent = "question", conflicts = "none", fits_scope = "pass"),
    questions = list(list(text = "Is the bus monitored?", context = "")), conflicts = list(),
    example = check_example_rewrite(GOOD, "When the bus falls, the power system shall report status within 5 s.")
  ), provider_id = "lmstudio", prompt_version = "req-coach v1", duration_ms = 1800, cached = TRUE)
  html <- html_of(render_coach_panel(review, NULL, FALSE, TRUE, shiny::NS("w")))
  expect_match(html, "<details>", fixed = TRUE)
  expect_false(grepl("<details open", html, fixed = TRUE))
  expect_match(html, "<del>drops,</del>", fixed = TRUE)
  expect_match(html, "<ins>falls,</ins>", fixed = TRUE)
  expect_match(html, "See the prompt", fixed = TRUE)
  expect_match(html, "cached by text", fixed = TRUE)
  expect_false(grepl("Apply|Accept", html))
  expect_lt(regexpr("Is the bus monitored", html)[1], regexpr("<details>", html)[1])

  review$answer$example <- list(status = "failed", reasons = "Contains banned wording.")
  expect_match(html_of(render_coach_panel(review, NULL, FALSE, TRUE, shiny::NS("w"))), "The example didn't pass the gate", fixed = TRUE)
})

test_that("the status strip says when local is down", {
  html <- html_of(render_status_strip(list(unreviewed = 3L, placeholders = 4L), "nav", list(local = FALSE, cloud = TRUE), list(calls = 41L, cost = 0.38)))
  expect_match(html, "local down", fixed = TRUE)
  expect_match(html, "cloud ready", fixed = TRUE)
  expect_match(html, "Today: 41 calls · $0.38", fixed = TRUE)
  expect_match(html, "Policy: tiered", fixed = TRUE)
})

test_that("the review queue keeps a sketch only with a note, and it reaches the model", {
  state <- local_sample_state()
  connection <- shiny::isolate(state$con())
  pid <- insert_proposal(connection, list(task = "gap_inference", proposal_kind = "sketch_element", title = "What?", payload_json = canonical_json(list(
    name = "Watchdog", stereotype = "HW", ports = list(list(name = "pwr", direction = "in", interface = "PowerBus28V"))))))
  html <- html_of(render_proposal_card(proposal_list(connection, "open", "ai")[proposal_list(connection, "open", "ai")$id == pid, ], shiny::NS("r")))
  expect_match(html, "Add as unreviewed draft", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
  shiny::testServer(mod_review_server, args = list(state = state), {
    session$setInputs(sources = c("structural", "ai"), status = "open")
    expect_true(pid %in% session$getReturned()$proposals()$id)
    session$setInputs(keep = list(id = pid, note = "  ", n = 1))
    expect_identical(DBI::dbGetQuery(con(), "SELECT status FROM ai_proposal WHERE id = ?", params = list(pid))$status, "open")
    session$setInputs(keep = list(id = pid, note = "Independent path", n = 2))
    created <- DBI::dbGetQuery(con(), "SELECT created_entity_id FROM ai_proposal WHERE id = ?", params = list(pid))$created_entity_id
    expect_identical(read_entity(con(), "element", created)$review_state, "unreviewed")
    expect_true(created %in% structure_view(con())$nodes$id)
    expect_identical(status_counts(con())$unreviewed, 4L)
  })
})

test_that("dismissing needs a reason, and dismissals persist through a refresh", {
  state <- local_sample_state()
  shiny::testServer(mod_review_server, args = list(state = state), {
    session$setInputs(sources = c("structural", "ai"), status = "open")
    session$setInputs(dismiss = "PR-0004", dismiss_reason = " ", dismiss_confirm = 1)
    expect_identical(DBI::dbGetQuery(con(), "SELECT status FROM ai_proposal WHERE id = 'PR-0004'")$status, "open")
    session$setInputs(dismiss_reason = "Later design", dismiss_confirm = 2)
    session$flushReact()
    expect_false("PR-0004" %in% session$getReturned()$proposals()$id)
    invalidate_db(state, "model")
    session$flushReact()
    expect_false("PR-0004" %in% session$getReturned()$proposals()$id)
  })
})

test_that("starting from a draft goes to Write, and an analysis stores what comes back", {
  state <- local_sample_state()
  connection <- shiny::isolate(state$con())
  set_task_route(connection, "gap_inference", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  answer <- list(items = list(list(kind = "question", title = "Who may command it?", body = "", target = "", confidence = "high", name = "", stereotype = "", draft = "", ports = list())))
  local_mocked_bindings(build_llm_client = fake_builder(answer), .package = "modelbench")
  pid <- insert_proposal(connection, list(task = "scope_gaps", proposal_kind = "sketch_requirement", title = "T?", payload_json = canonical_json(list(draft = "The UAV shall fly."))))
  shiny::testServer(mod_review_server, args = list(state = state), {
    session$setInputs(sources = c("structural", "ai"), status = "open")
    session$setInputs(start = pid)
    expect_identical(state$open_in_write()$text, "The UAV shall fly.")
    expect_identical(state$nav()$tab, "Draft")
    session$setInputs(run = 1)
    expect_true(pump(session, function() "Who may command it?" %in% session$getReturned()$proposals()$title))
  })
})

test_that("the settings screen saves routes and data protection", {
  state <- local_sample_state()
  shiny::testServer(mod_ai_settings_server, args = list(state = state), {
    session$setInputs(route_change = list(task = "req_light_check", field = "model", value = "qwen", n = 1))
    expect_true(coach_configured(con()))
    session$setInputs(local_only = TRUE)
    expect_true(local_only(con()))
    session$setInputs(spend_cap = "lots")
    expect_identical(get_setting(con(), "spend_cap_usd", ""), "")
    session$setInputs(spend_cap = "1.5")
    expect_identical(get_setting(con(), "spend_cap_usd", ""), "1.5")
    session$setInputs(coach_wait = "10")
    expect_identical(get_setting(con(), "coach_wait_ms", "2000"), "2000")
    session$setInputs(base_url = list(id = "lmstudio", value = "http://localhost:9/v1", n = 1))
    expect_identical(provider_row(con(), "lmstudio")$base_url, "http://localhost:9/v1")
  })
})
