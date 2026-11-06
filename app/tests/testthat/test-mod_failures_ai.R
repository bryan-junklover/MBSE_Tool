options(modelbench.no_keyring = TRUE)

test_that("Suggest with AI lists suggestions; keeping one opens it for review", {
  state <- local_sample_state()
  con <- shiny::isolate(state$con())
  set_task_route(con, "failure_suggest", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  candidates <- failure_suggest_prompt(con)$candidates
  answer <- list(items = list(list(subject = candidates$n[candidates$id == "C-001"], field = "", cause_kind = "degraded",
                                   title = "What if the fix is degraded?", cause = "Multipath", local_effect = "Position wanders",
                                   system_effect = "The track drifts", confidence = "medium")))
  builder <- function(provider, model, system_prompt) {
    list(chat_structured_async = function(message, type, convert) promises::promise_resolve(answer),
         get_tokens = function() data.frame(input = 10, output = 5), get_cost = function() 0)
  }
  local_mocked_bindings(build_llm_client = builder, .package = "modelbench")
  shiny::testServer(mod_failures_server, args = list(state = state), {
    session$setInputs(suggest = 1)
    for (i in 1:60) {
      later::run_now(0.01)
      session$flushReact()
      if (nrow(suggestions()) > 0L) break
    }
    expect_identical(nrow(suggestions()), 1L)
    html <- as.character(output$suggestions$html)
    expect_match(html, "AI suggestions (1)", fixed = TRUE)
    expect_match(html, "Navigation Sensors", fixed = TRUE)
    expect_match(html, "Position wanders → The track drifts", fixed = TRUE)

    session$setInputs(keep = list(id = suggestions()$id, note = "We survey near buildings", n = 1))
    id <- selected()
    expect_identical(read_entity(con(), "failure_mode", id)$review_state, "unreviewed")
    expect_identical(nrow(suggestions()), 0L)
    expect_match(as.character(output$editor$html), "Review, with my note", fixed = TRUE)
    session$setInputs(review = list(note = "Seen on the March flights", n = 2))
    expect_identical(read_entity(con(), "failure_mode", id)$review_state, "reviewed")
  })
})
