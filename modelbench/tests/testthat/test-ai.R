GOOD <- "When the bus drops, the power system shall report status within 5 s."

test_that("schemas give an ellmer type, JSON and a check", {
  expect_true(jsonlite::validate(schema_to_json(COACH_SCHEMA)))
  expect_s3_class(schema_to_ellmer(COACH_SCHEMA), "S7_object")
  expect_length(schema_problems(good_coach_answer(), COACH_SCHEMA), 0L)
  bad <- good_coach_answer()
  bad$criteria$singular <- "Good"
  expect_match(schema_problems(bad, COACH_SCHEMA), "criteria.singular")
  bad <- good_coach_answer()
  bad$questions <- NULL
  expect_match(schema_problems(bad, COACH_SCHEMA), "missing questions")
})

test_that("each prompt builder's dialog text is exactly what's sent", {
  con <- local_sample()
  set_light_model(con)
  fake <- fake_client(list(good_coach_answer()))
  local_mocked_bindings(build_llm_client = fake$builder)

  coach <- coach_inputs(con, GOOD, "req_light_check", "STK-003")
  run_ai(con, coach)
  dialog <- prompt_dialog_text(coach)
  expect_match(dialog, fake$log$systems[1], fixed = TRUE)
  expect_match(dialog, fake$log$messages[1], fixed = TRUE)
  expect_identical(fake$log$systems[1], coach$system)
  expect_identical(fake$log$messages[1], coach$message)
  stored <- DBI::dbGetQuery(con, "SELECT system_prompt, user_message, schema_json FROM llm_call ORDER BY id DESC LIMIT 1")
  expect_identical(stored$system_prompt, coach$system)
  expect_identical(stored$user_message, coach$message)
  expect_match(dialog, stored$schema_json, fixed = TRUE)

  fake2 <- fake_client(list(list(items = list())))
  local_mocked_bindings(build_llm_client = fake2$builder)
  set_task_route(con, "gap_inference", list(provider_id = "lmstudio", model = "local-test", fallback_provider_id = NA, fallback_model = NA))
  gaps <- gap_prompt(con)
  run_ai(con, gaps)
  expect_identical(fake2$log$messages[1], gaps$message)
  expect_identical(fake2$log$systems[1], gaps$system)
})

test_that("the authoring rules come from the gate's constants", {
  rules <- authoring_rules_block()
  expect_match(rules, "robust", fixed = TRUE)
  expect_match(rules, sprintf("at least %d words", MIN_WORD_COUNT), fixed = TRUE)
  expect_match(coach_inputs(local_sample(), GOOD, "req_light_check")$system, "as appropriate", fixed = TRUE)
})

test_that("handles go to the model, never ids, and the map isn't sent", {
  con <- local_sample()
  prompt <- gap_prompt(con)
  expect_false(grepl("C-003", prompt$message, fixed = TRUE))
  expect_false(grepl("IF-001", prompt$message, fixed = TRUE))
  expect_match(prompt$message, "Power System", fixed = TRUE)
  expect_match(prompt$candidates_json, "C-003", fixed = TRUE)
})

test_that("related requirements put shared rare words first and stay bounded", {
  committed <- c("The camera shall take images.", "The power system shall hold the bus at 28 V.", "The UAV shall fly.")
  expect_identical(related_requirements("The power system shall report bus status.", committed, 1L), committed[2])
  expect_length(related_requirements("x", rep("The UAV shall fly.", 60), 40L), 40L)
  expect_length(related_requirements("x", character(0)), 0L)
})

test_that("routing: fallback, local-only and the spend cap", {
  con <- local_sample()
  attempts <- route_attempts(con, "req_deep_check")
  expect_identical(vapply(attempts, function(a) a$provider_id, ""), c("anthropic", "openai"))

  set_setting(con, "local_only", "1")
  expect_identical(route_attempts(con, "req_deep_check")[[1]]$blocked, "local_only")
  set_light_model(con)
  local <- route_attempts(con, "req_deep_check")
  expect_identical(local[[1]]$provider_id, "lmstudio")
  expect_identical(local[[1]]$model, "local-test")
  set_setting(con, "local_only", "0")

  set_setting(con, "spend_cap_usd", "0")
  expect_identical(route_attempts(con, "req_deep_check")[[1]]$blocked, "spend_cap")
  expect_identical(route_attempts(con, "req_light_check")[[1]]$provider_id, "lmstudio")
})

test_that("a blocked call is logged, makes no request, and says why", {
  con <- local_sample()
  set_setting(con, "local_only", "1")
  fake <- fake_client(list(good_coach_answer()))
  local_mocked_bindings(build_llm_client = fake$builder)
  result <- run_coach(con, GOOD, "req_deep_check")
  expect_false(result$ok)
  expect_identical(result$kind, "local_only")
  expect_match(result$error, "Local only is on")
  expect_length(fake$log$messages, 0L)
  row <- DBI::dbGetQuery(con, "SELECT status, error_kind FROM llm_call ORDER BY id DESC LIMIT 1")
  expect_identical(row$status, "blocked")
  expect_identical(row$error_kind, "local_only")

  set_setting(con, "local_only", "0")
  set_setting(con, "spend_cap_usd", "0")
  expect_identical(run_coach(con, GOOD, "req_deep_check")$kind, "spend_cap")
})

test_that("a failed route falls back, and failures are never cached", {
  con <- local_sample()
  fake <- fake_client(list(simpleError("HTTP 500"), good_coach_answer()))
  local_mocked_bindings(build_llm_client = fake$builder)
  result <- run_coach(con, GOOD, "req_deep_check")
  expect_true(result$ok)
  expect_identical(result$provider_id, "openai")
  expect_identical(DBI::dbGetQuery(con, "SELECT status FROM llm_call ORDER BY id")$status[-1], c("error", "ok"))

  con2 <- local_sample()
  set_light_model(con2)
  fake2 <- fake_client(list(simpleError("Failed to perform HTTP request")))
  local_mocked_bindings(build_llm_client = fake2$builder)
  failed <- run_coach(con2, GOOD)
  expect_false(failed$ok)
  expect_identical(failed$kind, "local_down")
  expect_match(failed$error, "LM Studio isn't running at http://localhost:1234/v1")
  expect_null(coach_from_cache(con2, GOOD))
})

test_that("reviews are cached by exact text", {
  con <- local_sample()
  set_light_model(con)
  fake <- fake_client(list(good_coach_answer()))
  local_mocked_bindings(build_llm_client = fake$builder)
  expect_null(coach_from_cache(con, GOOD))
  first <- run_coach(con, GOOD)
  hit <- coach_from_cache(con, GOOD)
  expect_true(hit$cached)
  expect_identical(hit$call_id, first$call_id)
  expect_identical(hit$answer$questions[[1]]$text, "Is the bus monitored?")
  expect_null(coach_from_cache(con, paste(GOOD, "x")))
  expect_length(fake$log$messages, 1L)
})

test_that("an answer that doesn't fit the schema is refused and logged", {
  con <- local_sample()
  set_light_model(con)
  bad <- good_coach_answer()
  bad$criteria$singular <- "Good"
  local_mocked_bindings(build_llm_client = fake_client(list(bad))$builder)
  result <- run_coach(con, GOOD)
  expect_false(result$ok)
  expect_match(result$error, "expected shape")
  expect_null(coach_from_cache(con, GOOD))
})

test_that("redaction hides terms from cloud calls and maps them back, but not locally", {
  con <- local_sample()
  set_setting(con, "scope_text", "Built by Acme Motors for survey work.")
  set_redact_terms(con, "Acme Motors\nAcme")
  answer <- good_coach_answer(questions = list(list(text = "Does [REDACTED-1] supply it?", context = "")))
  fake <- fake_client(list(answer))
  local_mocked_bindings(build_llm_client = fake$builder)
  result <- run_coach(con, GOOD, "req_deep_check")
  expect_false(grepl("Acme", fake$log$messages[1], fixed = TRUE))
  expect_match(fake$log$messages[1], "[REDACTED-1]", fixed = TRUE)
  expect_identical(result$answer$questions[[1]]$text, "Does Acme Motors supply it?")
  stored <- DBI::dbGetQuery(con, "SELECT user_message, redactions_json FROM llm_call ORDER BY id DESC LIMIT 1")
  expect_false(grepl("Acme", stored$user_message, fixed = TRUE))
  expect_match(stored$redactions_json, "REDACTED-1", fixed = TRUE)

  set_light_model(con)
  fake2 <- fake_client(list(good_coach_answer()))
  local_mocked_bindings(build_llm_client = fake2$builder)
  run_coach(con, GOOD)
  expect_match(fake2$log$messages[1], "Acme Motors", fixed = TRUE)
})

test_that("usage and cost are logged, and code computes the cost", {
  con <- local_sample()
  local_mocked_bindings(build_llm_client = fake_client(list(good_coach_answer()))$builder)
  run_coach(con, GOOD, "req_deep_check")
  row <- DBI::dbGetQuery(con, "SELECT input_tokens, output_tokens, cost_usd, model, snapshot_hash FROM llm_call ORDER BY id DESC LIMIT 1")
  expect_equal(row$input_tokens, 1000)
  expect_equal(row$cost_usd, (1000 * 2 + 100 * 10) / 1e6)
  expect_identical(row$snapshot_hash, model_hash(con))
  # The sample already holds one call.
  expect_identical(ai_usage_today(con)$calls, 2L)
  provider <- provider_row(con, "lmstudio")
  expect_identical(call_cost(provider, "x", 100, 100), 0)
})

test_that("errors read in plain words", {
  local <- data.frame(id = "lmstudio", label = "LM Studio", is_local = 1L, base_url = "http://localhost:1234/v1")
  cloud <- data.frame(id = "anthropic", label = "Anthropic", is_local = 0L, base_url = NA)
  expect_identical(classify_llm_error("Connection refused", local)$kind, "local_down")
  expect_identical(classify_llm_error("model x not found", local, "x")$kind, "model_not_loaded")
  expect_identical(classify_llm_error("HTTP 401", cloud)$kind, "auth")
  expect_identical(classify_llm_error("HTTP 404", cloud, "m")$kind, "not_found")
  expect_identical(classify_llm_error("HTTP 429", cloud)$kind, "rate_limit")
  expect_identical(classify_llm_error("SSL certificate problem", cloud)$kind, "tls")
  expect_identical(classify_llm_error("Timeout was reached", cloud)$kind, "timeout")
  expect_identical(classify_llm_error("Can't find env var", cloud)$kind, "no_key")
  expect_identical(classify_llm_error("weird", cloud)$kind, "other")
})

test_that("the async path logs the same way, and a throw can't crash", {
  con <- local_sample()
  set_light_model(con)
  local_mocked_bindings(build_llm_client = fake_client(list(good_coach_answer()))$builder)
  result <- settle(run_coach_async(con, GOOD))
  expect_true(result$ok)
  expect_identical(result$text, GOOD)
  expect_identical(result$answer$criteria$singular, "pass")

  local_mocked_bindings(build_llm_client = function(...) stop("Connection refused"))
  failed <- settle(run_coach_async(con, paste(GOOD, "Again")))
  expect_false(failed$ok)
  expect_identical(failed$kind, "local_down")

  local_mocked_bindings(coach_inputs = function(...) stop("boom"))
  thrown <- settle(run_coach_async(con, GOOD))
  expect_false(thrown$ok)
  expect_match(thrown$error, "boom")
})

test_that("a result arriving after the project closed doesn't throw", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- init_project_db(path)
  set_light_model(con)
  local_mocked_bindings(build_llm_client = fake_client(list(good_coach_answer()))$builder)
  promise <- run_coach_async(con, GOOD)
  DBI::dbDisconnect(con)
  expect_false(settle(promise)$ok)
})

test_that("numbers are scrubbed unless they're names from the model", {
  out <- scrub_numbers("A 5000 mAh pack on PowerBus28V with 3 cells, version [value].", "PowerBus28V")
  expect_identical(out$text, "A [value] mAh pack on PowerBus28V with [value] cells, version [value].")
  expect_identical(out$count, 2L)
  expect_identical(scrub_numbers("No digits", character(0))$count, 0L)
  tree <- scrub_tree(list(name = "Unit 2", ports = list(list(name = "p1", interface = "PowerBus28V"))), "PowerBus28V")
  expect_identical(tree$value$name, "Unit [value]")
  expect_identical(tree$value$ports[[1]]$interface, "PowerBus28V")
  expect_identical(tree$count, 2L)
})

test_that("the word diff reconstructs both texts", {
  old <- "the power system shall report time"
  new <- "the power system shall report remaining energy"
  diff <- word_diff(old, new)
  expect_identical(paste(diff$word[diff$op != "ins"], collapse = " "), old)
  expect_identical(paste(diff$word[diff$op != "del"], collapse = " "), new)
  expect_identical(diff$word[diff$op == "del"], "time")
})

test_that("an example rewrite is re-gated, numbers scrubbed, and never repaired", {
  draft <- "When the bus drops, the power system shall report status within 5 s."
  ok <- check_example_rewrite(draft, "When the bus drops, the power system shall report status within 2 s.")
  expect_identical(ok$status, "ok")
  expect_match(ok$text, "within [value] s.", fixed = TRUE)
  expect_identical(ok$invented, 1L)
  kept <- check_example_rewrite(draft, "When the bus falls, the power system shall report status within 5 s.")
  expect_match(kept$text, "within 5 s.", fixed = TRUE)
  expect_identical(kept$invented, 0L)
  failed <- check_example_rewrite(draft, "The system should be robust.")
  expect_identical(failed$status, "failed")
  expect_true(any(grepl("robust", failed$reasons)))
  expect_identical(check_example_rewrite(draft, " ")$status, "none")
})

test_that("a conflict is recorded only for a verbatim quote", {
  con <- local_sample()
  quoted <- "The autopilot software shall log all flight-critical parameters at 50 Hz."
  value <- good_coach_answer(conflicts = list(list(quoted_text = quoted, explanation = "Same rate"), list(quoted_text = "Logs at about 50 Hz", explanation = "Paraphrase")))
  answer <- process_coach_answer(con, GOOD, value)
  expect_identical(answer$conflicts[[1]]$requirement_id, "SYS-014")
  expect_true(is.na(answer$conflicts[[2]]$requirement_id))
  record_conflicts(con, "SYS-001", answer, NA_integer_)
  rows <- DBI::dbGetQuery(con, "SELECT requirement_a, requirement_b FROM requirement_conflict")
  expect_identical(nrow(rows), 1L)
  expect_identical(rows$requirement_b, "SYS-014")
})

test_that("commit with a review checks the assessed text in core", {
  con <- local_sample()
  expect_error(commit_requirement(con, GOOD, coach_state = "reviewed", assessed_text = "older words"), class = "modelbench_refused")
  written <- commit_requirement(con, GOOD, coach_state = "reviewed", assessed_text = GOOD)
  expect_identical(read_entity(con, "requirement", written$id)$coach_state, "reviewed")
})

test_that("a coach that can't run doesn't lock out commit", {
  gate <- check_requirement(GOOD)
  expect_false(commit_state(gate, GOOD, TRUE)$enabled)
  state <- commit_state(gate, GOOD, TRUE, coach_failed_text = GOOD)
  expect_true(state$enabled)
  expect_match(state$reason, "uncoached")
  expect_false(commit_state(gate, GOOD, TRUE, coach_failed_text = "older")$enabled)
  con <- local_sample()
  expect_false(coach_configured(con))
  set_light_model(con)
  expect_true(coach_configured(con))
  set_task_route(con, "req_light_check", list(trigger_mode = "on_demand"))
  expect_false(coach_configured(con))
})

test_that("gap answers map handles, drop what doesn't resolve, and block invented numbers", {
  con <- local_sample()
  prompt <- gap_prompt(con)
  handle <- function(name) prompt$candidates$n[prompt$candidates$name == name]
  items <- list(
    gap_item(title = "What keeps the battery cool?", target = handle("Power System")),
    gap_item(title = "Dropped target?", target = "C999"),
    gap_item("sketch_element", "What brings it down?", name = "Termination Unit 2", stereotype = "HW",
             ports = list(list(name = "pwr", direction = "in", interface = handle("PowerBus28V")),
                          list(name = "bad", direction = "in", interface = "C999"))),
    gap_item("sketch_requirement", "What about temperature?", draft = "The power system shall hold cells below 40 °C.")
  )
  ids <- store_gap_answer(con, prompt, list(value = list(items = items), call_id = NA_integer_))
  rows <- DBI::dbGetQuery(con, sprintf("SELECT * FROM ai_proposal WHERE id IN ('%s') ORDER BY id", paste(ids, collapse = "','")))
  expect_identical(rows$target_id[1], "C-003")
  expect_true(is.na(rows$target_id[2]))
  sketch <- jsonlite::fromJSON(rows$payload_json[3], simplifyVector = FALSE)
  expect_identical(sketch$name, "Termination Unit [value]")
  expect_length(sketch$ports, 1L)
  expect_identical(sketch$ports[[1]]$interface, "PowerBus28V")
  expect_identical(sketch$`_invented`, 1L)
  draft <- jsonlite::fromJSON(rows$payload_json[4], simplifyVector = FALSE)
  expect_match(draft$draft, "below [value] °C.", fixed = TRUE)
  expect_identical(model_health(con)$ai$invented, 2L)
  dropped <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM audit_log WHERE action = 'ai_dropped'")$n
  expect_identical(dropped, 2L)
})

test_that("a title that isn't a question is asked for again, then stored with a warning", {
  con <- local_sample()
  set_task_route(con, "gap_inference", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  bad <- list(items = list(gap_item(title = "Add a unit.")))
  good <- list(items = list(gap_item(title = "Is a unit needed?")))
  fake <- fake_client(list(bad, good))
  local_mocked_bindings(build_llm_client = fake$builder)
  result <- run_gaps(con)
  expect_length(fake$log$messages, 2L)
  expect_identical(DBI::dbGetQuery(con, "SELECT title FROM ai_proposal WHERE id = ?", params = list(result$proposal_ids))$title, "Is a unit needed?")

  fake2 <- fake_client(list(bad, bad))
  local_mocked_bindings(build_llm_client = fake2$builder)
  again <- settle(run_gaps_async(con))
  row <- DBI::dbGetQuery(con, "SELECT payload_json FROM ai_proposal WHERE id = ?", params = list(again$proposal_ids))
  expect_match(row$payload_json, "isn't phrased as a question", fixed = TRUE)
})

test_that("adding a sketch needs a note, and creates an unreviewed draft with placeholders", {
  con <- local_built_sample()
  id <- insert_proposal(con, list(task = "gap_inference", proposal_kind = "sketch_element", title = "What?", payload_json = canonical_json(list(
    name = "Watchdog", stereotype = "HW", ports = list(list(name = "pwr", direction = "in", interface = "PowerBus28V"))))))
  expect_error(keep_sketch(con, id, " "), class = "modelbench_refused")
  before <- count_rows(con, "v_unreviewed")
  placeholders <- count_rows(con, "v_placeholders")
  written <- keep_sketch(con, id, "Independent path needed")
  row <- read_entity(con, "element", written$id)
  expect_identical(row$origin, "ai")
  expect_identical(row$review_state, "unreviewed")
  expect_identical(row$parent_id, "C-000")
  expect_identical(count_rows(con, "v_unreviewed"), before + 1L)
  expect_identical(count_rows(con, "v_placeholders"), placeholders + 1L)
  proposal <- DBI::dbGetQuery(con, "SELECT status, engineer_note, created_entity_id FROM ai_proposal WHERE id = ?", params = list(id))
  expect_identical(proposal$status, "kept")
  expect_identical(proposal$created_entity_id, written$id)
  expect_error(keep_sketch(con, id, "again"), class = "modelbench_refused")
  expect_identical(DBI::dbGetQuery(con, "SELECT interface_id FROM port WHERE element_id = ?", params = list(written$id))$interface_id, "IF-001")
})

test_that("dismissing needs a reason, and a dismissed gap stays dismissed", {
  con <- local_sample()
  expect_error(dismiss_proposal(con, "PR-0002", " "), class = "modelbench_refused")
  refresh_structural_gaps(con)
  open_gaps <- proposal_list(con, "open", "structural")
  expect_true("PR-0004" %in% open_gaps$id)
  count <- nrow(open_gaps)
  refresh_structural_gaps(con)
  expect_identical(nrow(proposal_list(con, "open", "structural")), count)
  dismiss_proposal(con, "PR-0004", "Trigger comes in a later design")
  refresh_structural_gaps(con)
  expect_false("PR-0004" %in% proposal_list(con, "open", "structural")$id)
  expect_identical(nrow(proposal_list(con, "open", "structural")), count - 1L)
})

test_that("a structural gap that's been fixed closes itself", {
  con <- local_sample()
  refresh_structural_gaps(con)
  write_port(con, "P-T", list(element_id = "C-004", name = "t", direction = "out", interface_id = "IF-010"))
  write_connector(con, fields = list(source_port_id = "P-T", target_port_id = "P-0019"))
  refresh_structural_gaps(con)
  expect_identical(DBI::dbGetQuery(con, "SELECT status FROM ai_proposal WHERE id = 'PR-0004'")$status, "superseded")
})

test_that("starting from a sketch hands over the draft, and a link needs its note", {
  con <- local_sample()
  id <- insert_proposal(con, list(task = "scope_gaps", proposal_kind = "sketch_requirement", title = "T?", payload_json = canonical_json(list(draft = "The UAV shall fly."))))
  expect_identical(start_from_sketch(con, id), "The UAV shall fly.")
  # It stays open until the draft is saved, so leaving Write doesn't lose it.
  expect_identical(DBI::dbGetQuery(con, "SELECT status FROM ai_proposal WHERE id = ?", params = list(id))$status, "open")
  keep_started_sketch(con, id, "SYS-003")
  expect_identical(DBI::dbGetQuery(con, "SELECT status, created_entity_id FROM ai_proposal WHERE id = ?", params = list(id))$created_entity_id, "SYS-003")

  link <- insert_proposal(con, list(task = "trace_coach", proposal_kind = "candidate_link", title = "Which?", payload_json = "{}",
                                    candidates_json = canonical_json(list(list(n = "C1", kind = "element", id = "C-004", name = "FC")))))
  expect_error(link_from_candidate(con, link, "C1", "satisfied_by", "requirement", "SYS-021", " "), class = "modelbench_refused")
  expect_error(link_from_candidate(con, link, "C9", "satisfied_by", "requirement", "SYS-021", "why"), class = "modelbench_refused")
  written <- link_from_candidate(con, link, "C1", "satisfied_by", "requirement", "SYS-021", "It holds the fence")
  row <- read_entity(con, "link", written$id)
  expect_identical(row$origin, "ai_candidate")
  expect_identical(row$target_id, "C-004")
  expect_identical(row$ai_proposal_id, link)
})

test_that("no key means the key check says so, and the keyring stays out of tests", {
  expect_false(provider_has_key("anthropic"))
  withr::local_envvar(ANTHROPIC_API_KEY = "test-key")
  expect_true(provider_has_key("anthropic"))
  expect_false(provider_has_key("lmstudio"))
})

test_that("the seeds run twice without changing a chosen route", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- init_project_db(path)
  set_light_model(con, "chosen")
  DBI::dbDisconnect(con)
  con <- init_project_db(path)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(DBI::dbGetQuery(con, "SELECT model FROM task_route WHERE task = 'req_light_check'")$model, "chosen")
  expect_identical(nrow(routes_table(con)), 12L)
})

test_that("a connection test reports in plain words", {
  con <- local_sample()
  local_mocked_bindings(build_llm_client = function(provider, model, system_prompt) list(chat_async = function(prompt) promises::promise_resolve("OK")))
  expect_true(settle(test_provider_async(con, "anthropic"))$ok)
  local_mocked_bindings(build_llm_client = function(provider, model, system_prompt) list(chat_async = function(prompt) promises::promise_reject(simpleError("HTTP 401"))))
  refused <- settle(test_provider_async(con, "anthropic"))
  expect_false(refused$ok)
  expect_match(refused$text, "refused the API key")
  local_mocked_bindings(build_llm_client = function(...) stop("Can't find env var"))
  expect_identical(settle(test_provider_async(con, "openai"))$kind, "no_key")
})
