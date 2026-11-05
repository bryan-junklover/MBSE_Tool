test_that("chat tools hand back handles and plain data, never ids", {
  con <- local_built_sample()
  registry <- new_chat_registry()
  found <- chat_find(con, registry, "Power")
  expect_true(length(found$elements) > 0L)
  handle <- found$elements[[1]]$handle
  expect_match(handle, "^E[0-9]+$")
  element <- chat_get_element(con, registry, handle)
  text <- jsonlite::toJSON(element, auto_unbox = TRUE)
  ids <- DBI::dbGetQuery(con, "SELECT id FROM element")$id
  expect_false(any(vapply(ids, function(id) grepl(paste0("\"", id, "\""), text, fixed = TRUE), TRUE)))
  expect_identical(element$name, "Power System")
  expect_identical(chat_get_element(con, registry, "Power System")$handle, handle)
  expect_match(chat_get_element(con, registry, "Nothing")$error, "No element")
  expect_identical(chat_get_interface(con, registry, "NavSolution")$name, "NavSolution")
  expect_gt(chat_list_requirements(con, registry, "system")$count, 0L)
  expect_match(chat_rollup(con, registry, "mass")$total, "kg")
  expect_match(chat_rollup(con, registry, "nothing")$error, "no roll-up")
  expect_true(length(chat_analysis(con, registry, "endurance")) == 1L)
  expect_gt(chat_impact(con, registry, "Power System")$total, 0L)
})

test_that("answers show names for handles, and numbers not from tools become [value]", {
  con <- local_built_sample()
  registry <- new_chat_registry()
  handle <- chat_find(con, registry, "Power")$elements[[1]]$handle
  chat_rollup(con, registry, "mass")
  total <- chat_rollup(con, registry, "mass")$total
  shown <- chat_display_text(registry, sprintf("%s adds to a total of %s, about 99 kg more than planned.", handle, total))
  expect_match(shown, "Power System adds to a total of", fixed = TRUE)
  expect_match(shown, total, fixed = TRUE)
  expect_match(shown, "about [value] kg", fixed = TRUE)
  expect_identical(chat_referenced_elements(registry, shown), DBI::dbGetQuery(con, "SELECT id FROM element WHERE name = 'Power System'")$id)
})

test_that("the chat has no way to write: its tools are the read-only set", {
  con <- local_built_sample()
  registry <- new_chat_registry()
  tools <- chat_tools(con, registry)
  expect_setequal(vapply(tools, function(t) t@name, ""), c("get_element", "get_interface", "list_requirements", "rollup", "analysis", "impact_set", "find"))
  before <- model_hash(con)
  for (tool in tools) try(tool("Power System"), silent = TRUE)
  expect_identical(model_hash(con), before)
  expect_length(registry$calls, 7L)
  expect_identical(registry$calls[[1]]$name, "get_element")
})

test_that("attaching an answer adds to an open proposal without resolving it", {
  con <- local_built_sample()
  append_proposal_body(con, "PR-0002", "From the chat: a note")
  row <- DBI::dbGetQuery(con, "SELECT body, status, resolved_at FROM ai_proposal WHERE id = 'PR-0002'")
  expect_match(row$body, "From the chat: a note$")
  expect_identical(row$status, "open")
  expect_true(is.na(row$resolved_at))
  expect_error(append_proposal_body(con, "PR-0001", "x"), "open proposal")
})

test_that("the chat takes a question as text or as shinychat's list, and routes to a model that can answer", {
  expect_identical(chat_question_text("Hi"), "Hi")
  expect_identical(chat_question_text(list(text = "Hi", attachments = list())), "Hi")
  con <- local_built_sample()
  # A cloud model with no key isn't tried: the chat says why instead.
  expect_identical(chat_attempt(con)$blocked, "no_key")
  # No cloud key in tests, so a local route with a model wins.
  set_task_route(con, "model_chat", list(fallback_provider_id = "lmstudio", fallback_model = "mock-model"))
  expect_identical(chat_attempt(con)$provider_id, "lmstudio")
  set_setting(con, "local_only", "1")
  expect_identical(chat_attempt(con)$model, "mock-model")
  # With no local model at all, Local only blocks it and says so.
  set_task_route(con, "model_chat", list(fallback_provider_id = NA, fallback_model = NA))
  expect_identical(chat_attempt(con)$blocked, "local_only")
})
