suggest_item <- function(subject, cause_kind = "degraded", field = "", title = "What if it's degraded rather than lost?",
                         cause = "Multipath near buildings", local_effect = "Position wanders", system_effect = "The survey track drifts") {
  list(subject = subject, field = field, cause_kind = cause_kind, title = title, cause = cause,
       local_effect = local_effect, system_effect = system_effect, confidence = "medium")
}

route_failure_suggest <- function(con) {
  set_task_route(con, "failure_suggest", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
}

test_that("the prompt names model content by handle only, with connections, hosts and recorded failures", {
  con <- local_built_sample()
  prompt <- failure_suggest_prompt(con)
  handle <- function(id) prompt$candidates$n[prompt$candidates$id == id]
  expect_match(prompt$message, sprintf("%s FC Hardware (hw, part of %s)", handle("C-008"), handle("C-004")), fixed = TRUE)
  expect_match(prompt$message, sprintf("- %s runs on %s", handle("C-009"), handle("C-008")), fixed = TRUE)
  expect_match(prompt$message, sprintf("- %s input from %s: lost", handle("C-004"), handle("IF-001")), fixed = TRUE)
  expect_match(prompt$message, sprintf("- %s to %s over %s", handle("C-003"), handle("C-004"), handle("IF-001")), fixed = TRUE)
  expect_no_match(prompt$message, "C-00[0-9]|IF-0|P-0|FM-0")
  expect_match(prompt$system, "Never say how the system responds", fixed = TRUE)
  expect_true("failure_suggest" %in% routes_table(con)$task)
})

test_that("suggestions are stored as proposals, minus repeats, bad handles and invented numbers", {
  con <- local_built_sample()
  route_failure_suggest(con)
  handle <- function(id) failure_suggest_prompt(con)$candidates$n[failure_suggest_prompt(con)$candidates$id == id]
  answer <- list(items = list(
    suggest_item(handle("C-001")),
    suggest_item(handle("C-003"), "over_temperature", title = "Is it hot?"),
    suggest_item("C99"),
    suggest_item(handle("IF-002"), "erroneous", field = "lat", system_effect = "The drone flies 100 m off track", title = "Wrong latitude"),
    suggest_item(handle("IF-002"), "stale", field = "nope", title = "What if the fix is stale?")
  ))
  fake <- fake_client(list(answer))
  local_mocked_bindings(build_llm_client = fake$builder)
  result <- run_failure_suggest(con)
  expect_true(result$ok)
  expect_length(result$proposal_ids, 3L)
  rows <- DBI::dbGetQuery(con, sprintf("SELECT * FROM ai_proposal WHERE id IN (%s) ORDER BY id", paste(sprintf("'%s'", result$proposal_ids), collapse = ", ")))
  expect_identical(rows$proposal_kind, rep("sketch_failure", 3L))
  expect_identical(rows$target_id, c("C-001", "IF-002", "IF-002"))
  wrong <- jsonlite::fromJSON(rows$payload_json[2])
  expect_identical(wrong$field_name, "lat")
  expect_identical(wrong$subject_label, "NavSolution.lat")
  expect_match(wrong$system_effect, "flies [value] m off track", fixed = TRUE)
  expect_identical(wrong$`_invented`, 1L)
  expect_match(wrong$`_warning`, "isn't phrased as a question", fixed = TRUE)
  expect_null(jsonlite::fromJSON(rows$payload_json[3])$field_name)
  dropped <- DBI::dbGetQuery(con, "SELECT detail_json FROM audit_log WHERE action = 'ai_dropped'")$detail_json
  expect_true(any(grepl("failure already recorded", dropped)) && any(grepl("C99", dropped)) && any(grepl("\"nope\"", dropped)))

  # Asking again doesn't repeat an open suggestion.
  local_mocked_bindings(build_llm_client = fake_client(list(list(items = list(suggest_item(handle("C-001"))))))$builder)
  expect_length(run_failure_suggest(con)$proposal_ids, 0L)
})

test_that("keeping a suggestion needs a note and makes an unreviewed failure whose response waits for review", {
  con <- local_built_sample()
  id <- insert_proposal(con, list(task = "failure_suggest", proposal_kind = "sketch_failure", target_kind = "element", target_id = "C-001",
                                  title = "Degraded?", payload_json = canonical_json(list(subject_kind = "element", subject_id = "C-001", cause_kind = "degraded",
                                                                                          cause = "Multipath", system_effect = "Track drifts"))))
  expect_error(keep_sketch(con, id, " "), "needs a note")
  before <- count_rows(con, "v_unreviewed")
  written <- keep_sketch(con, id, "We survey near buildings")
  row <- read_entity(con, "failure_mode", written$id)
  expect_identical(c(row$origin, row$review_state, row$response_state, row$cause_kind), c("ai", "unreviewed", "placeholder", "degraded"))
  expect_identical(count_rows(con, "v_unreviewed"), before + 1L)
  proposal <- DBI::dbGetQuery(con, "SELECT status, engineer_note, created_entity_id FROM ai_proposal WHERE id = ?", params = list(id))
  expect_identical(c(proposal$status, proposal$created_entity_id), c("kept", written$id))
  expect_error(keep_sketch(con, id, "again"), "already been handled")

  expect_error(update_failure_mode(con, written$id, "response_state", "unhandled"), "Review this AI suggestion first")
  expect_no_error(update_failure_mode(con, written$id, "system_effect", "Track drifts a little"))
  expect_error(review_entity(con, "failure_mode", written$id, " "), "needs a note")
  review_entity(con, "failure_mode", written$id, "Seen on the March flights")
  update_failure_mode(con, written$id, "response_state", "unhandled")
  expect_identical(read_entity(con, "failure_mode", written$id)$response_state, "unhandled")
  expect_identical(count_rows(con, "v_unreviewed"), before)
})

test_that("a suggestion about something since deleted can't be kept", {
  con <- local_built_sample()
  id <- insert_proposal(con, list(task = "failure_suggest", proposal_kind = "sketch_failure", title = "Gone?",
                                  payload_json = canonical_json(list(subject_kind = "element", subject_id = "C-999", cause_kind = "failed"))))
  expect_error(keep_sketch(con, id, "note"), "no longer in the model")
})
