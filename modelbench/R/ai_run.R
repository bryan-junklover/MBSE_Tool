# Runs one prompt through its route: safeguards, redaction, fallback, logging. Every
# call writes an llm_call row, including blocked ones, and failures come back as data.

cache_key <- function(prompt) sha256(paste(prompt$prompt_version, prompt$cache_text, sep = "\n"))

ai_cache_lookup <- function(con, prompt) {
  row <- DBI::dbGetQuery(con, "
    SELECT c.id, c.response_json, c.provider_id, c.model, c.prompt_version, c.duration_ms, c.snapshot_hash
      FROM llm_cache k JOIN llm_call c ON c.id = k.llm_call_id WHERE k.task = ? AND k.key_hash = ?",
    params = list(prompt$task, cache_key(prompt)))
  if (nrow(row) == 0L) return(NULL)
  ai_result(TRUE, jsonlite::fromJSON(row$response_json, simplifyVector = FALSE), call_id = row$id, provider_id = row$provider_id,
            model = row$model, prompt = prompt, duration_ms = row$duration_ms, cached = TRUE, snapshot = row$snapshot_hash)
}

ai_result <- function(ok, value = NULL, error = NULL, kind = NULL, call_id = NA_integer_, provider_id = NA_character_,
                      model = NA_character_, prompt = NULL, duration_ms = NA_real_, cached = FALSE, snapshot = NA_character_) {
  list(ok = ok, value = value, error = error, kind = kind, call_id = call_id, provider_id = provider_id, model = model,
       prompt_version = prompt$prompt_version, duration_ms = duration_ms, cached = cached, snapshot = snapshot,
       is_local = NA)
}

# What goes on the wire for an attempt: redacted only when leaving the machine.
outgoing_prompt <- function(con, prompt, attempt) {
  provider <- provider_row(con, attempt$provider_id)
  terms <- if (nrow(provider) == 1L && provider$is_local == 0L) redact_terms(con) else character(0)
  list(system = redact_text(prompt$system, terms), message = redact_text(prompt$message, terms), terms = terms, provider = provider)
}

log_attempt <- function(con, prompt, attempt, outgoing, status, snapshot, target, response = NULL, error_kind = NULL,
                        error_text = NULL, usage = list(input = NA_real_, output = NA_real_), cost = NA_real_, duration_ms = NA_real_) {
  log_llm_call(con, list(
    task = prompt$task, provider_id = attempt$provider_id, model = attempt$model, prompt_builder = prompt$prompt_builder,
    prompt_version = prompt$prompt_version, system_prompt = outgoing$system, user_message = outgoing$message,
    schema_json = schema_to_json(prompt$schema), candidates_json = prompt$candidates_json,
    response_json = if (is.null(response)) NULL else canonical_json(response), status = status, error_kind = error_kind,
    error_text = error_text, input_tokens = usage$input, output_tokens = usage$output, cost_usd = cost, duration_ms = duration_ms,
    snapshot_hash = snapshot, target_kind = target$kind, target_id = target$id,
    redactions_json = if (length(outgoing$terms) > 0L) canonical_json(as.list(sprintf("[REDACTED-%d]", seq_along(outgoing$terms)))) else NULL
  ))
}

finish_success <- function(con, prompt, attempt, outgoing, raw, client, started, snapshot, target) {
  duration <- round(as.numeric(Sys.time() - started, units = "secs") * 1000)
  value <- unredact(raw, outgoing$terms)
  usage <- client_usage(client)
  cost <- call_cost(outgoing$provider, attempt$model, usage$input, usage$output, client)
  problems <- schema_problems(value, prompt$schema)
  if (length(problems) > 0L) {
    text <- sprintf("The answer didn't fit the expected shape: %s", paste(utils::head(problems, 3L), collapse = "; "))
    id <- log_attempt(con, prompt, attempt, outgoing, "error", snapshot, target, response = value, error_kind = "other",
                      error_text = text, usage = usage, cost = cost, duration_ms = duration)
    return(ai_result(FALSE, error = text, kind = "other", call_id = id, provider_id = attempt$provider_id, model = attempt$model,
                     prompt = prompt, duration_ms = duration, snapshot = snapshot))
  }
  id <- log_attempt(con, prompt, attempt, outgoing, "ok", snapshot, target, response = value, usage = usage, cost = cost, duration_ms = duration)
  if (!is.null(prompt$cache_text)) write_llm_cache(con, prompt$task, cache_key(prompt), id)
  ai_result(TRUE, value, call_id = id, provider_id = attempt$provider_id, model = attempt$model, prompt = prompt,
            duration_ms = duration, snapshot = snapshot)
}

finish_failure <- function(con, prompt, attempt, outgoing, message, started, snapshot, target) {
  duration <- round(as.numeric(Sys.time() - started, units = "secs") * 1000)
  explained <- classify_llm_error(message, outgoing$provider, attempt$model)
  id <- log_attempt(con, prompt, attempt, outgoing, "error", snapshot, target, error_kind = explained$kind, error_text = message, duration_ms = duration)
  ai_result(FALSE, error = explained$text, kind = explained$kind, call_id = id, provider_id = attempt$provider_id,
            model = attempt$model, prompt = prompt, duration_ms = duration, snapshot = snapshot)
}

finish_blocked <- function(con, prompt, attempt, snapshot, target) {
  outgoing <- list(system = prompt$system, message = prompt$message, terms = character(0))
  id <- log_attempt(con, prompt, attempt, outgoing, "blocked", snapshot, target, error_kind = attempt$blocked, error_text = blocked_text(attempt$blocked))
  ai_result(FALSE, error = blocked_text(attempt$blocked), kind = attempt$blocked, call_id = id, provider_id = attempt$provider_id,
            model = attempt$model, prompt = prompt, snapshot = snapshot)
}

# Runs a prompt, resolving to a result that always says what happened. Sync and async
# differ only in how the request is made.
run_ai_async <- function(con, prompt, target = list(kind = NULL, id = NULL)) {
  snapshot <- model_hash(con)
  attempts <- route_attempts(con, prompt$task)
  try_next <- function(index, last = NULL) {
    if (index > length(attempts)) return(promises::promise_resolve(last))
    attempt <- attempts[[index]]
    if (!is.null(attempt$blocked)) return(promises::promise_resolve(finish_blocked(con, prompt, attempt, snapshot, target)))
    outgoing <- outgoing_prompt(con, prompt, attempt)
    started <- Sys.time()
    client <- NULL
    request <- tryCatch({
      client <- build_llm_client(outgoing$provider, attempt$model, outgoing$system)
      client$chat_structured_async(outgoing$message, type = schema_to_ellmer(prompt$schema), convert = FALSE)
    }, error = function(e) promises::promise_reject(e))
    promises::then(
      request,
      onFulfilled = function(raw) {
        if (!DBI::dbIsValid(con)) return(ai_result(FALSE, error = "The project was closed.", kind = "other", prompt = prompt))
        finish_success(con, prompt, attempt, outgoing, raw, client, started, snapshot, target)
      },
      onRejected = function(e) {
        if (!DBI::dbIsValid(con)) return(ai_result(FALSE, error = "The project was closed.", kind = "other", prompt = prompt))
        try_next(index + 1L, finish_failure(con, prompt, attempt, outgoing, conditionMessage(e), started, snapshot, target))
      }
    )
  }
  try_next(1L)
}

run_ai <- function(con, prompt, target = list(kind = NULL, id = NULL)) {
  snapshot <- model_hash(con)
  last <- NULL
  for (attempt in route_attempts(con, prompt$task)) {
    if (!is.null(attempt$blocked)) return(finish_blocked(con, prompt, attempt, snapshot, target))
    outgoing <- outgoing_prompt(con, prompt, attempt)
    started <- Sys.time()
    outcome <- tryCatch({
      client <- build_llm_client(outgoing$provider, attempt$model, outgoing$system)
      raw <- client$chat_structured(outgoing$message, type = schema_to_ellmer(prompt$schema), convert = FALSE)
      finish_success(con, prompt, attempt, outgoing, raw, client, started, snapshot, target)
    }, error = function(e) finish_failure(con, prompt, attempt, outgoing, conditionMessage(e), started, snapshot, target))
    if (isTRUE(outcome$ok)) return(outcome)
    last <- outcome
  }
  last
}

# Exactly what a call would send, for the "See the prompt" dialog.
prompt_dialog_text <- function(prompt) {
  paste0("SYSTEM\n", prompt$system, "\n\nMESSAGE\n", prompt$message, "\n\nANSWER SCHEMA\n", schema_to_json(prompt$schema))
}
