# Providers, routing and safeguards. build_llm_client() is the only place a chat client
# is created.

# Per million tokens, in US dollars. Other models fall back to ellmer's own figure.
LLM_PRICES <- list(
  "claude-opus-5-5" = c(input = 4, output = 20),
  "claude-sonnet-5-5" = c(input = 2, output = 10),
  "claude-haiku-5-5" = c(input = 0.10, output = 0.50)
)

PROVIDER_KEY_VARS <- c(anthropic = "ANTHROPIC_API_KEY", openai = "OPENAI_API_KEY")
KEYRING_SERVICE <- "modelbench"

keyring_enabled <- function() !isTRUE(getOption("modelbench.no_keyring"))

# The keyring first, then the environment. Keys are never stored in the project.
provider_key <- function(provider_id) {
  if (keyring_enabled()) {
    stored <- tryCatch(keyring::key_get(KEYRING_SERVICE, provider_id), error = function(e) "")
    if (nzchar(stored)) return(stored)
  }
  variable <- PROVIDER_KEY_VARS[provider_id]
  if (is.na(variable)) "" else Sys.getenv(variable, "")
}

provider_has_key <- function(provider_id) nzchar(provider_key(provider_id))

store_provider_key <- function(provider_id, key) {
  keyring::key_set_with_value(KEYRING_SERVICE, provider_id, password = key)
  invisible(TRUE)
}

build_llm_client <- function(provider, model, system_prompt) {
  credentials <- function() provider_key(provider$id)
  switch(provider$kind,
         anthropic = {
           # A refused request retries on the model the API picks for that kind of refusal.
           fallback <- model %in% c("claude-sonnet-5-5", "claude-opus-5-5")
           ellmer::chat_anthropic(system_prompt = system_prompt, model = model, credentials = credentials, echo = "none",
                                  api_args = if (fallback) list(fallbacks = "default") else list(),
                                  beta_headers = if (fallback) "server-side-fallback-2026-07-01" else character())
         },
         openai = ellmer::chat_openai(system_prompt = system_prompt, model = model, credentials = credentials, echo = "none"),
         openai_compatible = ellmer::chat_openai_compatible(
           base_url = provider$base_url, name = provider$label, system_prompt = system_prompt, model = model,
           credentials = function() "not-needed", echo = "none"
         ))
}

provider_row <- function(con, provider_id) {
  DBI::dbGetQuery(con, "SELECT * FROM provider WHERE id = ?", params = list(provider_id))
}

local_only <- function(con) identical(get_setting(con, "local_only", "0"), "1")

ai_usage_today <- function(con) {
  today <- format(Sys.time(), "%Y-%m-%d", tz = "UTC")
  usage <- DBI::dbGetQuery(con, "SELECT count(*) AS calls, coalesce(sum(cost_usd), 0) AS cost FROM llm_call
                                  WHERE status = 'ok' AND substr(created_at, 1, 10) = ?", params = list(today))
  list(calls = as.integer(usage$calls), cost = usage$cost)
}

spend_cap_reached <- function(con) {
  cap <- suppressWarnings(as.numeric(get_setting(con, "spend_cap_usd", "")))
  !is.na(cap) && ai_usage_today(con)$cost >= cap
}

# The local model a task can use: its own if it's routed locally, or the light check's.
local_fallback_attempt <- function(con) {
  route <- DBI::dbGetQuery(con, "SELECT r.provider_id, r.model FROM task_route r JOIN provider p ON p.id = r.provider_id
                                  WHERE p.is_local = 1 AND r.model IS NOT NULL ORDER BY r.task = 'req_light_check' DESC LIMIT 1")
  if (nrow(route) == 0L) return(NULL)
  list(provider_id = route$provider_id, model = route$model)
}

# Where a task may go, in order, with the reason for anything blocked. Each entry is
# list(provider_id, model) or, when a safeguard stops it, list(blocked = kind).
route_attempts <- function(con, task) {
  route <- DBI::dbGetQuery(con, "SELECT * FROM task_route WHERE task = ?", params = list(task))
  if (nrow(route) == 0L) stop("No route for task ", task, call. = FALSE)
  planned <- list(list(provider_id = route$provider_id, model = route$model),
                  list(provider_id = route$fallback_provider_id, model = route$fallback_model))
  planned <- Filter(function(attempt) !is.na(attempt$provider_id), planned)
  # A local provider with no model named uses whichever local model is set.
  planned <- lapply(planned, function(attempt) {
    if (!is.na(attempt$model)) return(attempt)
    local <- local_fallback_attempt(con)
    if (is.null(local) || !identical(local$provider_id, attempt$provider_id)) attempt$model <- NA_character_ else attempt$model <- local$model
    attempt
  })
  is_local <- function(attempt) provider_row(con, attempt$provider_id)$is_local == 1L
  usable <- Filter(function(attempt) !is.na(attempt$model), planned)

  if (local_only(con)) {
    local <- Filter(is_local, usable)
    if (length(local) == 0L) {
      fallback <- local_fallback_attempt(con)
      local <- if (is.null(fallback)) list() else list(fallback)
    }
    return(if (length(local) > 0L) local else list(list(blocked = "local_only", provider_id = route$provider_id, model = route$model)))
  }
  if (spend_cap_reached(con)) {
    local <- Filter(is_local, usable)
    return(if (length(local) > 0L) local else list(list(blocked = "spend_cap", provider_id = route$provider_id, model = route$model)))
  }
  if (length(usable) == 0L) return(list(list(blocked = "not_found", provider_id = route$provider_id, model = NA_character_)))
  usable
}

# --- Redaction ---

redact_terms <- function(con) {
  stored <- get_setting(con, "redact_terms", "[]")
  terms <- tryCatch(unlist(jsonlite::fromJSON(stored)), error = function(e) character(0))
  terms <- terms[nzchar(trimws(terms))]
  # Longest first, so a term inside a longer one can't break it apart.
  terms[order(-nchar(terms))]
}

redact_text <- function(text, terms) {
  for (i in seq_along(terms)) {
    text <- gsub(terms[i], sprintf("[REDACTED-%d]", i), text, fixed = TRUE)
  }
  text
}

# Puts listed terms back into everything a model returned.
unredact <- function(value, terms) {
  if (is.character(value)) {
    for (i in seq_along(terms)) value <- gsub(sprintf("[REDACTED-%d]", i), terms[i], value, fixed = TRUE)
    return(value)
  }
  if (is.list(value)) return(lapply(value, unredact, terms))
  value
}

# --- Errors in plain words ---

classify_llm_error <- function(message, provider = NULL, model = NA_character_) {
  has <- function(pattern) grepl(pattern, message, ignore.case = TRUE)
  local <- !is.null(provider) && nrow(provider) == 1L && provider$is_local == 1L
  label <- if (is.null(provider) || nrow(provider) == 0L) "The provider" else provider$label
  kind <- if (local && has("model .*(not|n't).*(found|loaded|available)|no such model|No models loaded")) "model_not_loaded"
  else if (local && has("Failed to perform|Couldn't connect|Could not connect|Connection refused|Failed to connect")) "local_down"
  else if (has("Can't find env var|no api key|API key .*missing|No key")) "no_key"
  else if (has("HTTP 401|HTTP 403|Unauthorized|Forbidden|invalid_api_key|Incorrect API key")) "auth"
  else if (has("HTTP 404|not_found")) "not_found"
  else if (has("HTTP 429|rate.?limit|overloaded")) "rate_limit"
  else if (has("certificate|SSL|TLS")) "tls"
  else if (has("timed? ?out|Timeout")) "timeout"
  else "other"
  where <- if (local) provider$base_url else ""
  list(kind = kind, text = switch(
    kind,
    local_down = sprintf("%s isn't running at %s. Start it, or switch this task to a cloud provider in Project.", label, where),
    model_not_loaded = sprintf("%s doesn't have the model \"%s\" loaded. Load it there, or pick another in Project.", label, model),
    no_key = sprintf("%s has no API key. Add one in Project, or switch this task to a local model.", label),
    auth = sprintf("%s refused the API key. Check it in Project.", label),
    not_found = sprintf("%s doesn't recognise the model \"%s\". Pick another in Project.", label, model),
    rate_limit = sprintf("%s is rate-limiting requests. Try again shortly.", label),
    tls = "A secure connection couldn't be made. Check for a proxy or certificate problem on this network.",
    timeout = sprintf("%s took too long to answer. Try again.", label),
    sprintf("%s returned an error: %s", label, message)
  ))
}

blocked_text <- function(kind) {
  switch(kind,
         local_only = "Local only is on, and this task has no local model. Pick one in Project, or turn Local only off.",
         spend_cap = "Today's cloud spend has reached the cap, so cloud calls are blocked until tomorrow. Change the cap in Project.",
         not_found = "This task has no model chosen. Pick one in Project.",
         "This call was blocked.")
}

# One small real call, for the "Connected" chip. Uses the provider's first routed model.
test_provider_async <- function(con, provider_id) {
  provider <- provider_row(con, provider_id)
  model <- DBI::dbGetQuery(con, "SELECT model FROM task_route WHERE provider_id = ? AND model IS NOT NULL ORDER BY task LIMIT 1",
                           params = list(provider_id))$model
  explain <- function(e) c(list(ok = FALSE, provider_id = provider_id), classify_llm_error(conditionMessage(e), provider, if (length(model) == 1L) model else NA_character_))
  if (length(model) != 1L) return(promises::promise_resolve(list(ok = FALSE, provider_id = provider_id, text = "No task uses this provider yet, so there's no model to test.")))
  tryCatch({
    client <- build_llm_client(provider, model, "Reply with the single word OK.")
    promises::catch(promises::then(client$chat_async("OK?"), function(answer) list(ok = TRUE, provider_id = provider_id)), explain)
  }, error = function(e) promises::promise_resolve(explain(e)))
}

# --- What the status strip and Project screen show ---

local_server_models <- function(base_url, timeout_s = 0.5) {
  tryCatch({
    response <- httr2::request(paste0(sub("/$", "", base_url), "/models")) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_perform()
    ids <- vapply(httr2::resp_body_json(response)$data, function(model) model$id, "")
    list(reachable = TRUE, models = ids)
  }, error = function(e) list(reachable = FALSE, models = character(0)))
}

coach_status <- function(con) {
  providers <- DBI::dbGetQuery(con, "SELECT * FROM provider WHERE enabled = 1")
  local <- providers[providers$is_local == 1L, , drop = FALSE]
  cloud <- providers[providers$is_local == 0L, , drop = FALSE]
  local_up <- nrow(local) > 0L && any(vapply(local$base_url, function(url) local_server_models(url)$reachable, TRUE))
  cloud_up <- !local_only(con) && any(vapply(cloud$id, provider_has_key, TRUE))
  list(local = local_up, cloud = cloud_up)
}

call_cost <- function(provider, model, input_tokens, output_tokens, client = NULL) {
  if (nrow(provider) == 1L && provider$is_local == 1L) return(0)
  price <- LLM_PRICES[[model]]
  if (!is.null(price)) return((input_tokens * price[["input"]] + output_tokens * price[["output"]]) / 1e6)
  tryCatch(as.numeric(client$get_cost()), error = function(e) NA_real_)
}

client_usage <- function(client) {
  tryCatch({
    tokens <- client$get_tokens()
    list(input = sum(tokens$input, na.rm = TRUE), output = sum(tokens$output, na.rm = TRUE))
  }, error = function(e) list(input = NA_real_, output = NA_real_))
}
