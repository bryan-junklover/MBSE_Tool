# Network guard: every ellmer chat and model-listing constructor throws, and the
# keys a developer's profile may load are cleared, so no test can reach a provider.
LLM_GUARD_MESSAGE <- "LLM_NETWORK_GUARD: a real LLM call was attempted during tests"

Sys.unsetenv(c("ANTHROPIC_API_KEY", "OPENAI_API_KEY", "GOOGLE_API_KEY", "GEMINI_API_KEY"))

local({
  ns <- asNamespace("ellmer")
  guarded <- grep("^(chat|models)(_|$)", ls(ns), value = TRUE)
  for (name in guarded) {
    if (!is.function(get(name, envir = ns))) next
    unlockBinding(name, ns)
    assign(name, function(...) stop(LLM_GUARD_MESSAGE, call. = FALSE), envir = ns)
    lockBinding(name, ns)
  }
})
