# Session state shared by the modules. Nothing here is global: each session owns its
# connection and counters.

DB_AREAS <- c("spec", "model", "behavior", "ai", "baseline", "settings", "layout")

new_app_state <- function(con) {
  list(
    con = shiny::reactiveVal(con),
    revisions = do.call(shiny::reactiveValues, stats::setNames(as.list(rep(0L, length(DB_AREAS))), DB_AREAS)),
    selected_requirement = shiny::reactiveVal(NULL),
    open_in_write = shiny::reactiveVal(NULL),
    nav = shiny::reactiveVal(list(workspace = "Write", tab = "Draft"))
  )
}

db_dep <- function(state, area) {
  state$revisions[[area]]
  invisible(NULL)
}

invalidate_db <- function(state, area) {
  for (one in area) state$revisions[[one]] <- shiny::isolate(state$revisions[[one]]) + 1L
  invisible(NULL)
}

go_to <- function(state, workspace, tab = NULL) {
  state$nav(list(workspace = workspace, tab = tab))
}

# Edits refused by a gate are the one thing that gets a toast (handoff §5.13).
refusal_toast <- function(message) {
  shiny::showNotification(message, duration = 6, type = "warning")
}

# Runs a model edit. A refusal becomes a toast and returns NULL, never a crash.
guarded_edit <- function(code) {
  tryCatch(code, error = function(e) {
    refusal_toast(conditionMessage(e))
    NULL
  })
}

# Sends a click to the server as an event, so the same value can be clicked twice.
event_js <- function(input_id, value) {
  sprintf("Shiny.setInputValue(%s, %s, {priority: 'event'})",
          jsonlite::toJSON(input_id, auto_unbox = TRUE),
          jsonlite::toJSON(value, auto_unbox = TRUE))
}
