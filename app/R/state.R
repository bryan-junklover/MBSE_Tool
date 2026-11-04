# Session state shared by the modules. Nothing here is global: each session owns its
# connection and counters.

DB_AREAS <- c("spec", "model", "behavior", "ai", "baseline", "settings", "layout")

new_app_state <- function(con) {
  list(
    con = shiny::reactiveVal(con),
    revisions = do.call(shiny::reactiveValues, stats::setNames(as.list(rep(0L, length(DB_AREAS))), DB_AREAS)),
    selected_requirement = shiny::reactiveVal(NULL),
    selected_entity = shiny::reactiveVal(list(kind = "none", ids = character(0))),
    selected_interface = shiny::reactiveVal(NULL),
    commands = shiny::reactiveVal(new_command_stack()),
    open_in_write = shiny::reactiveVal(NULL),
    behavior_focus = shiny::reactiveVal(NULL),
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

# Runs a model edit that returns a change set: a refusal is a toast, a success can be undone.
model_edit <- function(state, code, label = "edit") {
  written <- guarded_edit(code)
  if (is.null(written)) return(NULL)
  change_set <- if (is.list(written)) written$change_set else written
  state$commands(push_command(shiny::isolate(state$commands()), change_command(change_set, label)))
  invalidate_db(state, c("model", "spec"))
  written
}

# The same, for behaviour: state machines and scenarios also feed the model's counts.
behavior_edit <- function(state, code, label = "edit") {
  written <- guarded_edit(code)
  if (is.null(written)) return(NULL)
  state$commands(push_command(shiny::isolate(state$commands()), change_command(written$change_set, label)))
  invalidate_db(state, c("behavior", "model"))
  written
}

# An input that reports its value when the field is left, not on every keystroke.
change_js <- function(input_id, fields) {
  sprintf("Shiny.setInputValue('%s', Object.assign(%s, {value: this.value, n: Math.random()}), {priority: 'event'})",
          input_id, jsonlite::toJSON(fields, auto_unbox = TRUE))
}
