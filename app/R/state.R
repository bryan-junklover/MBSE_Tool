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
    selected_failure = shiny::reactiveVal(NULL),
    commands = shiny::reactiveVal(new_command_stack()),
    open_in_write = shiny::reactiveVal(NULL),
    behavior_focus = shiny::reactiveVal(NULL),
    # Where a jump came from, so the target screen can offer the way back (UX review §2.26).
    return_to = shiny::reactiveVal(NULL),
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

# A download whose refusal is a toast, like any refused edit (handoff §5.13).
export_handler <- function(con, name, extension, writer) {
  shiny::downloadHandler(
    filename = function() sprintf("%s_%s.%s", gsub("[^A-Za-z0-9]+", "_", get_setting(con(), "project_name", "project")), name, extension),
    content = export_content(con, writer)
  )
}

export_content <- function(con, writer) {
  function(file) {
    outcome <- tryCatch(writer(con(), file), error = function(e) e)
    if (inherits(outcome, "error")) {
      refusal_toast(conditionMessage(outcome))
      stop(conditionMessage(outcome))
    }
  }
}

export_button <- function(id, label) shiny::downloadButton(id, label, class = "mb-btn mb-btn-small", icon = NULL)

# An input that reports its value when the field is left, not on every keystroke.
change_js <- function(input_id, fields) {
  sprintf("Shiny.setInputValue('%s', Object.assign(%s, {value: this.value, n: Math.random()}), {priority: 'event'})",
          input_id, if (length(fields) == 0L) "{}" else jsonlite::toJSON(fields, auto_unbox = TRUE))
}

# Undo and redo, from a button or a keyboard shortcut on a diagram.
run_command <- function(state, action, redraw = NULL) {
  con <- shiny::isolate(state$con())
  stack <- guarded_edit(action(con, shiny::isolate(state$commands())))
  if (is.null(stack)) return(invisible(NULL))
  state$commands(stack)
  invalidate_db(state, c("model", "spec", "behavior"))
  if (!is.null(redraw)) redraw(shiny::isolate(redraw()) + 1L)
  invisible(stack)
}

# The screen bar: the workspace's tabs, the screen's own controls, then its actions on the
# right (UX review §4.5). One bar per screen, so the chrome takes a single row.
screen_bar <- function(tabs = NULL, ..., actions = NULL) {
  shiny::tags$div(class = "mb-tabbar", tabs, ..., if (!is.null(actions)) shiny::tags$div(class = "mb-actions", actions))
}

# A side column that can be resized and hidden (UX review §1.4). `key` names where its
# width is kept; `auto` columns start hidden and open when something is selected.
side_column <- function(side, key, label, ..., width = NULL, auto = FALSE, closed = FALSE, class = NULL, style = NULL) {
  shiny::tags$aside(class = paste(if (side == "left") "mb-left" else "mb-right", class), style = style,
                    `data-col` = key, `data-label` = label, `data-width` = width,
                    `data-auto` = if (auto) "true", `data-closed` = if (closed) "true", ...)
}

# Opens (or, for auto columns, closes) the side column next to an element on this screen.
reveal_column <- function(session, anchor_id, open = TRUE, side = "right") {
  session$sendCustomMessage("mb_column", list(anchor = session$ns(anchor_id), side = side, open = open))
}

# A delete or remove control that asks first (UX review §4.2). The question names what goes.
confirm_button <- function(label, input_id, value, title, body, confirm = sub("…$", "", label), class = "mb-btn") {
  shiny::tags$button(type = "button", class = class, onclick = "mbConfirm(this)", `data-input` = input_id,
                     `data-value` = as.character(jsonlite::toJSON(value, auto_unbox = TRUE)),
                     `data-title` = title, `data-body` = body, `data-confirm` = confirm, label)
}

# Ends an inspector with its destructive actions, set apart by a rule.
danger_zone <- function(...) shiny::tags$div(class = "mb-danger-zone", ...)

# Names an unlabelled control inside a Shiny input, for screen readers.
aria_label <- function(tag, element, label) {
  htmltools::tagQuery(tag)$find(element)$addAttrs(`aria-label` = label)$allTags()
}

# A delete's consequences in words, from delete_effects(): what goes with it, and what stays
# but loses its link. A refusal says why instead, and the button is still offered so the
# reason shows on the toast.
DELETE_LABELS <- c(element = "part", port = "port", connector = "connector", seq_lifeline = "scenario lifeline", sm_transition = "transition",
                   seq_step = "scenario step", state_machine = "state machine", budget = "budget", failure_mode = "failure mode",
                   failure_waiver = "set-aside check", link = "link", property_value = "property value", interface_field = "field",
                   analysis_limit = "analysis limit")
CLEAR_LABELS <- c(requirement = "requirement keeps its text but no longer has it as its actor", act_node = "activity step loses its performer",
                  port = "port loses its interface", seq_step = "scenario step loses its interface", sm_event = "event loses its interface",
                  budget = "budget no longer names the requirement", sm_assertion = "assertion no longer names the requirement")
delete_effects_text <- function(effects) {
  if (!is.null(effects$refusal)) return(paste(effects$refusal, "Nothing will be deleted."))
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1L) "" else "s")
  removed <- effects$removed[names(effects$removed) %in% names(DELETE_LABELS)]
  goes <- vapply(names(removed), function(kind) plural(removed[[kind]], DELETE_LABELS[[kind]]), "")
  cleared <- effects$cleared[names(effects$cleared) %in% names(CLEAR_LABELS)]
  keeps <- vapply(names(cleared), function(kind) sprintf("%d %s", cleared[[kind]], CLEAR_LABELS[[kind]]), "")
  paste(c(if (length(goes) > 0L) sprintf("It goes with its %s.", paste(goes, collapse = ", ")) else "Nothing else goes with it.",
          if (length(keeps) > 0L) sprintf("Kept, but unlinked: %s.", paste(keeps, collapse = "; ")),
          "Undo brings it all back."), collapse = " ")
}
