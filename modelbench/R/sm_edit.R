# Editing a state machine after it's made: events, variables and the machine itself.
# Assertions name events, and guards and effects name variables, so a change that would
# break one of those is refused in plain words rather than made silently.

assertions_naming <- function(con, sm_id, name) {
  rows <- DBI::dbGetQuery(con, "SELECT id, script_json FROM sm_assertion WHERE sm_id = ?", params = list(sm_id))
  rows$id[vapply(rows$script_json, function(json) grepl(sprintf("\"%s\"", name), json, fixed = TRUE), TRUE, USE.NAMES = FALSE)]
}

expressions_naming <- function(con, sm_id, name) {
  pattern <- sprintf("(^|[^A-Za-z0-9_])%s([^A-Za-z0-9_]|$)", name)
  transitions <- DBI::dbGetQuery(con, "SELECT id, guard_expr, effect_expr FROM sm_transition WHERE sm_id = ?", params = list(sm_id))
  states <- DBI::dbGetQuery(con, "SELECT id, entry_effect FROM sm_state WHERE sm_id = ?", params = list(sm_id))
  hit <- function(x) !is.na(x) & grepl(pattern, x, perl = TRUE)
  c(transitions$id[hit(transitions$guard_expr) | hit(transitions$effect_expr)], states$id[hit(states$entry_effect)])
}

rename_state_machine <- function(con, sm_id, name, author = "engineer") {
  name <- trimws(name)
  if (!nzchar(name)) refuse("A state machine needs a name.")
  write_state_machine(con, sm_id, list(name = name), author, "Renamed")
}

rename_sm_event <- function(con, event_id, name, author = "engineer") {
  event <- read_entity(con, "sm_event", event_id)
  if (is.null(event)) refuse("That event no longer exists.")
  name <- trimws(name)
  if (!grepl("^[a-z][a-z0-9_]*$", name)) refuse("An event name is snake_case, like gnss_lost.")
  if (identical(name, event$name)) return(NULL)
  if (name %in% DBI::dbGetQuery(con, "SELECT name FROM sm_event WHERE sm_id = ?", params = list(event$sm_id))$name) refuse("There's already an event called ", name, ".")
  used <- assertions_naming(con, event$sm_id, event$name)
  if (length(used) > 0L) refuse("Assertion ", used[1], " fires ", event$name, ", so renaming it would break the assertion. Change the assertion first.")
  write_sm_event(con, event_id, list(name = name), author, "Renamed")
}

# Ties an event to the interface field that raises it, or unties it with a blank interface.
set_event_binding <- function(con, event_id, interface_id, field_name = NULL, author = "engineer") {
  blank <- function(x) is.null(x) || is.na(x) || !nzchar(x)
  if (blank(interface_id)) return(write_sm_event(con, event_id, list(interface_id = NA_character_, field_name = NA_character_), author, "Unbound"))
  if (is.null(read_entity(con, "interface", interface_id))) refuse("That interface no longer exists.")
  if (!blank(field_name) && !field_name %in% interface_fields(con, interface_id)$name) refuse("That interface has no field ", field_name, ".")
  write_sm_event(con, event_id, list(interface_id = interface_id, field_name = if (blank(field_name)) NA_character_ else field_name), author, "Bound to interface")
}

delete_sm_event <- function(con, event_id, author = "engineer") {
  event <- read_entity(con, "sm_event", event_id)
  if (is.null(event)) refuse("That event no longer exists.")
  used <- assertions_naming(con, event$sm_id, event$name)
  if (length(used) > 0L) refuse("Assertion ", used[1], " fires ", event$name, ". Delete or change the assertion first.")
  delete_entity(con, "sm_event", event_id, author)
}

# A variable's initial value and unit. Its name is used in guards and effects, so it isn't
# renamed here.
update_sm_variable <- function(con, variable_id, field, value, author = "engineer") {
  variable <- read_entity(con, "sm_variable", variable_id)
  if (is.null(variable)) refuse("That variable no longer exists.")
  value <- trimws(value)
  if (field == "initial_value") {
    ok <- switch(variable$value_type, number = !is.na(suppressWarnings(as.numeric(value))), bool = value %in% c("true", "false"), TRUE)
    if (!ok) refuse(variable$name, " is a ", variable$value_type, ", so its initial value has to be ", if (variable$value_type == "bool") "true or false." else "a number.")
    return(write_sm_variable(con, variable_id, list(initial_value = value), author, "Initial value changed"))
  }
  if (field == "unit") {
    if (nzchar(value) && !value %in% unit_list(con)$symbol) refuse(value, " isn't in the unit registry.")
    return(write_sm_variable(con, variable_id, list(unit = if (nzchar(value)) value else NA_character_), author, "Unit changed"))
  }
  refuse("A variable's ", field, " can't be changed here.")
}

delete_sm_variable <- function(con, variable_id, author = "engineer") {
  variable <- read_entity(con, "sm_variable", variable_id)
  if (is.null(variable)) refuse("That variable no longer exists.")
  used <- c(expressions_naming(con, variable$sm_id, variable$name), assertions_naming(con, variable$sm_id, variable$name))
  if (length(used) > 0L) refuse(used[1], " uses ", variable$name, ". Change it first.")
  delete_entity(con, "sm_variable", variable_id, author)
}

# A new event must be handled, or knowingly ignored, in every state that isn't final, so
# Completeness checks it from the start. Untick Must handle where it doesn't apply.
add_sm_event <- function(con, sm_id, name, author = "engineer") {
  name <- trimws(name)
  if (!grepl("^[a-z][a-z0-9_]*$", name)) refuse("An event name is snake_case, like gnss_lost.")
  if (name %in% DBI::dbGetQuery(con, "SELECT name FROM sm_event WHERE sm_id = ?", params = list(sm_id))$name) refuse("There's already an event called ", name, ".")
  states <- DBI::dbGetQuery(con, "SELECT id FROM sm_state WHERE sm_id = ? AND kind <> 'final'", params = list(sm_id))$id
  with_change_set(con, function(change_set) {
    event <- write_sm_event(con, fields = list(sm_id = sm_id, name = name), author = author, note = "Added event", change_set = change_set)
    for (state in states) write_sm_event_scope(con, paste(event$id, state, sep = "|"), list(), author, "In scope", change_set)
    event
  })
}

# A new state must handle, or knowingly ignore, every event the machine has.
add_sm_state <- function(con, sm_id, name, author = "engineer") {
  events <- DBI::dbGetQuery(con, "SELECT id FROM sm_event WHERE sm_id = ?", params = list(sm_id))$id
  with_change_set(con, function(change_set) {
    state <- save_state(con, fields = list(sm_id = sm_id, name = name), note = "Added state", change_set = change_set, author = author)
    for (event in events) write_sm_event_scope(con, paste(event, state$id, sep = "|"), list(), author, "In scope", change_set)
    state
  })
}

# Events no state has to handle: Completeness can't check them.
unscoped_events <- function(con, sm_id) {
  DBI::dbGetQuery(con, "SELECT e.id, e.name FROM sm_event e WHERE e.sm_id = ?
                          AND NOT EXISTS (SELECT 1 FROM sm_event_scope s WHERE s.event_id = e.id) ORDER BY e.name", params = list(sm_id))
}

# An assertion's description or the requirement it verifies. The requirement lives in the
# assertion and in its verified_by link, so both change together (review B5).
update_assertion <- function(con, assertion_id, field, value, author = "engineer") {
  row <- read_entity(con, "sm_assertion", assertion_id)
  if (is.null(row)) refuse("That assertion no longer exists.")
  value <- trimws(if (is.null(value)) "" else value)
  if (field == "description") {
    if (!nzchar(value)) refuse("An assertion needs a description.")
    return(write_sm_assertion(con, assertion_id, list(description = value), author, "Assertion edited"))
  }
  if (field != "requirement_id") refuse("An assertion's ", field, " can't be changed here. Delete it and add it again.")
  if (nzchar(value) && is.null(read_entity(con, "requirement", value))) refuse("That requirement isn't in the model.")
  with_change_set(con, function(change_set) {
    old <- DBI::dbGetQuery(con, "SELECT id FROM link WHERE link_type = 'verified_by' AND target_kind = 'sm_assertion' AND target_id = ?", params = list(assertion_id))$id
    for (id in old) delete_entity(con, "link", id, author, "Assertion now verifies another requirement", change_set)
    written <- write_sm_assertion(con, assertion_id, list(requirement_id = if (nzchar(value)) value else NA_character_), author, "Requirement changed", change_set)
    if (nzchar(value)) {
      write_link(con, fields = list(link_type = "verified_by", source_kind = "requirement", source_id = value, target_kind = "sm_assertion", target_id = assertion_id),
                 author = author, note = "Assertion verifies requirement", change_set = change_set)
    }
    written
  })
}
