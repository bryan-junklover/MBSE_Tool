# The state machine simulator, assertions and completeness check (handoff §5.8). The
# simulator is pure: a state goes in, a new state comes out, and nothing is written.

load_machine <- function(con, sm_id) {
  q <- function(sql) DBI::dbGetQuery(con, sql, params = list(sm_id))
  machine <- q("SELECT * FROM state_machine WHERE id = ?")
  if (nrow(machine) == 0L) refuse("State machine ", sm_id, " doesn't exist.")
  list(
    id = sm_id, name = machine$name, initial_state_id = machine$initial_state_id,
    states = q("SELECT * FROM sm_state WHERE sm_id = ? ORDER BY id"),
    events = q("SELECT * FROM sm_event WHERE sm_id = ? ORDER BY id"),
    variables = q("SELECT * FROM sm_variable WHERE sm_id = ? ORDER BY id"),
    transitions = q("SELECT * FROM sm_transition WHERE sm_id = ? ORDER BY priority DESC, id"),
    scope = q("SELECT c.event_id, c.state_id FROM sm_event_scope c JOIN sm_event e ON e.id = c.event_id WHERE e.sm_id = ?"),
    ignores = q("SELECT i.event_id, i.state_id, i.rationale FROM sm_ignore i JOIN sm_event e ON e.id = i.event_id WHERE e.sm_id = ?"),
    property = function(element, property) find_model_property(con, element, property)
  )
}

initial_variables <- function(machine) {
  out <- list()
  for (i in seq_len(nrow(machine$variables))) {
    row <- machine$variables[i, ]
    out[[row$name]] <- switch(row$value_type, number = as.numeric(row$initial_value), bool = identical(tolower(row$initial_value), "true"), row$initial_value)
  }
  out
}

state_name <- function(machine, id) machine$states$name[match(id, machine$states$id)]

sim_start <- function(machine, start_state = NULL) {
  current <- if (is.null(start_state)) machine$initial_state_id else start_state
  if (is.na(current) || !current %in% machine$states$id) {
    return(list(current = NA_character_, clock = 0, variables = initial_variables(machine), trace = list(), status = "error", error = "This state machine has no start state."))
  }
  list(current = current, clock = 0, variables = initial_variables(machine), trace = list(), status = "ok", error = NA_character_)
}

sim_log <- function(sim, from, to, event, note, level = "normal") {
  sim$trace[[length(sim$trace) + 1L]] <- list(t = sim$clock, from = from, to = to, event = event, note = note, level = level)
  sim
}

sim_fail <- function(sim, machine, event, message) {
  sim$status <- "error"
  sim$error <- message
  sim_log(sim, state_name(machine, sim$current), NA_character_, event, message, "error")
}

# Fires one event by name, with `set` values applied before the guards (handoff §5.8).
sim_fire <- function(machine, sim, event_name, set = list()) {
  if (sim$status != "ok") return(sim)
  event <- machine$events[machine$events$name == event_name, , drop = FALSE]
  if (nrow(event) == 0L) return(sim_fail(sim, machine, event_name, sprintf("\"%s\" isn't an event of this state machine.", event_name)))
  for (name in names(set)) {
    if (!name %in% machine$variables$name) return(sim_fail(sim, machine, event_name, sprintf("\"%s\" isn't a variable.", name)))
    sim$variables[[name]] <- set[[name]]
  }
  tryCatch({
    from <- machine$transitions[machine$transitions$from_state_id == sim$current & machine$transitions$event_id == event$id, , drop = FALSE]
    true <- vapply(from$guard_expr, eval_guard, TRUE, variables = sim$variables, property = machine$property, USE.NAMES = FALSE)
    open <- from[true, , drop = FALSE]
    here <- state_name(machine, sim$current)
    if (nrow(open) == 0L) {
      in_scope <- any(machine$scope$event_id == event$id & machine$scope$state_id == sim$current)
      ignored <- any(machine$ignores$event_id == event$id & machine$ignores$state_id == sim$current)
      return(sim_log(sim, here, here, event_name, sprintf("%s ignored in %s", event_name, here), if (in_scope && !ignored) "gap" else "normal"))
    }
    top <- open[open$priority == max(open$priority), , drop = FALSE]
    if (nrow(top) > 1L) stop_sim(sprintf("%s has %d equally ranked transitions out of %s.", event_name, nrow(top), here))
    if (top$hold_state == "placeholder") stop_sim(sprintf("%s needs a value", event_name))
    if (top$hold_state == "set") sim$clock <- sim$clock + top$hold_s
    sim$variables <- apply_effect(top$effect_expr, sim$variables, machine$property)
    sim$current <- top$to_state_id
    target <- machine$states[machine$states$id == top$to_state_id, , drop = FALSE]
    sim$variables <- apply_effect(target$entry_effect, sim$variables, machine$property)
    sim_log(sim, here, target$name, event_name, sprintf("%s → %s on %s", here, target$name, event_name))
  }, modelbench_sim_error = function(e) sim_fail(sim, machine, event_name, conditionMessage(e)),
  modelbench_refused = function(e) sim_fail(sim, machine, event_name, conditionMessage(e)))
}

sim_advance <- function(machine, sim, seconds) {
  if (sim$status != "ok") return(sim)
  sim$clock <- sim$clock + seconds
  here <- state_name(machine, sim$current)
  sim_log(sim, here, here, NA_character_, sprintf("%s s pass", format(seconds, trim = TRUE)))
}

# Events with a transition out of the current state, for the dark Fire buttons.
sim_valid_events <- function(machine, sim) {
  held <- machine$transitions$event_id[machine$transitions$from_state_id == sim$current]
  machine$events$name[machine$events$id %in% held]
}

sim_step_hold <- function(machine, sim, event_name) {
  event_id <- machine$events$id[machine$events$name == event_name]
  from <- machine$transitions[machine$transitions$from_state_id == sim$current & machine$transitions$event_id == event_id, , drop = FALSE]
  from$hold_s[from$hold_state == "set"]
}

# Runs an assertion's script without writing anything. Returns result, message and trace.
check_assertion <- function(machine, script, expect) {
  sim <- sim_start(machine)
  last_event <- NA_character_
  started_at <- 0
  outcome <- function(result, message) list(result = result, message = message, trace = sim$trace, clock = sim$clock, current = state_name(machine, sim$current))
  for (step in script) {
    if (!is.null(step$start)) {
      sim <- sim_start(machine, step$start)
    } else if (!is.null(step$advance_s)) {
      sim <- sim_advance(machine, sim, step$advance_s)
    } else if (!is.null(step$event)) {
      if (!is.null(step$hold_s)) {
        pending <- sim
        for (name in names(step$set)) pending$variables[[name]] <- step$set[[name]]
        held <- sim_step_hold(machine, pending, step$event)
        if (length(held) > 0L && !isTRUE(all.equal(held[1], step$hold_s))) {
          return(outcome("error", sprintf("The script says hold %s s, but the transition holds %s s.", format(step$hold_s, trim = TRUE), format(held[1], trim = TRUE))))
        }
      }
      last_event <- step$event
      started_at <- sim$clock
      sim <- sim_fire(machine, sim, step$event, if (is.null(step$set)) list() else step$set)
    } else {
      return(outcome("error", "A script step needs start, event or advance_s."))
    }
    if (sim$status != "ok") return(outcome("error", sim$error))
  }
  if (!is.null(expect$state)) {
    if (!identical(sim$current, expect$state)) {
      return(outcome("fail", sprintf("Expected %s, but it's in %s after %s.", state_name(machine, expect$state), state_name(machine, sim$current), if (is.na(last_event)) "the script" else last_event)))
    }
    elapsed <- sim$clock - started_at
    if (!is.null(expect$within_s) && elapsed > expect$within_s) {
      return(outcome("fail", sprintf("Entered %s %.1f s after %s, over the %.1f s limit.", state_name(machine, expect$state), elapsed, last_event, expect$within_s)))
    }
  }
  for (name in names(expect$vars)) {
    if (!identical(sim$variables[[name]], expect$vars[[name]]) && !isTRUE(all.equal(sim$variables[[name]], expect$vars[[name]]))) {
      return(outcome("fail", sprintf("Expected %s to be %s, but it's %s.", name, format(expect$vars[[name]]), format(sim$variables[[name]]))))
    }
  }
  message <- if (!is.null(expect$state) && !is.na(last_event)) {
    sprintf("entered %s %.1f s after %s", state_name(machine, expect$state), sim$clock - started_at, last_event)
  } else {
    "the expected values hold"
  }
  outcome("pass", message)
}

run_assertion_check <- function(machine, assertion) {
  parsed <- tryCatch(list(script = jsonlite::fromJSON(assertion$script_json, simplifyVector = FALSE), expect = jsonlite::fromJSON(assertion$expect_json, simplifyVector = FALSE)),
                     error = function(e) NULL)
  if (is.null(parsed)) return(list(result = "error", message = "The script isn't valid JSON.", trace = list()))
  check_assertion(machine, parsed$script, parsed$expect)
}

# Runs one assertion and records it with the model hash.
run_assertion <- function(con, assertion_id) {
  assertion <- DBI::dbGetQuery(con, "SELECT * FROM sm_assertion WHERE id = ?", params = list(assertion_id))
  if (nrow(assertion) == 0L) refuse("Assertion ", assertion_id, " doesn't exist.")
  outcome <- run_assertion_check(load_machine(con, assertion$sm_id), assertion)
  insert_sm_run(con, assertion_id, outcome$result, canonical_json(outcome$trace), model_hash(con))
  outcome
}

# The latest recorded run of each assertion, with whether it still matches the model.
assertion_status <- function(con, sm_id) {
  rows <- DBI::dbGetQuery(con, "
    SELECT a.id, a.requirement_id, a.description, a.script_json, a.expect_json, r.result, r.model_hash, r.run_at
      FROM sm_assertion a LEFT JOIN sm_run r ON r.id = (SELECT max(id) FROM sm_run WHERE assertion_id = a.id)
     WHERE a.sm_id = ? ORDER BY a.id", params = list(sm_id))
  rows$current <- !is.na(rows$model_hash) & rows$model_hash == model_hash(con)
  rows
}

# Checks every assertion live, without recording: the panel beside the simulator.
live_assertions <- function(con, sm_id) {
  machine <- load_machine(con, sm_id)
  rows <- assertion_status(con, sm_id)
  rows$live_result <- rep(NA_character_, nrow(rows))
  rows$live_message <- rep(NA_character_, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    outcome <- run_assertion_check(machine, rows[i, ])
    rows$live_result[i] <- outcome$result
    rows$live_message[i] <- outcome$message
  }
  rows
}

check_assertion_json <- function(script_json, expect_json) {
  script <- tryCatch(jsonlite::fromJSON(script_json, simplifyVector = FALSE), error = function(e) refuse("The script isn't valid JSON."))
  expect <- tryCatch(jsonlite::fromJSON(expect_json, simplifyVector = FALSE), error = function(e) refuse("The expected result isn't valid JSON."))
  if (!is.list(script) || length(script) == 0L) refuse("A script needs at least one step.")
  if (!is.list(expect) || is.null(expect$state) && is.null(expect$vars)) refuse("Say what's expected: a state, variables or both.")
  invisible(NULL)
}

add_assertion <- function(con, sm_id, requirement_id, description, script_json, expect_json) {
  if (!nzchar(trimws(description))) refuse("An assertion needs a description.")
  check_assertion_json(script_json, expect_json)
  with_change_set(con, function(change_set) {
    written <- write_sm_assertion(con, fields = list(sm_id = sm_id, requirement_id = requirement_id, description = trimws(description), script_json = script_json, expect_json = expect_json),
                                  note = "Added assertion", change_set = change_set)
    if (!is.na(requirement_id)) {
      write_link(con, fields = list(link_type = "verified_by", source_kind = "requirement", source_id = requirement_id, target_kind = "sm_assertion", target_id = written$id),
                 note = "Assertion verifies requirement", change_set = change_set)
    }
    written
  })
}

# --- Editing, with the gate ---

save_transition <- function(con, id = NULL, fields = list(), note = NULL, change_set = NULL, author = "engineer") {
  current <- if (is.null(id)) list() else read_entity(con, "sm_transition", id)
  row <- utils::modifyList(as.list(current), fields)
  scope <- expr_scope(con, row$sm_id)
  guard <- if (is.null(row$guard_expr)) NA_character_ else row$guard_expr
  effect <- if (is.null(row$effect_expr)) NA_character_ else row$effect_expr
  check_guard(guard, scope)
  check_effect(effect, scope)
  # Two transitions that are certain to tie are refused here; the rest stop the run.
  priority <- if (is.null(row$priority)) 0L else row$priority
  twins <- DBI::dbGetQuery(con, "SELECT id, guard_expr FROM sm_transition WHERE from_state_id = ? AND event_id = ? AND priority = ? AND id <> ?",
                           params = list(row$from_state_id, row$event_id, priority, if (is.null(id)) "" else id))
  if (any(blank_or_same(twins$guard_expr, guard))) refuse("Another transition out of this state has the same event, guard and priority.")
  write_sm_transition(con, id, fields, author, note, change_set)
}

blank_or_same <- function(existing, guard) {
  clean <- function(x) ifelse(is.na(x), "", trimws(x))
  clean(existing) == clean(guard)
}

save_state <- function(con, id = NULL, fields = list(), note = NULL, change_set = NULL, author = "engineer") {
  current <- if (is.null(id)) list() else read_entity(con, "sm_state", id)
  row <- utils::modifyList(as.list(current), fields)
  if (!is.null(fields$name) && !nzchar(trimws(fields$name))) refuse("A state needs a name.")
  check_effect(if (is.null(row$entry_effect)) NA_character_ else row$entry_effect, expr_scope(con, row$sm_id))
  write_sm_state(con, id, fields, author, note, change_set)
}

ignore_pair <- function(con, state_id, event_id, rationale) {
  if (!nzchar(trimws(rationale))) refuse("Ignoring an event needs your note: why it's safe to ignore here.")
  write_sm_ignore(con, paste(state_id, event_id, sep = "|"), list(rationale = trimws(rationale)), note = "Ignored on purpose")
}

unhandled_pairs <- function(con, sm_id) {
  DBI::dbGetQuery(con, "SELECT state_id, state_name, event_id, event_name FROM v_sm_unhandled WHERE sm_id = ? ORDER BY state_name, event_name", params = list(sm_id))
}

machine_view_id <- function(sm_id) paste0("VW-sm-", sm_id)

# The state machine as diagram data. States without a stored position are laid out by ELK.
machine_diagram <- function(con, sm_id) {
  machine <- load_machine(con, sm_id)
  view_id <- machine_view_id(sm_id)
  ensure_view(con, view_id, machine$name, "state_machine", sm_id)
  stored <- layout_rows(con, view_id)
  at <- match(machine$states$id, stored$item_id)
  nodes <- data.frame(id = machine$states$id, name = machine$states$name, shape = "state", review_state = machine$states$review_state,
                      initial = machine$states$id %in% machine$initial_state_id, final = machine$states$kind == "final",
                      x = stored$x[at], y = stored$y[at], stringsAsFactors = FALSE)
  transitions <- machine$transitions
  names_of <- machine$events$name[match(transitions$event_id, machine$events$id)]
  labels <- vapply(seq_len(nrow(transitions)), function(i) {
    row <- transitions[i, ]
    hold <- switch(row$hold_state, set = sprintf(" after %s s", format(row$hold_s, trim = TRUE)), placeholder = " after [value] s", "")
    paste0(names_of[i], if (!is.na(row$guard_expr) && nzchar(row$guard_expr)) sprintf(" [%s]", row$guard_expr), hold)
  }, "")
  edges <- data.frame(id = transitions$id, source_holder = transitions$from_state_id, target_holder = transitions$to_state_id, label = labels,
                      review_state = transitions$review_state, gap = transitions$hold_state == "placeholder", stringsAsFactors = FALSE)
  list(mode = "state_machine", root = sm_id, view_id = view_id, nodes = nodes, ports = data.frame(), edges = edges)
}
