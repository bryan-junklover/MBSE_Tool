# Effect chains: what a failure leads to, worked out from the model rather than typed in
# (docs/VIEWPOINTS_AND_FAILURES.md §4). Computed when needed and never stored.
#
# - A failed element breaks every interface it sends: whoever receives them loses them.
# - A failed host fails every element that runs on it (hosted_on), and a failed component
#   fails its own parts.
# - Where something loses an input, its own failure mode for that loss decides what
#   happens. Handled or accepted stops the chain there. Otherwise losing power fails the
#   receiver and the chain goes on; losing data or matter is flagged and stops, because
#   the model doesn't say what the receiver does without it.

# Causes that mean an input stops arriving, and so match a receiver's failure mode.
LOSS_CAUSES <- c("lost", "stale", "failed", "open_circuit", "short_circuit")
EFFECT_STATUS <- c(handled = "handled", accepted = "accepted", open = "open", unrecorded = "unrecorded", propagates = "propagates", reaches = "reaches")

effect_model <- function(con) {
  list(
    elements = DBI::dbGetQuery(con, "SELECT id, name, kind, parent_id FROM element ORDER BY sort_order, name"),
    connectors = DBI::dbGetQuery(con, "
      SELECT c.id, sp.id AS source_port, se.id AS source_id, tp.id AS target_port, tp.name AS target_port_name,
             te.id AS target_id, i.id AS interface_id, i.name AS interface, i.flow_type
        FROM connector c JOIN port sp ON sp.id = c.source_port_id JOIN port tp ON tp.id = c.target_port_id
        JOIN element se ON se.id = sp.element_id JOIN element te ON te.id = tp.element_id
        JOIN interface i ON i.id = sp.interface_id WHERE i.layer = 'physical' ORDER BY c.id"),
    ports = DBI::dbGetQuery(con, "SELECT p.id, p.element_id, p.direction, p.interface_id FROM port p"),
    hosting = DBI::dbGetQuery(con, "SELECT source_id AS software, target_id AS host FROM link WHERE link_type = 'hosted_on'"),
    modes = failure_mode_list(con)
  )
}

# The chain for one failure mode, as rows in reading order: `depth` for indenting,
# `parent` the row it follows from, `status` and `text` for the reader.
effect_chain <- function(con, failure_id, model = effect_model(con), max_steps = 200L) {
  root <- model$modes[model$modes$id == failure_id, , drop = FALSE]
  if (nrow(root) == 0L) refuse("There's no failure mode ", failure_id, ".")
  name <- function(id) model$elements$name[match(id, model$elements$id)]
  # What the walk has seen so far: rows written, elements failed, inputs lost.
  seen <- new.env()
  seen$rows <- list()
  seen$failed <- character(0)
  seen$lost <- character(0)
  steps <- function() length(seen$rows)
  add <- function(depth, parent, status, text, element = NA_character_, failure = NA_character_) {
    seen$rows[[steps() + 1L]] <- data.frame(step = steps() + 1L, depth = depth, parent = parent, status = status,
                                            element = element, failure_id = failure, text = text)
    steps()
  }
  # The receiver's own failure mode for losing this input, preferring the most specific.
  receiver_mode <- function(port, interface_id) {
    m <- model$modes
    hit <- m$cause_kind %in% LOSS_CAUSES & ((m$subject_kind == "port" & m$subject_id == port) | (m$subject_kind == "interface" & m$subject_id == interface_id))
    found <- m[hit, , drop = FALSE]
    if (nrow(found) == 0L) return(NULL)
    found[order(found$subject_kind != "port"), , drop = FALSE][1, ]
  }

  fail_element <- function(element, depth, parent, reason) {
    if (element %in% seen$failed || steps() >= max_steps) return(invisible())
    seen$failed <- c(seen$failed, element)
    step <- add(depth, parent, "propagates", sprintf("%s stops working%s.", name(element), reason), element)
    outgoing <- model$connectors[model$connectors$source_id == element, , drop = FALSE]
    for (i in seq_len(nrow(outgoing))) lose_input(outgoing[i, ], depth + 1L, step)
    parts <- model$elements$id[model$elements$parent_id %in% element & model$elements$kind == "component"]
    for (part in parts) fail_element(part, depth + 1L, step, sprintf(", as part of %s", name(element)))
    hosted <- model$hosting$software[model$hosting$host == element]
    for (software in hosted) fail_element(software, depth + 1L, step, sprintf(", because it runs on %s", name(element)))
  }

  lose_input <- function(connector, depth, parent) {
    key <- paste(connector$target_port, connector$interface_id)
    if (key %in% seen$lost || steps() >= max_steps) return(invisible())
    seen$lost <- c(seen$lost, key)
    receiver <- connector$target_id
    what <- sprintf("%s loses %s from %s", name(receiver), connector$interface, name(connector$source_id))
    # Outside the system the chain only notes what reaches it.
    if (model$elements$kind[match(receiver, model$elements$id)] == "external") {
      add(depth, parent, "reaches", paste0(what, "."), receiver)
      return(invisible())
    }
    mode <- receiver_mode(connector$target_port, connector$interface_id)
    if (!is.null(mode) && mode$response_state %in% c("handled", "accepted")) {
      how <- c(if (!is.na(mode$response_note)) mode$response_note, if (nzchar(mode$handled_by)) sprintf("(%s)", mode$handled_by))
      add(depth, parent, mode$response_state, sprintf("%s. %s (%s)%s.", what, FAILURE_RESPONSES[[mode$response_state]], mode$id,
                                                       if (length(how) > 0L) paste0(": ", paste(how, collapse = " ")) else ""),
          receiver, mode$id)
      return(invisible())
    }
    if (connector$flow_type == "energy") {
      note <- if (is.null(mode)) "Nothing records what happens, so it loses power." else sprintf("%s records it, but %s.", mode$id,
                                                                                               if (mode$response_state == "unhandled") "nothing handles it" else "its response isn't decided")
      step <- add(depth, parent, if (is.null(mode)) "unrecorded" else "open", sprintf("%s. %s", what, note), receiver, if (is.null(mode)) NA_character_ else mode$id)
      fail_element(receiver, depth + 1L, step, "")
      return(invisible())
    }
    if (is.null(mode)) {
      add(depth, parent, "unrecorded", sprintf("%s. Nothing records what %s does without it.", what, name(receiver)), receiver)
    } else {
      add(depth, parent, "open", sprintf("%s. %s records it, but %s.", what, mode$id,
                                        if (mode$response_state == "unhandled") "nothing handles it" else "its response isn't decided"), receiver, mode$id)
    }
  }

  # Where the chain starts depends on what fails.
  kind <- root$subject_kind
  id <- root$subject_id
  if (kind == "element") {
    if (root$cause_kind %in% c("degraded", "stale", "out_of_range", "erroneous", "other")) {
      add(0L, NA_integer_, "open", sprintf("%s is %s; the model can't say how far that reaches.", name(id), tolower(FAILURE_CAUSES[[root$cause_kind]])), id, root$id)
    } else {
      fail_element(id, 0L, NA_integer_, sprintf(" (%s)", tolower(FAILURE_CAUSES[[root$cause_kind]])))
    }
  } else if (kind == "port") {
    port <- model$ports[model$ports$id == id, , drop = FALSE]
    incoming <- model$connectors[model$connectors$target_port == id, , drop = FALSE]
    outgoing <- model$connectors[model$connectors$source_port == id, , drop = FALSE]
    for (i in seq_len(nrow(incoming))) lose_input(incoming[i, ], 0L, NA_integer_)
    for (i in seq_len(nrow(outgoing))) lose_input(outgoing[i, ], 0L, NA_integer_)
    if (nrow(incoming) + nrow(outgoing) == 0L) add(0L, NA_integer_, "open", sprintf("%s has no connector, so nothing else is affected yet.", root$subject), port$element_id, root$id)
  } else if (kind == "interface") {
    carrying <- model$connectors[model$connectors$interface_id == id, , drop = FALSE]
    for (i in seq_len(nrow(carrying))) lose_input(carrying[i, ], 0L, NA_integer_)
    if (nrow(carrying) == 0L) add(0L, NA_integer_, "open", sprintf("%s isn't connected, so nothing else is affected yet.", root$subject), NA_character_, root$id)
  } else {
    add(0L, NA_integer_, "open", "A state's failure stays inside its state machine; the model doesn't trace it further.", NA_character_, root$id)
  }
  if (steps() >= max_steps) add(0L, NA_integer_, "open", sprintf("The chain was cut at %d steps.", max_steps))
  do.call(rbind, c(list(data.frame(step = integer(0), depth = integer(0), parent = integer(0), status = character(0),
                                   element = character(0), failure_id = character(0), text = character(0))), seen$rows))
}

# How a chain ends: the parts that stop, and the losses nobody has decided about.
effect_summary <- function(chain) {
  stopped <- unique(chain$element[chain$status == "propagates"])
  list(stopped = stopped[!is.na(stopped)], open = sum(chain$status %in% c("open", "unrecorded")),
       handled = sum(chain$status %in% c("handled", "accepted")))
}

effect_chain_markdown <- function(chain) {
  if (nrow(chain) == 0L) return("_Nothing else is affected._")
  marks <- c(handled = "✓", accepted = "✓", open = "⚠", unrecorded = "⚠", propagates = "→", reaches = "→")
  paste(sprintf("%s- %s %s", strrep("    ", chain$depth), marks[chain$status], chain$text), collapse = "\n")
}
