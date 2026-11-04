# Scenarios: sequence steps, activity diagrams, consistency and behavioural impact
# (handoff §5.7). Rendering lives in the app.

scenario_list <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, name, description, has_sequence, has_activity, origin, review_state FROM scenario ORDER BY id")
}

scenario_lifelines <- function(con, scenario_id) {
  DBI::dbGetQuery(con, "
    SELECT l.id, l.element_id, l.sort_order, e.name FROM seq_lifeline l JOIN element e ON e.id = l.element_id
     WHERE l.scenario_id = ? ORDER BY l.sort_order, l.id", params = list(scenario_id))
}

# Steps in order, with whether their field exists on the interface.
scenario_steps <- function(con, scenario_id) {
  rows <- DBI::dbGetQuery(con, "
    SELECT s.*, i.name AS interface_name,
           s.field_name IS NOT NULL AND s.interface_id IS NOT NULL AND NOT EXISTS
             (SELECT 1 FROM interface_field f WHERE f.interface_id = s.interface_id AND f.name = s.field_name) AS field_missing
      FROM seq_step s LEFT JOIN interface i ON i.id = s.interface_id
     WHERE s.scenario_id = ? ORDER BY s.seq", params = list(scenario_id))
  rows$field_missing <- rows$field_missing == 1L
  rows
}

step_label <- function(step) {
  timing <- switch(step$timing_state, set = sprintf(" (%s s)", format(step$timing_s, trim = TRUE)), placeholder = " ([value] s)", "")
  paste0(step$label, timing)
}

# "N of M messages map to defined interfaces", plus each problem (handoff §5.7).
scenario_consistency <- function(con, scenario_id) {
  steps <- scenario_steps(con, scenario_id)
  messages <- steps[steps$step_kind %in% c("message", "self"), , drop = FALSE]
  no_interface <- messages[is.na(messages$interface_id), , drop = FALSE]
  missing <- messages[messages$field_missing, , drop = FALSE]
  problems <- c(
    lapply(seq_len(nrow(missing)), function(i) {
      list(kind = "field", step_id = missing$id[i], interface_id = missing$interface_id[i], field_name = missing$field_name[i],
           text = sprintf("%s: %s has no field %s", missing$label[i], missing$interface_name[i], missing$field_name[i]))
    }),
    lapply(seq_len(nrow(no_interface)), function(i) list(kind = "interface", step_id = no_interface$id[i], text = sprintf("%s: no interface chosen", no_interface$label[i])))
  )
  list(mapped = nrow(messages) - nrow(no_interface) - nrow(missing), total = nrow(messages), problems = problems)
}

add_field_for_step <- function(con, interface_id, field_name) {
  write_interface_field(con, fields = list(interface_id = interface_id, name = field_name, value_type = "string", range_state = "not_applicable"), note = "Added from a sequence step")
}

new_scenario <- function(con, name, description = NULL, has_sequence = TRUE, has_activity = FALSE) {
  if (!nzchar(trimws(name))) refuse("A scenario needs a name.")
  write_scenario(con, fields = list(name = trimws(name), description = description, has_sequence = as.integer(has_sequence), has_activity = as.integer(has_activity)), note = "New scenario")
}

add_lifeline <- function(con, scenario_id, element_id) {
  next_order <- DBI::dbGetQuery(con, "SELECT coalesce(max(sort_order), 0) + 1 AS n FROM seq_lifeline WHERE scenario_id = ?", params = list(scenario_id))$n
  write_seq_lifeline(con, fields = list(scenario_id = scenario_id, element_id = element_id, sort_order = as.integer(next_order)), note = "Added lifeline")
}

next_seq <- function(con, scenario_id) {
  as.integer(DBI::dbGetQuery(con, "SELECT coalesce(max(seq), 0) + 1 AS n FROM seq_step WHERE scenario_id = ?", params = list(scenario_id))$n)
}

# Toolbar: a message or note goes last; a fragment arrives as an opening and a closing row.
add_step <- function(con, scenario_id, kind) {
  lifelines <- scenario_lifelines(con, scenario_id)
  if (kind %in% c("message", "note") && nrow(lifelines) == 0L) refuse("Add a lifeline first.")
  at <- next_seq(con, scenario_id)
  with_change_set(con, function(change_set) {
    first <- lifelines$id[1]
    second <- lifelines$id[min(2L, nrow(lifelines))]
    switch(kind,
           message = write_seq_step(con, fields = list(scenario_id = scenario_id, seq = at, step_kind = if (first == second) "self" else "message", from_lifeline_id = first, to_lifeline_id = second, label = "New message"),
                                    note = "Added step", change_set = change_set),
           note = write_seq_step(con, fields = list(scenario_id = scenario_id, seq = at, step_kind = "note", from_lifeline_id = first, to_lifeline_id = first, label = "New note"), note = "Added step", change_set = change_set),
           fragment = {
             opened <- write_seq_step(con, fields = list(scenario_id = scenario_id, seq = at, step_kind = "fragment_open", fragment_kind = "opt", guard = "[guard]", label = "[guard]"), note = "Added fragment", change_set = change_set)
             write_seq_step(con, fields = list(scenario_id = scenario_id, seq = at + 1L, step_kind = "fragment_close", label = "end"), note = "Added fragment", change_set = change_set)
             opened
           })
  })
}

# Sets seq to the given order. UNIQUE (scenario_id, seq) means going through free numbers.
reorder_steps <- function(con, scenario_id, step_ids) {
  rows <- scenario_steps(con, scenario_id)
  if (!setequal(step_ids, rows$id) || anyDuplicated(step_ids)) refuse("That order doesn't match this scenario's steps.")
  with_change_set(con, function(change_set) {
    current <- rows$seq[match(step_ids, rows$id)]
    moved <- which(current != seq_along(step_ids))
    free <- max(c(rows$seq, length(step_ids))) + 1L
    for (i in moved) write_seq_step(con, step_ids[i], list(seq = free + i), note = "Reordered", change_set = change_set)
    for (i in moved) write_seq_step(con, step_ids[i], list(seq = as.integer(i)), note = "Reordered", change_set = change_set)
    list(change_set = change_set)
  })
}

# An edit from the step table: the timing cell is one box for three states.
update_step_field <- function(con, step_id, field, value) {
  value <- trimws(value)
  blank <- !nzchar(value)
  fields <- switch(field,
                   timing = if (blank) {
                     list(timing_state = "none", timing_s = NA_real_)
                   } else if (value == "[value]") {
                     list(timing_state = "placeholder", timing_s = NA_real_)
                   } else {
                     seconds <- suppressWarnings(as.numeric(value))
                     if (is.na(seconds) || seconds < 0) refuse("A timing is a number of seconds, or [value].")
                     list(timing_state = "set", timing_s = seconds)
                   },
                   stats::setNames(list(if (blank) NA_character_ else value), field))
  if (field == "label" && blank) refuse("A step needs a label.")
  if (field == "interface_id") fields$field_name <- NA_character_
  write_seq_step(con, step_id, fields, note = "Edited step")
}

# --- Activity ---

activity_view_id <- function(scenario_id) paste0("VW-act-", scenario_id)

activity_nodes <- function(con, scenario_id) {
  DBI::dbGetQuery(con, "SELECT n.*, e.name AS performer FROM act_node n LEFT JOIN element e ON e.id = n.performer_element_id WHERE n.scenario_id = ? ORDER BY n.id", params = list(scenario_id))
}

activity_edges <- function(con, scenario_id) {
  DBI::dbGetQuery(con, "SELECT * FROM act_edge WHERE scenario_id = ? ORDER BY id", params = list(scenario_id))
}

node_reads <- function(json) {
  if (is.na(json) || !nzchar(json)) return(list())
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# Properties a node may read: every recorded property on an element.
readable_properties <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT v.owner_id, e.name AS element, d.name AS property, d.label FROM property_value v
      JOIN property_def d ON d.id = v.property_def_id JOIN element e ON e.id = v.owner_id
     WHERE v.owner_kind = 'element' ORDER BY e.name, d.label")
}

# changed: reads rows of list(owner_kind, owner_id, property, text) marked amber, with their callout.
activity_diagram <- function(con, scenario_id, changed = list()) {
  view_id <- activity_view_id(scenario_id)
  ensure_view(con, view_id, sprintf("Activity %s", scenario_id), "activity", scenario_id)
  nodes <- activity_nodes(con, scenario_id)
  stored <- layout_rows(con, view_id)
  at <- match(nodes$id, stored$item_id)
  callouts <- vapply(seq_len(nrow(nodes)), function(i) {
    hits <- Filter(function(read) any(vapply(changed, function(item) identical(item$owner_id, read$owner_id) && identical(item$property, read$property), TRUE)), node_reads(nodes$reads_json[i]))
    if (length(hits) == 0L) return(NA_character_)
    item <- Filter(function(entry) identical(entry$owner_id, hits[[1]]$owner_id) && identical(entry$property, hits[[1]]$property), changed)[[1]]
    item$text
  }, "")
  out <- data.frame(id = nodes$id, name = ifelse(is.na(nodes$label), "", nodes$label), shape = nodes$node_kind, review_state = nodes$review_state,
                    x = stored$x[at], y = stored$y[at], stringsAsFactors = FALSE)
  out$gap <- !is.na(callouts)
  out$gap_note <- callouts
  edges <- activity_edges(con, scenario_id)
  list(mode = "activity", root = scenario_id, view_id = view_id, nodes = out, ports = data.frame(),
       edges = data.frame(id = edges$id, source_holder = edges$from_node_id, target_holder = edges$to_node_id, label = edges$guard, stringsAsFactors = FALSE))
}

add_act_node <- function(con, scenario_id, kind = "action", label = "New action") {
  write_act_node(con, fields = list(scenario_id = scenario_id, node_kind = kind, label = if (kind %in% c("initial", "final")) NA_character_ else label), note = "Added activity node")
}

set_node_reads <- function(con, node_id, reads) {
  write_act_node(con, node_id, list(reads_json = if (length(reads) == 0L) NA_character_ else canonical_json(reads)), note = "Edited what this node reads")
}

add_act_edge <- function(con, scenario_id, from, to) {
  known <- DBI::dbGetQuery(con, "SELECT id FROM act_node WHERE scenario_id = ? AND id IN (?, ?)", params = list(scenario_id, from, to))$id
  if (!all(c(from, to) %in% known)) refuse("Both ends have to be in this activity.")
  write_act_edge(con, fields = list(scenario_id = scenario_id, from_node_id = from, to_node_id = to), note = "Added activity edge")
}

# --- Behavioural impact ---

# Reads the old and new value out of a card's change text: "5000 → 4200 mAh".
change_values <- function(text) {
  parts <- strsplit(text, " → ", fixed = TRUE)[[1]]
  if (length(parts) != 2L) return(list(before = text, after = text))
  unit <- sub("^[^ ]+ ?", "", parts[2])
  before <- if (grepl(" ", parts[1], fixed = TRUE) || !nzchar(unit)) parts[1] else paste(parts[1], unit)
  list(before = before, after = parts[2])
}

# What a card changed, as inputs to behaviour: owner, property and text.
changed_inputs <- function(con, card) {
  values <- Filter(function(entity) entity$kind == "property_value", card$entities)
  lapply(values, function(entity) {
    row <- DBI::dbGetQuery(con, "SELECT v.owner_kind, v.owner_id, d.name AS property FROM property_value v JOIN property_def d ON d.id = v.property_def_id WHERE v.id = ?", params = list(entity$id))
    change <- change_values(entity$change_text)
    list(owner_kind = row$owner_kind, owner_id = row$owner_id, property = row$property, title = entity$title, before = change$before, after = change$after)
  })
}

changed_owner_ids <- function(con, card) {
  ids <- vapply(card$entities, function(entity) {
    if (entity$kind == "element") return(entity$id)
    if (entity$kind != "property_value") return(NA_character_)
    row <- DBI::dbGetQuery(con, "SELECT owner_kind, owner_id FROM property_value WHERE id = ?", params = list(entity$id))
    if (nrow(row) == 1L && row$owner_kind == "element") row$owner_id else NA_character_
  }, "")
  ids[!is.na(ids)]
}

changed_interfaces <- function(con, card) {
  ids <- vapply(card$entities, function(entity) {
    if (entity$kind == "interface") return(entity$id)
    if (entity$kind != "interface_field") return(NA_character_)
    row <- DBI::dbGetQuery(con, "SELECT interface_id FROM interface_field WHERE id = ?", params = list(entity$id))
    if (nrow(row) == 1L) row$interface_id else NA_character_
  }, "")
  ids[!is.na(ids)]
}

# Scenarios reached by impact_set(), each with the steps and nodes the change touches.
behavior_impact <- function(con, card) {
  scenarios <- impacted_scenarios(card_impact(con, card))
  inputs <- changed_inputs(con, card)
  owners <- changed_owner_ids(con, card)
  interfaces <- changed_interfaces(con, card)
  lapply(seq_len(nrow(scenarios)), function(i) {
    id <- scenarios$id[i]
    row <- DBI::dbGetQuery(con, "SELECT name, has_sequence, has_activity FROM scenario WHERE id = ?", params = list(id))
    steps <- scenario_steps(con, id)
    lifelines <- scenario_lifelines(con, id)
    touching <- lifelines$id[lifelines$element_id %in% owners]
    hit <- steps$id[(steps$from_lifeline_id %in% touching | steps$to_lifeline_id %in% touching | steps$interface_id %in% interfaces) & steps$step_kind %in% c("message", "self")]
    nodes <- activity_nodes(con, id)
    read_hits <- nodes$id[vapply(nodes$reads_json, function(json) {
      any(vapply(node_reads(json), function(read) any(vapply(inputs, function(item) identical(item$owner_id, read$owner_id) && identical(item$property, read$property), TRUE)), TRUE))
    }, TRUE, USE.NAMES = FALSE)]
    list(id = id, name = row$name, hops = scenarios$hops[i], path = scenarios$path[i], has_sequence = row$has_sequence == 1L, has_activity = row$has_activity == 1L,
         changed_steps = hit, changed_nodes = read_hits, inputs = inputs)
  })
}

# Callouts for one side of the before/after view.
impact_callouts <- function(inputs, side) {
  lapply(inputs, function(item) list(owner_kind = item$owner_kind, owner_id = item$owner_id, property = item$property, text = sprintf("%s: %s", item$title, item[[side]])))
}
