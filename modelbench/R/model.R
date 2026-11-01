# Model queries and edits behind the tree, inspector, diagram and interface dictionary.

# Components and externals, depth-first, one row each with its depth.
model_tree <- function(con) {
  rows <- DBI::dbGetQuery(con, "
    SELECT e.id, e.name, e.kind, e.parent_id, e.multiplicity, e.review_state, s.name AS stereotype
      FROM element e LEFT JOIN stereotype s ON s.id = e.stereotype_id
     WHERE e.kind IN ('component', 'external') ORDER BY e.kind, e.sort_order, e.name")
  walk <- new.env()
  walk$order <- integer(0)
  walk$depth <- integer(nrow(rows))
  visit <- function(indexes, depth) {
    for (i in indexes) {
      walk$depth[i] <- depth
      walk$order <- c(walk$order, i)
      visit(which(rows$parent_id %in% rows$id[i]), depth + 1L)
    }
  }
  visit(which(is.na(rows$parent_id) | !rows$parent_id %in% rows$id), 0L)
  rows$depth <- walk$depth
  out <- rows[walk$order, , drop = FALSE]
  rownames(out) <- NULL
  out
}

element_properties <- function(con, element_id) {
  DBI::dbGetQuery(con, "
    SELECT v.id, d.name, d.label, d.value_type, v.value_state, v.num_value, v.text_value, coalesce(v.unit, d.unit) AS unit
      FROM property_value v JOIN property_def d ON d.id = v.property_def_id
     WHERE v.owner_kind = 'element' AND v.owner_id = ? ORDER BY d.label", params = list(element_id))
}

element_ports <- function(con, element_id) {
  DBI::dbGetQuery(con, "
    SELECT p.id, p.name, p.direction, p.side, p.interface_id, i.name AS interface,
           EXISTS (SELECT 1 FROM connector c WHERE c.source_port_id = p.id OR c.target_port_id = p.id) AS connected
      FROM port p LEFT JOIN interface i ON i.id = p.interface_id WHERE p.element_id = ? ORDER BY p.name",
                  params = list(element_id))
}

element_requirements <- function(con, element_id) {
  DBI::dbGetQuery(con, "
    SELECT r.id, r.short_title FROM requirement r JOIN link l ON l.source_kind = 'requirement' AND l.source_id = r.id
     WHERE l.link_type = 'satisfied_by' AND l.target_kind = 'element' AND l.target_id = ? ORDER BY r.id",
                  params = list(element_id))
}

# Sets a value, or clears it back to a placeholder when `value` is blank. Numbers
# are checked here, because the database stores whatever it's given.
set_property_value <- function(con, value_id, value, author = "engineer") {
  row <- read_entity(con, "property_value", value_id)
  if (is.null(row)) refuse("That property no longer exists.")
  def <- read_entity(con, "property_def", row$property_def_id)
  text <- trimws(as.character(value))
  if (!nzchar(text)) {
    return(write_property_value(con, value_id, list(value_state = "placeholder", num_value = NA, text_value = NA), author))
  }
  if (def$value_type == "number") {
    number <- suppressWarnings(as.numeric(text))
    if (is.na(number)) refuse(def$label, " needs a number, not \"", text, "\".")
    return(write_property_value(con, value_id, list(value_state = "set", num_value = number, text_value = NA, set_by = "human"), author))
  }
  write_property_value(con, value_id, list(value_state = "set", num_value = NA, text_value = text, set_by = "human"), author)
}

# A human adds a property with no value yet: a placeholder, shown as [value].
add_property_to_element <- function(con, element_id, property_def_id, author = "engineer") {
  def <- read_entity(con, "property_def", property_def_id)
  write_property_value(con, fields = list(owner_kind = "element", owner_id = element_id, property_def_id = property_def_id,
                                          value_state = "placeholder", unit = def$unit), author = author)
}

# An engineer's note makes AI-originated content ordinary model content; origin stays.
review_entity <- function(con, kind, id, note, author = "engineer") {
  if (!nzchar(trimws(note))) refuse("Reviewing needs a note in your own words.")
  write_entity(con, kind, id, list(review_state = "reviewed", review_note = trimws(note)), author, "Reviewed")
}

# Wraps the selection in a new parent at their level (handoff §5.4).
group_into_subsystem <- function(con, element_ids, name = "New subsystem", author = "engineer") {
  if (length(element_ids) < 1L) refuse("Select the components to group first.")
  rows <- lapply(element_ids, function(id) read_entity(con, "element", id))
  parents <- unique(vapply(rows, function(row) if (is.na(row$parent_id)) "" else row$parent_id, ""))
  if (length(parents) > 1L) refuse("Only components at the same level can be grouped.")
  with_change_set(con, function(change_set) {
    parent <- if (nzchar(parents)) parents else NA_character_
    group <- write_element(con, fields = list(kind = "component", name = name, parent_id = parent),
                           author = author, change_set = change_set)
    for (id in element_ids) write_element(con, id, list(parent_id = group$id), author, change_set = change_set)
    group
  })
}

default_structure_root <- function(con) {
  tops <- DBI::dbGetQuery(con, "SELECT id FROM element WHERE kind = 'component' AND parent_id IS NULL ORDER BY sort_order, id")$id
  if (length(tops) == 1L) tops else NA_character_
}

element_breadcrumb <- function(con, root) {
  path <- character(0)
  current <- root
  while (length(current) == 1L && !is.na(current)) {
    path <- c(current, path)
    current <- read_entity(con, "element", current)$parent_id
  }
  path
}

# Does an element pass a saved view's filter (handoff §5.4)?
filter_elements <- function(con, nodes, filter_json) {
  if (is.null(filter_json) || is.na(filter_json) || nrow(nodes) == 0L) return(rep(TRUE, nrow(nodes)))
  filter <- jsonlite::fromJSON(filter_json, simplifyVector = TRUE)
  keep <- rep(TRUE, nrow(nodes))
  if (!is.null(filter$kinds)) keep <- keep & nodes$kind %in% filter$kinds
  if (!is.null(filter$stereotypes)) keep <- keep & nodes$stereotype %in% filter$stereotypes
  if (!is.null(filter$property)) {
    held <- DBI::dbGetQuery(con, "SELECT v.owner_id, v.num_value FROM property_value v JOIN property_def d ON d.id = v.property_def_id
                                   WHERE v.owner_kind = 'element' AND d.name = ? AND v.value_state = 'set'",
                            params = list(filter$property$name))
    compare <- match.fun(filter$property$op)
    matching <- held$owner_id[!is.na(held$num_value) & compare(held$num_value, filter$property$value)]
    keep <- keep & nodes$id %in% matching
  }
  if (!is.null(filter$allocated_to)) {
    allocated <- DBI::dbGetQuery(con, "SELECT source_id FROM link WHERE link_type = 'allocated_to' AND target_id = ?",
                                 params = list(filter$allocated_to))$source_id
    keep <- keep & nodes$id %in% allocated
  }
  keep
}

# What a Structure diagram shows at one level: the children of `root`, its ports as
# boundary ports, and each connector drawn between the blocks that hold its ends. A
# connector to something hidden is a stub to the view's edge.
structure_view <- function(con, view_id = "VW-all", root = default_structure_root(con), include_external = FALSE) {
  view <- DBI::dbGetQuery(con, "SELECT * FROM view_def WHERE id = ?", params = list(view_id))
  filter_json <- if (nrow(view) == 1L) view$filter_json else NA_character_
  all <- DBI::dbGetQuery(con, "
    SELECT e.id, e.name, e.kind, e.parent_id, e.multiplicity, e.review_state, e.origin, s.name AS stereotype
      FROM element e LEFT JOIN stereotype s ON s.id = e.stereotype_id")
  children <- all[if (is.na(root)) is.na(all$parent_id) else all$parent_id %in% root, , drop = FALSE]
  children <- children[children$kind != "function", , drop = FALSE]
  if (include_external) children <- rbind(children, all[all$kind == "external" & is.na(all$parent_id), , drop = FALSE])
  nodes <- children[filter_elements(con, children, filter_json), , drop = FALSE]

  ports <- DBI::dbGetQuery(con, "
    SELECT p.id, p.element_id, p.name, p.direction, p.side, p.interface_id, i.name AS interface,
           NOT EXISTS (SELECT 1 FROM connector c WHERE c.source_port_id = p.id OR c.target_port_id = p.id) AS unconnected
      FROM port p LEFT JOIN interface i ON i.id = p.interface_id")
  connectors <- DBI::dbGetQuery(con, "
    SELECT c.id, c.label, c.review_state, c.source_port_id, c.target_port_id, s.element_id AS source_element, t.element_id AS target_element,
           i.flow_type FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id
      LEFT JOIN interface i ON i.id = s.interface_id")

  # Which visible block holds an element: itself, or the ancestor shown at this level.
  holder <- function(element) {
    if (!is.na(root) && identical(element, root)) return(root)
    current <- element
    while (length(current) == 1L && !is.na(current)) {
      if (current %in% nodes$id) return(current)
      if (!is.na(root) && identical(current, root)) return(NA_character_)
      current <- all$parent_id[match(current, all$id)]
    }
    NA_character_
  }
  hold_source <- vapply(connectors$source_element, holder, "", USE.NAMES = FALSE)
  hold_target <- vapply(connectors$target_element, holder, "", USE.NAMES = FALSE)
  shown <- (!is.na(hold_source) | !is.na(hold_target)) & (is.na(hold_source) | is.na(hold_target) | hold_source != hold_target)
  # A connector from the boundary to outside this level belongs to the level above.
  on_boundary <- !is.na(root) & ((hold_source %in% root & is.na(hold_target)) | (hold_target %in% root & is.na(hold_source)))
  shown <- shown & !on_boundary
  edges <- connectors[shown, , drop = FALSE]
  edges$source_holder <- hold_source[shown]
  edges$target_holder <- hold_target[shown]
  # A stub has one end hidden.
  edges$stub <- is.na(edges$source_holder) | is.na(edges$target_holder)

  unconnected <- DBI::dbGetQuery(con, "SELECT port_id, element_id, name FROM v_unconnected_ports")
  nodes$gap <- nodes$id %in% unconnected$element_id
  nodes$gap_note <- vapply(nodes$id, function(id) {
    held <- unconnected$name[unconnected$element_id == id]
    if (length(held) == 0L) NA_character_ else sprintf("%s has no connector", paste(held, collapse = ", "))
  }, "", USE.NAMES = FALSE)

  stored <- layout_rows(con, view_id)
  stored <- stored[stored$item_kind == "element", , drop = FALSE]
  at <- match(nodes$id, stored$item_id)
  nodes$x <- stored$x[at]
  nodes$y <- stored$y[at]
  nodes$pinned <- grepl('"pinned":true', stored$extra_json[at], fixed = TRUE)

  list(view_id = view_id, root = root,
       breadcrumb = if (is.na(root)) character(0) else element_breadcrumb(con, root),
       nodes = nodes,
       ports = ports[ports$element_id %in% nodes$id, , drop = FALSE],
       boundary_ports = if (is.na(root)) ports[0, , drop = FALSE] else ports[ports$element_id == root, , drop = FALSE],
       edges = edges)
}

# Moved blocks, as layout rows.
layout_items <- function(positions, pinned = NULL) {
  data.frame(item_kind = "element", item_id = positions$id, x = positions$x, y = positions$y, w = NA_real_, h = NA_real_,
             extra_json = if (is.null(pinned)) NA_character_ else ifelse(pinned, '{"pinned":true}', '{"pinned":false}'),
             stringsAsFactors = FALSE)
}

set_pinned <- function(con, view_id, element_id, pinned) {
  row <- layout_rows(con, view_id, element_id)
  x <- if (nrow(row) == 1L) row$x else 0
  y <- if (nrow(row) == 1L) row$y else 0
  save_layout(con, view_id, layout_items(data.frame(id = element_id, x = x, y = y), pinned))
}

interface_list <- function(con, layer = "physical") {
  DBI::dbGetQuery(con, "
    SELECT i.id, i.name, i.flow_type, i.review_state, (SELECT count(*) FROM port p WHERE p.interface_id = i.id) AS ports,
           (SELECT count(*) FROM interface_field f WHERE f.interface_id = i.id AND f.range_state = 'placeholder') AS placeholders
      FROM interface i WHERE i.layer = ? ORDER BY i.flow_type, i.name", params = list(layer))
}

interface_fields <- function(con, interface_id) {
  DBI::dbGetQuery(con, "SELECT * FROM interface_field WHERE interface_id = ? ORDER BY sort_order, name", params = list(interface_id))
}

# Everywhere an interface is used.
interface_where_used <- function(con, interface_id) {
  list(
    ports = DBI::dbGetQuery(con, "SELECT p.id, p.name, p.direction, e.id AS element_id, e.name AS element FROM port p
                                   JOIN element e ON e.id = p.element_id WHERE p.interface_id = ? ORDER BY e.name", params = list(interface_id)),
    scenarios = DBI::dbGetQuery(con, "SELECT DISTINCT s.id, s.name FROM scenario s JOIN seq_step t ON t.scenario_id = s.id
                                       WHERE t.interface_id = ? ORDER BY s.name", params = list(interface_id)),
    events = DBI::dbGetQuery(con, "SELECT id, name FROM sm_event WHERE interface_id = ? ORDER BY name", params = list(interface_id)),
    requirements = DBI::dbGetQuery(con, "SELECT DISTINCT r.id, r.short_title FROM requirement r
                                          JOIN link l ON l.source_kind = 'requirement' AND l.source_id = r.id AND l.link_type = 'satisfied_by'
                                          JOIN port p ON p.element_id = l.target_id WHERE p.interface_id = ? ORDER BY r.id", params = list(interface_id)),
    functional_flows = DBI::dbGetQuery(con, "SELECT DISTINCT f.id, f.label FROM link l JOIN connector f ON f.id = l.source_id
                                              JOIN connector k ON k.id = l.target_id JOIN port p ON p.id = k.source_port_id
                                              WHERE l.link_type = 'realized_by' AND p.interface_id = ? ORDER BY f.id", params = list(interface_id))
  )
}

save_interface_field <- function(con, field_id, fields, author = "engineer") {
  state <- fields$range_state
  if (!is.null(state) && state != "set") {
    fields$min_value <- NA
    fields$max_value <- NA
  }
  write_interface_field(con, field_id, fields, author)
}

# "-90 … 90" or "-90 .. 90" sets a range, blank is a placeholder and "n/a" says none applies.
parse_field_range <- function(text) {
  clean <- trimws(gsub("\u2212", "-", text))
  if (!nzchar(clean)) return(list(range_state = "placeholder", min_value = NA, max_value = NA))
  if (tolower(clean) %in% c("n/a", "na", "none")) return(list(range_state = "not_applicable", min_value = NA, max_value = NA))
  parts <- trimws(strsplit(clean, "\u2026|\\.\\.|\\bto\\b")[[1]])
  numbers <- suppressWarnings(as.numeric(parts))
  if (length(numbers) != 2L || anyNA(numbers)) refuse("A range is two numbers, like 0 … 360.")
  if (numbers[1] > numbers[2]) refuse("A range's minimum can't be above its maximum.")
  list(range_state = "set", min_value = numbers[1], max_value = numbers[2])
}

format_field_range <- function(state, min_value, max_value) {
  switch(state, set = paste(format(min_value, trim = TRUE), "…", format(max_value, trim = TRUE)), not_applicable = "n/a", "")
}

# "50 Hz" sets a rate and blank clears it. The unit must be in the registry.
parse_interface_rate <- function(con, text) {
  clean <- trimws(text)
  if (!nzchar(clean)) return(list(rate_value = NA, rate_unit = NA))
  parts <- strsplit(clean, "\\s+")[[1]]
  number <- suppressWarnings(as.numeric(parts[1]))
  if (length(parts) != 2L || is.na(number)) refuse("A rate is a number and a unit, like 50 Hz.")
  if (length(unit_quantity(con, parts[2])) == 0L) refuse(parts[2], " isn't in the unit registry.")
  list(rate_value = number, rate_unit = parts[2])
}

# --- Health: computed from the model, no AI calls ---

# The blockers a baseline would be refused for, in words, with where to fix them.
baseline_blockers_detail <- function(con) {
  placeholders <- DBI::dbGetQuery(con, "
    SELECT p.entity_kind, p.entity_id, p.owner_kind, p.owner_id, p.item, p.unit,
           coalesce(e.name, i.name, s.name, c.name, p.owner_id) AS owner
      FROM v_placeholders p
      LEFT JOIN element e ON p.owner_kind = 'element' AND e.id = p.owner_id
      LEFT JOIN interface i ON p.owner_kind = 'interface' AND i.id = p.owner_id
      LEFT JOIN state_machine s ON p.owner_kind = 'state_machine' AND s.id = p.owner_id
      LEFT JOIN scenario c ON p.owner_kind = 'scenario' AND c.id = p.owner_id ORDER BY p.entity_id")
  placeholders$text <- vapply(seq_len(nrow(placeholders)), function(i) {
    row <- placeholders[i, ]
    amount <- if (row$entity_kind == "interface_field") "[min] … [max]" else "[value]"
    sprintf("%s.%s · %s%s", row$owner, row$item, amount, if (is.na(row$unit)) "" else paste0(" ", row$unit))
  }, "")
  list(placeholders = placeholders,
       unreviewed = DBI::dbGetQuery(con, "SELECT entity_kind, entity_id, label FROM v_unreviewed ORDER BY entity_kind, entity_id"),
       stale = stale_evidence(con),
       draft_placeholders = count_of(con, "SELECT count(*) AS n FROM v_draft_placeholders"))
}

count_of <- function(con, sql) as.integer(DBI::dbGetQuery(con, sql)$n)

# Messages naming a field their interface doesn't have.
missing_field_steps <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT s.id, s.scenario_id, s.label, s.field_name FROM seq_step s
     WHERE s.field_name IS NOT NULL AND s.interface_id IS NOT NULL
       AND NOT EXISTS (SELECT 1 FROM interface_field f WHERE f.interface_id = s.interface_id AND f.name = s.field_name)")
}

model_health <- function(con) {
  top <- default_structure_root(con)
  list(
    blockers = baseline_blockers_detail(con),
    requirements = list(
      committed = count_of(con, "SELECT count(*) AS n FROM requirement WHERE status = 'committed'"),
      passing = count_of(con, "SELECT count(*) AS n FROM requirement WHERE status <> 'deprecated' AND gate_pass = 1"),
      failing = count_of(con, "SELECT count(*) AS n FROM requirement WHERE status <> 'deprecated' AND gate_pass = 0"),
      uncoached = count_of(con, "SELECT count(*) AS n FROM requirement WHERE status = 'committed' AND coach_state = 'uncoached'"),
      unallocated = count_of(con, "SELECT count(*) AS n FROM v_unallocated_requirements"),
      no_verification = count_of(con, "SELECT count(*) AS n FROM requirement WHERE status = 'committed' AND verification_method IS NULL")
    ),
    architecture = list(
      components = count_of(con, "SELECT count(*) AS n FROM element WHERE kind = 'component'") - as.integer(!is.na(top)),
      unconnected_ports = count_of(con, "SELECT count(*) AS n FROM v_unconnected_ports"),
      no_requirements = count_of(con, sprintf("
        SELECT count(*) AS n FROM element e WHERE e.kind = 'component' AND e.id <> %s AND NOT EXISTS (
          SELECT 1 FROM link l WHERE l.link_type = 'satisfied_by' AND l.target_id = e.id)",
        as.character(DBI::dbQuoteString(con, if (is.na(top)) "" else top)))),
      interfaces = count_of(con, "SELECT count(*) AS n FROM interface WHERE layer = 'physical'"),
      gate_findings = nrow(element_gate_findings(con))
    ),
    functional = list(
      functions = count_of(con, "SELECT count(*) AS n FROM element WHERE kind = 'function'"),
      unallocated = count_of(con, "SELECT count(*) AS n FROM v_unallocated_functions"),
      flows = nrow(realisation_check(con)),
      flows_realised = sum(realisation_check(con)$realised)
    ),
    behavior = list(
      scenarios = count_of(con, "SELECT count(*) AS n FROM scenario"),
      missing_fields = nrow(missing_field_steps(con)),
      unhandled = count_of(con, "SELECT count(*) AS n FROM v_sm_unhandled")
    ),
    budgets = budget_status(con),
    analyses = analysis_status(con),
    ai = list(
      kept = count_of(con, "SELECT count(*) AS n FROM ai_proposal WHERE status = 'kept'"),
      dismissed = count_of(con, "SELECT count(*) AS n FROM ai_proposal WHERE status = 'dismissed'"),
      unreviewed = count_of(con, "SELECT count(*) AS n FROM v_unreviewed"),
      invented = count_of(con, "SELECT coalesce(sum(json_extract(payload_json, '$._invented')), 0) AS n FROM ai_proposal")
    )
  )
}

# A synthetic model for the performance gate (BUILD_PLAN Phase 2).
build_synthetic_model <- function(con, components = 150L, ports_each = 4L) {
  with_change_set(con, function(change_set) {
    write_interface(con, "IF-SYN", list(name = "SynData", flow_type = "data"), change_set = change_set)
    write_element(con, "C-ROOT", list(kind = "component", name = "Synthetic system"), change_set = change_set)
    for (i in seq_len(components)) {
      id <- sprintf("C-S%03d", i)
      write_element(con, id, list(kind = "component", name = sprintf("Block %d", i), parent_id = "C-ROOT"), change_set = change_set)
      for (k in seq_len(ports_each)) {
        direction <- if (k %% 2L == 0L) "out" else "in"
        write_port(con, sprintf("P-S%03d-%d", i, k),
                   list(element_id = id, name = paste0(direction, k), direction = direction, interface_id = "IF-SYN",
                        side = if (direction == "in") "top" else "bottom"), change_set = change_set)
      }
    }
    for (i in seq_len(components)) {
      for (k in seq(2L, ports_each, by = 2L)) {
        target <- (i + (i * 7L + k * 13L) %% min(components - 1L, 9L)) %% components + 1L
        if (target == i) next
        target_port <- sprintf("P-S%03d-%d", target, k - 1L)
        taken <- DBI::dbGetQuery(con, "SELECT 1 FROM connector WHERE target_port_id = ?", params = list(target_port))
        if (nrow(taken) == 0L) {
          write_connector(con, sprintf("K-S%03d-%d", i, k),
                          list(source_port_id = sprintf("P-S%03d-%d", i, k), target_port_id = target_port), change_set = change_set)
        }
      }
    }
    change_set
  })
}

# A new connector, labelled with its interface like the ones an engineer draws.
connect_ports <- function(con, source_port_id, target_port_id, author = "engineer") {
  interface <- DBI::dbGetQuery(con, "SELECT i.name FROM port p JOIN interface i ON i.id = p.interface_id WHERE p.id = ?",
                               params = list(source_port_id))$name
  write_connector(con, fields = list(source_port_id = source_port_id, target_port_id = target_port_id,
                                     label = if (length(interface) == 1L) interface else NA_character_), author = author)
}

connector_detail <- function(con, connector_id) {
  DBI::dbGetQuery(con, "
    SELECT c.id, c.label, s.name AS source_port, t.name AS target_port, a.name AS source_element, b.name AS target_element, i.name AS interface
      FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id
      JOIN element a ON a.id = s.element_id JOIN element b ON b.id = t.element_id LEFT JOIN interface i ON i.id = s.interface_id
     WHERE c.id = ?", params = list(connector_id))
}

next_interface_name <- function(con) {
  names <- DBI::dbGetQuery(con, "SELECT name FROM interface")$name
  n <- 1L
  while (sprintf("NewInterface%d", n) %in% names) n <- n + 1L
  sprintf("NewInterface%d", n)
}

next_field_name <- function(con, interface_id) {
  names <- interface_fields(con, interface_id)$name
  n <- 1L
  while (sprintf("new_field_%d", n) %in% names) n <- n + 1L
  sprintf("new_field_%d", n)
}

# --- Flows and Functional views ---

connector_quantities <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT v.owner_id AS connector_id, v.num_value, coalesce(v.unit, d.unit) AS unit, d.name FROM property_value v
      JOIN property_def d ON d.id = v.property_def_id
     WHERE v.owner_kind = 'connector' AND v.value_state = 'set' AND v.num_value IS NOT NULL
     ORDER BY v.owner_id, CASE d.name WHEN 'bus_voltage' THEN 0 ELSE 1 END, d.name")
}

# "28 V · 380 W" from the connector's values, or the interface's rate ("50 Hz").
flow_label <- function(connector_id, label, rate_value, rate_unit, quantities) {
  mine <- quantities[quantities$connector_id == connector_id, , drop = FALSE]
  if (nrow(mine) > 0L) return(paste(sprintf("%s %s", format(mine$num_value, trim = TRUE, drop0trailing = TRUE), mine$unit), collapse = " · "))
  if (!is.na(rate_value)) return(sprintf("%s %s", format(rate_value, trim = TRUE, drop0trailing = TRUE), rate_unit))
  label
}

flows_view <- function(con, flow_types = c("data", "energy", "matter"), root = default_structure_root(con), quantities = TRUE) {
  view <- structure_view(con, "VW-flows", root, include_external = TRUE)
  connected <- unique(c(view$edges$source_holder, view$edges$target_holder))
  view$nodes <- view$nodes[view$nodes$kind != "external" | view$nodes$id %in% connected, , drop = FALSE]
  view$ports <- view$ports[view$ports$element_id %in% view$nodes$id, , drop = FALSE]
  edges <- view$edges[view$edges$flow_type %in% flow_types, , drop = FALSE]
  if (quantities && nrow(edges) > 0L) {
    rates <- DBI::dbGetQuery(con, "SELECT c.id, i.rate_value, i.rate_unit FROM connector c JOIN port p ON p.id = c.source_port_id JOIN interface i ON i.id = p.interface_id")
    amounts <- connector_quantities(con)
    at <- match(edges$id, rates$id)
    edges$label <- vapply(seq_len(nrow(edges)), function(i) {
      flow_label(edges$id[i], edges$label[i], rates$rate_value[at[i]], rates$rate_unit[at[i]], amounts)
    }, "")
  }
  view$edges <- edges
  view
}

FLOW_PREFIX <- c(data = "D:", energy = "E:", matter = "M:")

functional_view <- function(con) {
  nodes <- DBI::dbGetQuery(con, "
    SELECT e.id, e.name, e.kind, e.parent_id, e.multiplicity, e.review_state, e.origin, NULL AS stereotype
      FROM element e WHERE e.kind = 'function' ORDER BY e.sort_order, e.id")
  allocations <- DBI::dbGetQuery(con, "
    SELECT l.source_id AS function_id, e.name FROM link l JOIN element e ON e.id = l.target_id
     WHERE l.link_type = 'allocated_to' AND l.source_kind = 'element' ORDER BY e.sort_order")
  nodes$allocation <- vapply(nodes$id, function(id) {
    names <- allocations$name[allocations$function_id == id]
    if (length(names) == 0L) "→ unallocated" else paste("→", paste(names, collapse = ", "))
  }, "", USE.NAMES = FALSE)
  nodes$unallocated <- !nodes$id %in% allocations$function_id
  stored <- layout_rows(con, "VW-functional")
  at <- match(nodes$id, stored$item_id)
  nodes$x <- stored$x[at]
  nodes$y <- stored$y[at]
  nodes$pinned <- grepl('"pinned":true', stored$extra_json[at], fixed = TRUE)
  nodes$gap <- FALSE
  nodes$gap_note <- NA_character_
  ports <- DBI::dbGetQuery(con, "
    SELECT p.id, p.element_id, p.name, p.direction, p.side, p.interface_id, i.name AS interface,
           NOT EXISTS (SELECT 1 FROM connector c WHERE c.source_port_id = p.id OR c.target_port_id = p.id) AS unconnected
      FROM port p JOIN element e ON e.id = p.element_id LEFT JOIN interface i ON i.id = p.interface_id WHERE e.kind = 'function'")
  edges <- DBI::dbGetQuery(con, "
    SELECT c.id, c.label, c.review_state, c.source_port_id, c.target_port_id, s.element_id AS source_holder, t.element_id AS target_holder, i.flow_type
      FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id
      JOIN element a ON a.id = s.element_id LEFT JOIN interface i ON i.id = s.interface_id WHERE a.kind = 'function'")
  edges$stub <- rep(FALSE, nrow(edges))
  prefix <- FLOW_PREFIX[edges$flow_type]
  edges$label <- ifelse(is.na(prefix), edges$label, paste(prefix, ifelse(is.na(edges$label), "", edges$label)))
  list(view_id = "VW-functional", root = NA_character_, breadcrumb = character(0), nodes = nodes, ports = ports,
       boundary_ports = ports[0, , drop = FALSE], edges = edges)
}

# Does an element sit inside a component (or the other way round)?
elements_related <- function(a, b, parents) {
  chain <- function(id) {
    out <- character(0)
    while (length(id) == 1L && !is.na(id)) {
      out <- c(out, id)
      id <- parents[id]
    }
    out
  }
  any(chain(a) %in% b) || any(chain(b) %in% a)
}

# One row per functional flow: is it carried by a physical connector between the
# components its two functions are allocated to?
realisation_check <- function(con) {
  flows <- DBI::dbGetQuery(con, "
    SELECT c.id, c.label, s.element_id AS source_function, t.element_id AS target_function
      FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id
      JOIN element a ON a.id = s.element_id WHERE a.kind = 'function' ORDER BY c.id")
  allocations <- DBI::dbGetQuery(con, "SELECT source_id, target_id FROM link WHERE link_type = 'allocated_to' AND source_kind = 'element'")
  physical <- DBI::dbGetQuery(con, "
    SELECT c.id, c.label, s.element_id AS a, t.element_id AS b FROM connector c JOIN port s ON s.id = c.source_port_id
      JOIN port t ON t.id = c.target_port_id JOIN element e ON e.id = s.element_id WHERE e.kind <> 'function'")
  claimed <- DBI::dbGetQuery(con, "SELECT source_id, target_id FROM link WHERE link_type = 'realized_by'")
  element_rows <- DBI::dbGetQuery(con, "SELECT id, parent_id FROM element")
  parents <- stats::setNames(element_rows$parent_id, element_rows$id)
  carries <- function(connector, from, to) {
    row <- physical[physical$id == connector, , drop = FALSE]
    if (nrow(row) == 0L) return(FALSE)
    any(outer(from, to, Vectorize(function(x, y) {
      (elements_related(x, row$a, parents) && elements_related(y, row$b, parents)) ||
        (elements_related(x, row$b, parents) && elements_related(y, row$a, parents))
    })))
  }
  empty <- data.frame(id = character(0), label = character(0), realised = logical(0), carried_by = character(0), problem = character(0), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(flows)), function(i) {
    flow <- flows[i, ]
    from <- allocations$target_id[allocations$source_id == flow$source_function]
    to <- allocations$target_id[allocations$source_id == flow$target_function]
    problem <- NA_character_
    carrier <- NA_character_
    named <- claimed$target_id[claimed$source_id == flow$id]
    if (length(from) == 0L || length(to) == 0L) {
      problem <- "one of its functions isn't allocated"
    } else if (length(named) > 0L) {
      if (carries(named[1], from, to)) carrier <- named[1] else problem <- sprintf("%s doesn't join the allocated components", named[1])
    } else {
      # Allocated to one component: nothing needs to cross a connector.
      if (any(outer(from, to, Vectorize(function(x, y) elements_related(x, y, parents))))) {
        carrier <- "same component"
      } else {
        found <- Filter(function(id) carries(id, from, to), physical$id)
        if (length(found) > 0L) carrier <- found[1] else problem <- "no physical connector joins the allocated components"
      }
    }
    data.frame(id = flow$id, label = if (is.na(flow$label)) "" else flow$label, realised = is.na(problem), carried_by = carrier, problem = problem, stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(empty), rows))
}

# --- Traceability ---

matrix_columns <- function(con) {
  root <- default_structure_root(con)
  if (is.na(root)) return(DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind = 'component' AND parent_id IS NULL ORDER BY sort_order, name"))
  DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind = 'component' AND parent_id = ? ORDER BY sort_order, name", params = list(root))
}

# Requirements × components, drafts included. A link to a sub-component counts for the
# column that holds it, and is marked as indirect, since it can't be removed from here.
trace_matrix <- function(con) {
  columns <- matrix_columns(con)
  rows <- DBI::dbGetQuery(con, "SELECT id, short_title, status, level, verification_method FROM requirement WHERE status <> 'deprecated' ORDER BY id")
  links <- DBI::dbGetQuery(con, "SELECT id, source_id, target_id FROM link WHERE link_type = 'satisfied_by' AND source_kind = 'requirement' AND target_kind = 'element'")
  element_rows <- DBI::dbGetQuery(con, "SELECT id, parent_id FROM element")
  parents <- stats::setNames(element_rows$parent_id, element_rows$id)
  column_of <- function(element) {
    current <- element
    while (length(current) == 1L && !is.na(current)) {
      if (current %in% columns$id) return(current)
      current <- parents[current]
    }
    NA_character_
  }
  links$column <- if (nrow(links) > 0L) vapply(links$target_id, column_of, "", USE.NAMES = FALSE) else character(0)
  cells <- list(link = links, direct = links[links$column == links$target_id & !is.na(links$column), , drop = FALSE])
  covered <- links$column[!is.na(links$column)]
  allocatable <- rows$status == "committed" & rows$level != "stakeholder"
  rows$unallocated <- allocatable & !rows$id %in% links$source_id
  columns$empty <- !columns$id %in% covered
  list(rows = rows, columns = columns, links = links, cells = cells,
       coverage = list(allocated = sum(allocatable & rows$id %in% links$source_id), allocatable = sum(allocatable),
                       components = sum(!columns$empty), component_total = nrow(columns),
                       verified = sum(allocatable & !is.na(rows$verification_method))))
}

# By hand a rationale is optional. From AI candidates it isn't (link_from_candidate).
link_requirement <- function(con, requirement_id, element_id, rationale = NULL, author = "engineer") {
  note <- if (is.null(rationale) || !nzchar(trimws(rationale))) NA_character_ else trimws(rationale)
  write_link(con, fields = list(link_type = "satisfied_by", source_kind = "requirement", source_id = requirement_id,
                                target_kind = "element", target_id = element_id, rationale = note), author = author)
}

allocate_function <- function(con, function_id, component_id, rationale = NULL, author = "engineer") {
  note <- if (is.null(rationale) || !nzchar(trimws(rationale))) NA_character_ else trimws(rationale)
  write_link(con, fields = list(link_type = "allocated_to", source_kind = "element", source_id = function_id,
                                target_kind = "element", target_id = component_id, rationale = note), author = author)
}

function_allocations <- function(con, function_id) {
  DBI::dbGetQuery(con, "SELECT l.id, e.id AS component_id, e.name, l.rationale FROM link l JOIN element e ON e.id = l.target_id
                         WHERE l.link_type = 'allocated_to' AND l.source_id = ? ORDER BY e.name", params = list(function_id))
}

allocation_matrix <- function(con) {
  columns <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind = 'component' AND review_state = 'reviewed' AND id NOT IN (
                                     SELECT coalesce(?, '')) ORDER BY sort_order, name", params = list(default_structure_root(con)))
  rows <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind = 'function' ORDER BY sort_order, id")
  links <- DBI::dbGetQuery(con, "SELECT id, source_id, target_id FROM link WHERE link_type = 'allocated_to' AND source_kind = 'element'")
  rows$unallocated <- !rows$id %in% links$source_id
  list(rows = rows, columns = columns, links = links)
}

realize_flow <- function(con, flow_id, connector_id, author = "engineer") {
  with_change_set(con, function(change_set) {
    old <- DBI::dbGetQuery(con, "SELECT id FROM link WHERE link_type = 'realized_by' AND source_id = ?", params = list(flow_id))$id
    for (id in old) delete_entity(con, "link", id, author, NULL, change_set)
    if (is.null(connector_id) || !nzchar(connector_id)) return(list(change_set = change_set, id = NA_character_, area = "model"))
    write_link(con, fields = list(link_type = "realized_by", source_kind = "connector", source_id = flow_id,
                                  target_kind = "connector", target_id = connector_id), author = author, change_set = change_set)
  })
}
