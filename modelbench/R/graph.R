# Impact traversal: deterministic, complete and repeatable (SCHEMA §8). The AI only rates
# what this finds.

node_id <- function(kind, id) paste(kind, id, sep = ":")

edge_rows <- function(from, to, both = FALSE) {
  if (length(from) == 0L) return(data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE))
  rows <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  if (both) rows <- rbind(rows, data.frame(from = to, to = from, stringsAsFactors = FALSE))
  rows
}

impact_edges <- function(con) {
  q <- function(sql) DBI::dbGetQuery(con, sql)
  store <- new.env()
  store$edges <- list()
  add <- function(...) store$edges[[length(store$edges) + 1L]] <- edge_rows(...)

  values <- q("SELECT v.id, v.owner_kind, v.owner_id, v.property_def_id, d.name, d.rollup FROM property_value v JOIN property_def d ON d.id = v.property_def_id")
  owned <- values[values$owner_kind %in% c("element", "connector"), , drop = FALSE]
  add(node_id("property_value", owned$id), node_id(owned$owner_kind, owned$owner_id))

  ends <- q("SELECT c.id, s.element_id AS a, t.element_id AS b, s.interface_id FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id")
  add(node_id("element", ends$a), node_id("element", ends$b), both = TRUE)
  add(node_id("connector", ends$id), node_id("element", ends$a))
  add(node_id("connector", ends$id), node_id("element", ends$b))

  links <- q("SELECT link_type, source_kind, source_id, target_kind, target_id FROM link WHERE link_type IN ('satisfied_by', 'allocated_to', 'derives_from', 'verified_by', 'refines')")
  add(node_id(links$source_kind, links$source_id), node_id(links$target_kind, links$target_id), both = TRUE)

  parents <- q("SELECT id, parent_id FROM requirement WHERE parent_id IS NOT NULL")
  add(node_id("requirement", parents$id), node_id("requirement", parents$parent_id), both = TRUE)

  # A roll-up reaches the budget at every ancestor.
  budgets <- q("SELECT b.id, b.property_def_id, b.scope_element_id, b.requirement_id FROM budget b")
  for (i in seq_len(nrow(budgets))) {
    tree <- scope_elements(con, budgets$scope_element_id[i])$id
    feeding <- values[values$property_def_id == budgets$property_def_id[i] & values$owner_kind == "element" & values$owner_id %in% tree, , drop = FALSE]
    add(node_id("property_value", feeding$id), rep(node_id("budget", budgets$id[i]), nrow(feeding)))
    if (!is.na(budgets$requirement_id[i])) add(node_id("budget", budgets$id[i]), node_id("requirement", budgets$requirement_id[i]))
  }

  # Analysis inputs reach the result, then the requirement judged by it.
  limits <- q("SELECT analysis, scope_id, requirement_id FROM analysis_limit")
  for (i in seq_len(nrow(limits))) {
    if (!limits$analysis[i] %in% registered_analyses()) next
    inputs <- analysis_input_entities(con, limits$analysis[i], limits$scope_id[i])
    node <- node_id("analysis", paste(limits$analysis[i], limits$scope_id[i], sep = "@"))
    add(node_id("property_value", inputs$id), rep(node, nrow(inputs)))
    if (!is.na(limits$requirement_id[i])) add(node, node_id("requirement", limits$requirement_id[i]))
  }

  fields <- q("SELECT id, interface_id, name FROM interface_field")
  add(node_id("interface_field", fields$id), node_id("interface", fields$interface_id))
  ports <- q("SELECT id, element_id, interface_id FROM port WHERE interface_id IS NOT NULL")
  add(node_id("interface", ports$interface_id), node_id("element", ports$element_id))
  steps <- q("SELECT DISTINCT scenario_id, interface_id FROM seq_step WHERE interface_id IS NOT NULL")
  add(node_id("interface", steps$interface_id), node_id("scenario", steps$scenario_id))
  events <- q("SELECT e.sm_id, e.interface_id FROM sm_event e WHERE e.interface_id IS NOT NULL")
  add(node_id("interface", events$interface_id), node_id("state_machine", events$sm_id))

  lifelines <- q("SELECT DISTINCT scenario_id, element_id FROM seq_lifeline")
  add(node_id("element", lifelines$element_id), node_id("scenario", lifelines$scenario_id), both = TRUE)
  performers <- q("SELECT DISTINCT scenario_id, performer_element_id AS element_id FROM act_node WHERE performer_element_id IS NOT NULL")
  add(node_id("element", performers$element_id), node_id("scenario", performers$scenario_id), both = TRUE)
  reads <- q("SELECT scenario_id, reads_json FROM act_node WHERE reads_json IS NOT NULL")
  for (i in seq_len(nrow(reads))) {
    for (entry in jsonlite::fromJSON(reads$reads_json[i], simplifyVector = FALSE)) {
      add(node_id(entry$owner_kind, entry$owner_id), node_id("scenario", reads$scenario_id[i]))
      read_values <- values$id[values$owner_kind == entry$owner_kind & values$owner_id == entry$owner_id & values$name == entry$property]
      add(node_id("property_value", read_values), rep(node_id("scenario", reads$scenario_id[i]), length(read_values)))
    }
  }

  machines <- q("SELECT id, owner_element_id FROM state_machine")
  add(node_id("element", machines$owner_element_id), node_id("state_machine", machines$id), both = TRUE)

  rows <- do.call(rbind, store$edges)
  rows <- unique(rows)
  rows[order(rows$from, rows$to, method = "radix"), , drop = FALSE]
}

impact_node_label <- function(con, node) {
  kind <- sub(":.*$", "", node)
  id <- sub("^[^:]*:", "", node)
  if (kind == "analysis") return(sprintf("%s analysis", term_to_title(sub("@.*$", "", id))))
  if (kind == "budget") {
    row <- DBI::dbGetQuery(con, "SELECT d.label FROM budget b JOIN property_def d ON d.id = b.property_def_id WHERE b.id = ?", params = list(id))
    return(if (nrow(row) == 1L) sprintf("%s budget", row$label) else id)
  }
  data <- read_entity(con, kind, id)
  if (is.null(data)) return(id)
  switch(kind,
         requirement = if (is.na(data$short_title)) id else sprintf("%s %s", id, data$short_title),
         test_case = sprintf("%s %s", id, data$name),
         property_value = entity_title(con, kind, id, data),
         connector = if (is.na(data$label)) id else data$label,
         if (!is.null(data$name)) data$name else id)
}

# Everything a change reaches within max_hops: one row each with kind, id, label, hops
# and the shortest path. Sources are entities as (kind, id) lists.
impact_set <- function(con, changes, max_hops = 3L) {
  empty <- data.frame(kind = character(0), id = character(0), label = character(0), hops = integer(0), path = character(0), stringsAsFactors = FALSE)
  edges <- impact_edges(con)
  sources <- unique(vapply(changes, function(change) node_id(change$kind, change$id), ""))
  vertices <- sort(unique(c(edges$from, edges$to, sources)), method = "radix")
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = data.frame(name = vertices, stringsAsFactors = FALSE))
  distances <- igraph::distances(graph, v = sources, to = vertices, mode = "out")
  best <- apply(distances, 2L, min)
  reached <- names(best)[is.finite(best) & best > 0 & best <= max_hops & !names(best) %in% sources]
  # Property values only matter as inputs; the things they lead to are the impact.
  reached <- reached[!grepl("^property_value:", reached)]
  if (length(reached) == 0L) return(empty)
  labels <- new.env()
  label_of <- function(node) {
    if (is.null(labels[[node]])) labels[[node]] <- impact_node_label(con, node)
    labels[[node]]
  }
  rows <- lapply(reached, function(node) {
    from <- sources[which.min(distances[, node])]
    path <- names(igraph::shortest_paths(graph, from = from, to = node, mode = "out", output = "vpath")$vpath[[1]])
    data.frame(kind = sub(":.*$", "", node), id = sub("^[^:]*:", "", node), label = label_of(node), hops = as.integer(best[[node]]),
               path = paste(vapply(path, label_of, ""), collapse = " → "), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$hops, out$kind, out$id, method = "radix"), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Impact of one compare card, with stale evidence on its test cases.
card_impact <- function(con, card) {
  sources <- lapply(card$entities, function(entity) list(kind = entity$kind, id = entity$id))
  items <- impact_set(con, sources)
  items$stale <- rep(NA_character_, nrow(items))
  stale <- stale_evidence(con)
  at <- match(items$id, stale$test_case_id)
  items$stale[items$kind == "test_case" & !is.na(at)] <- stale$text[at[items$kind == "test_case" & !is.na(at)]]
  items
}

# Scenarios a change reaches, for behavioural impact (Phase 6).
impacted_scenarios <- function(items) items[items$kind == "scenario", , drop = FALSE]
