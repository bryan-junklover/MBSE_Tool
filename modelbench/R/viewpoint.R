# Stakeholder viewpoints: read-only documents cut from the model for one group of readers
# (docs/VIEWPOINTS_AND_FAILURES.md §2). A viewpoint names the domains and flow types it
# covers and its sections in order; each section is a function that returns Markdown.

VIEWPOINT_SECTIONS <- c("summary", "breakdown", "interfaces", "budgets", "software", "signals", "states", "failures", "effects", "requirements", "open_items")

viewpoint_list <- function(con) {
  DBI::dbGetQuery(con, "SELECT id, name, stakeholder, concerns, rules_json, is_builtin FROM viewpoint ORDER BY sort_order, name")
}

read_viewpoint <- function(con, viewpoint_id) {
  row <- DBI::dbGetQuery(con, "SELECT * FROM viewpoint WHERE id = ?", params = list(viewpoint_id))
  if (nrow(row) == 0L) refuse("There's no viewpoint ", viewpoint_id, ".")
  rules <- jsonlite::fromJSON(row$rules_json, simplifyVector = TRUE)
  unknown <- setdiff(rules$sections, VIEWPOINT_SECTIONS)
  if (length(unknown) > 0L) refuse("Viewpoint ", viewpoint_id, " names a section that doesn't exist: ", paste(unknown, collapse = ", "), ".")
  list(id = row$id, name = row$name, stakeholder = row$stakeholder, concerns = row$concerns,
       domains = as.character(rules$domains), flow_types = as.character(rules$flow_types), sections = as.character(rules$sections),
       failure_causes = if (is.null(rules$failure_causes)) names(FAILURE_CAUSES) else as.character(rules$failure_causes))
}

# What a viewpoint covers: its components, the physical interfaces their ports use, and the
# state machines they own. The top-level system block is the document's subject, not a part.
viewpoint_scope <- function(con, viewpoint) {
  elements <- DBI::dbGetQuery(con, "SELECT id, name, kind, parent_id, domain, multiplicity, status, review_state, sort_order
                                      FROM element WHERE kind = 'component' ORDER BY sort_order, name")
  domain <- ifelse(is.na(elements$domain), "unset", elements$domain)
  root <- default_structure_root(con)
  components <- elements[domain %in% viewpoint$domains & !elements$id %in% root, , drop = FALSE]
  interfaces <- DBI::dbGetQuery(con, "SELECT DISTINCT i.* FROM interface i JOIN port p ON p.interface_id = i.id
                                       WHERE i.layer = 'physical' ORDER BY i.flow_type, i.name")
  ports <- DBI::dbGetQuery(con, "SELECT element_id, interface_id FROM port WHERE interface_id IS NOT NULL")
  used <- unique(ports$interface_id[ports$element_id %in% components$id])
  interfaces <- interfaces[interfaces$id %in% used & interfaces$flow_type %in% viewpoint$flow_types, , drop = FALSE]
  machines <- DBI::dbGetQuery(con, "SELECT sm.id, sm.name, sm.owner_element_id, sm.initial_state_id, e.name AS owner
                                      FROM state_machine sm JOIN element e ON e.id = sm.owner_element_id ORDER BY sm.id")
  list(all = elements, root = root, components = components, interfaces = interfaces,
       machines = machines[machines$owner_element_id %in% components$id, , drop = FALSE])
}

doc_section <- function(title, body) paste0("# ", title, "\n\n", body, "\n")
doc_bullets <- function(lines) if (length(lines) == 0L) "_None._" else paste0("- ", lines, collapse = "\n")
or_dash <- function(x) ifelse(is.na(x) | !nzchar(x), "—", x)

viewpoint_section_summary <- function(con, viewpoint, scope, source) {
  requirements <- viewpoint_requirements(con, scope)
  body <- paste0(
    sprintf("**For:** %s. %s\n\n", viewpoint$stakeholder, viewpoint$concerns),
    sprintf("**Source:** %s · model %s.\n\n", source, substr(model_hash(con), 1L, 19L)),
    sprintf("**Covers:** %d components, %d interfaces%s and %d requirements.\n\n",
            nrow(scope$components), nrow(scope$interfaces),
            if ("states" %in% viewpoint$sections) sprintf(", %d state machine%s", nrow(scope$machines), if (nrow(scope$machines) == 1L) "" else "s") else "",
            length(unique(requirements$id))),
    "This document is generated from the model and is read-only. Questions and changes go to the systems engineer, who owns the model.\n"
  )
  doc_section("About this document", body)
}

# Components as a nested list. Each sits under its nearest ancestor in scope, and software
# that runs on a part is listed under it.
viewpoint_section_breakdown <- function(con, viewpoint, scope, source) {
  components <- scope$components
  if (nrow(components) == 0L) return(doc_section("Physical breakdown", "_None._"))
  holder <- function(id) {
    parent <- scope$all$parent_id[match(id, scope$all$id)]
    while (!is.na(parent) && !parent %in% components$id) parent <- scope$all$parent_id[match(parent, scope$all$id)]
    parent
  }
  components$holder <- vapply(components$id, holder, "", USE.NAMES = FALSE)
  hosting <- DBI::dbGetQuery(con, "SELECT l.target_id AS host_id, e.id, e.name FROM link l JOIN element e ON e.id = l.source_id
                                    WHERE l.link_type = 'hosted_on' ORDER BY e.sort_order, e.name")
  walk <- function(parent, depth) {
    children <- components[if (is.na(parent)) is.na(components$holder) else components$holder %in% parent, , drop = FALSE]
    unlist(lapply(seq_len(nrow(children)), function(i) {
      row <- children[i, ]
      indent <- strrep("    ", depth)
      runs <- hosting[hosting$host_id == row$id & !hosting$id %in% components$id, , drop = FALSE]
      c(sprintf("%s- **%s** (%s)%s · %s%s", indent, row$name, row$id, if (row$multiplicity > 1L) sprintf(" × %d", row$multiplicity) else "",
                STATUS_LABELS[[row$status]], if (row$review_state == "unreviewed") " · unreviewed AI draft" else ""),
        if (nrow(runs) > 0L) sprintf("%s    - runs %s", indent, paste(sprintf("%s (%s)", runs$name, runs$id), collapse = ", ")),
        walk(row$id, depth + 1L))
    }))
  }
  doc_section("Physical breakdown", paste(walk(NA_character_, 0L), collapse = "\n"))
}

viewpoint_section_interfaces <- function(con, viewpoint, scope, source) {
  if (nrow(scope$interfaces) == 0L) return(doc_section("Interfaces", "_None._"))
  connections <- connection_rows(con)
  connections <- connections[connections$source_id %in% scope$components$id | connections$target_id %in% scope$components$id, , drop = FALSE]
  summary <- data.frame(scope$interfaces$id, scope$interfaces$name, scope$interfaces$flow_type,
                        vapply(seq_len(nrow(scope$interfaces)), function(i) interface_rate_text(scope$interfaces[i, ]), ""), scope$interfaces$transport)
  sections <- vapply(seq_len(nrow(scope$interfaces)), function(i) icd_interface_section(con, scope$interfaces[i, ], connections)$markdown, "")
  doc_section("Interfaces", paste0(md_table(c("ID", "Name", "Flow", "Rate", "Transport"), summary), "\n\n", paste(sections, collapse = "\n")))
}

viewpoint_section_budgets <- function(con, viewpoint, scope, source) {
  budgets <- budget_status(con)
  names <- scope$all$name[match(budgets$scope, scope$all$id)]
  budget_table <- md_table(c("Budget", "Scope", "Value / limit", "Result", "Note"),
                           data.frame(budgets$label, or_dash(names), budgets$text, ifelse(budgets$pass, "Within limit", "Over limit"), budgets$note))
  values <- DBI::dbGetQuery(con, "SELECT pv.owner_id, pd.label, pv.value_state, pv.num_value, pv.text_value, coalesce(pv.unit, pd.unit) AS unit
                                    FROM property_value pv JOIN property_def pd ON pd.id = pv.property_def_id
                                   WHERE pv.owner_kind = 'element'")
  values <- values[values$owner_id %in% scope$components$id, , drop = FALSE]
  values <- values[order(match(values$owner_id, scope$components$id), values$label), , drop = FALSE]
  shown <- vapply(seq_len(nrow(values)), function(i) {
    if (values$value_state[i] == "placeholder") return("[value]")
    if (is.na(values$num_value[i])) return(values$text_value[i])
    trimws(paste(format(values$num_value[i], trim = TRUE, digits = 15), if (is.na(values$unit[i])) "" else values$unit[i]))
  }, "")
  property_table <- md_table(c("Component", "Property", "Value"),
                             data.frame(scope$components$name[match(values$owner_id, scope$components$id)], values$label, shown))
  doc_section("Budgets and properties", paste0("## Budgets\n\n", budget_table, "\n\n## Properties by component\n\nValues are per unit; roll-ups multiply by how many.\n\n", property_table))
}

viewpoint_section_software <- function(con, viewpoint, scope, source) {
  software <- scope$components[scope$components$domain %in% c("sw", "hw_sw"), , drop = FALSE]
  if (nrow(software) == 0L) return(doc_section("Software", "_None._"))
  hosts <- DBI::dbGetQuery(con, "SELECT l.source_id, e.name FROM link l JOIN element e ON e.id = l.target_id WHERE l.link_type = 'hosted_on'")
  functions <- DBI::dbGetQuery(con, "SELECT l.target_id, f.name FROM link l JOIN element f ON f.id = l.source_id
                                      WHERE l.link_type = 'allocated_to' ORDER BY f.sort_order, f.name")
  runs_on <- ifelse(software$domain == "hw_sw", "Itself (hardware and software)",
                    or_dash(hosts$name[match(software$id, hosts$source_id)]))
  part_of <- scope$all$name[match(software$parent_id, scope$all$id)]
  allocated <- vapply(software$id, function(id) paste(functions$name[functions$target_id == id], collapse = ", "), "", USE.NAMES = FALSE)
  doc_section("Software", md_table(c("Component", "Runs on", "Part of", "Functions", "Status"),
                                   data.frame(sprintf("%s (%s)", software$name, software$id), runs_on, or_dash(part_of), or_dash(allocated),
                                              unname(STATUS_LABELS[software$status]))))
}

viewpoint_section_signals <- function(con, viewpoint, scope, source) {
  data <- scope$interfaces[scope$interfaces$flow_type == "data", , drop = FALSE]
  if (nrow(data) == 0L) return(doc_section("Signals", "_None._"))
  connections <- connection_rows(con)
  parts <- vapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]
    links <- connections[connections$interface_id == row$id, , drop = FALSE]
    fields <- interface_fields(con, row$id)
    ranges <- vapply(seq_len(nrow(fields)), function(j) {
      text <- format_field_range(fields$range_state[j], fields$min_value[j], fields$max_value[j])
      if (fields$range_state[j] == "placeholder") "[to be decided]" else text
    }, "")
    rate <- interface_rate_text(row)
    paste0(sprintf("## %s · %s\n\n", row$id, row$name),
           if (nrow(links) == 0L) "Not connected yet." else paste0("From ", paste(unique(sprintf("%s to %s", links$source, links$target)), collapse = "; "), "."),
           if (nzchar(rate)) sprintf(" Rate %s.", rate) else "", if (!is.na(row$transport)) sprintf(" Over %s.", row$transport) else "", "\n\n",
           md_table(c("Field", "Type", "Unit", "Range", "Notes"), data.frame(fields$name, fields$value_type, fields$unit, ranges, fields$notes)), "\n")
  }, "")
  doc_section("Signals", paste(parts, collapse = "\n"))
}

viewpoint_section_states <- function(con, viewpoint, scope, source) {
  if (nrow(scope$machines) == 0L) return(doc_section("States and transitions", "_None._"))
  parts <- vapply(seq_len(nrow(scope$machines)), function(i) {
    machine <- scope$machines[i, ]
    states <- DBI::dbGetQuery(con, "SELECT id, name, kind, entry_effect FROM sm_state WHERE sm_id = ? ORDER BY rowid", params = list(machine$id))
    transitions <- DBI::dbGetQuery(con, "
      SELECT f.name AS from_state, e.name AS event, t.guard_expr, t.hold_state, t.hold_s, s.name AS to_state, t.effect_expr
        FROM sm_transition t JOIN sm_state f ON f.id = t.from_state_id JOIN sm_state s ON s.id = t.to_state_id JOIN sm_event e ON e.id = t.event_id
       WHERE t.sm_id = ? ORDER BY f.rowid, t.priority DESC, e.name", params = list(machine$id))
    ignored <- DBI::dbGetQuery(con, "SELECT s.name AS state, e.name AS event, i.rationale FROM sm_ignore i
                                      JOIN sm_state s ON s.id = i.state_id JOIN sm_event e ON e.id = i.event_id
                                     WHERE s.sm_id = ? ORDER BY s.rowid, e.name", params = list(machine$id))
    state_notes <- ifelse(states$id %in% machine$initial_state_id, "initial", ifelse(states$kind == "final", "final", ""))
    hold <- ifelse(transitions$hold_state == "set", paste0(format(transitions$hold_s, trim = TRUE), " s"),
                   ifelse(transitions$hold_state == "placeholder", "[value]", ""))
    paste0(sprintf("## %s · %s (%s)\n\n", machine$id, machine$name, machine$owner),
           "### States\n\n", md_table(c("State", "", "On entry"), data.frame(states$name, state_notes, states$entry_effect)), "\n\n",
           "### Transitions\n\n", md_table(c("From", "Event", "Guard", "Held for", "To", "Effect"),
                                           data.frame(transitions$from_state, transitions$event, transitions$guard_expr, hold, transitions$to_state, transitions$effect_expr)), "\n\n",
           "### Events ignored on purpose\n\n", md_table(c("State", "Event", "Why"), data.frame(ignored$state, ignored$event, ignored$rationale)), "\n")
  }, "")
  doc_section("States and transitions", paste(parts, collapse = "\n"))
}

# Failure modes inside a viewpoint: about its parts, interfaces or (when it shows states)
# its state machines, with a cause the viewpoint covers.
viewpoint_failures <- function(con, viewpoint, scope) {
  modes <- failure_mode_list(con)
  ports <- DBI::dbGetQuery(con, "SELECT id, element_id FROM port")
  states <- DBI::dbGetQuery(con, "SELECT id, sm_id FROM sm_state")
  inside <- vapply(seq_len(nrow(modes)), function(i) {
    id <- modes$subject_id[i]
    switch(modes$subject_kind[i],
           element = id %in% scope$components$id,
           port = ports$element_id[match(id, ports$id)] %in% scope$components$id,
           interface = id %in% scope$interfaces$id,
           sm_state = "states" %in% viewpoint$sections && states$sm_id[match(id, states$id)] %in% scope$machines$id,
           FALSE)
  }, TRUE)
  modes[inside & modes$cause_kind %in% viewpoint$failure_causes, , drop = FALSE]
}

viewpoint_section_failures <- function(con, viewpoint, scope, source) {
  modes <- viewpoint_failures(con, viewpoint, scope)
  if (nrow(modes) == 0L) return(doc_section("Failure modes", "_None recorded in this part of the model._"))
  join <- function(a, b) ifelse(is.na(a) & is.na(b), "—", ifelse(is.na(b), a, ifelse(is.na(a), b, paste(a, "→", b))))
  response <- vapply(seq_len(nrow(modes)), function(i) {
    parts <- c(sprintf("**%s**", FAILURE_RESPONSES[[modes$response_state[i]]]),
               if (!is.na(modes$response_note[i])) modes$response_note[i],
               if (nzchar(modes$handled_by[i])) sprintf("(%s)", modes$handled_by[i]))
    paste(parts, collapse = " ")
  }, "")
  cause <- ifelse(is.na(modes$cause), FAILURE_CAUSES[modes$cause_kind], paste0(FAILURE_CAUSES[modes$cause_kind], " — ", modes$cause))
  body <- paste0("Effects read nearby → system. A response that isn't decided is an open item.\n\n",
                 md_table(c("ID", "What fails", "Cause", "Effect", "Detected by", "Response"),
                          data.frame(modes$id, modes$subject, unname(cause), join(modes$local_effect, modes$system_effect), or_dash(modes$detected_by), response)))
  doc_section("Failure modes", body)
}

# What each failure in the viewpoint leads to. Chains are shown whole: a power loss that
# stops software is exactly what a hardware reader needs to see.
viewpoint_section_effects <- function(con, viewpoint, scope, source) {
  modes <- viewpoint_failures(con, viewpoint, scope)
  modes <- modes[modes$subject_kind != "sm_state", , drop = FALSE]
  if (nrow(modes) == 0L) return(doc_section("Effect chains", "_None recorded in this part of the model._"))
  model <- effect_model(con)
  parts <- vapply(seq_len(nrow(modes)), function(i) {
    chain <- effect_chain(con, modes$id[i], model)
    sprintf("## %s · %s %s\n\n%s\n", modes$id[i], modes$subject[i], tolower(FAILURE_CAUSES[[modes$cause_kind[i]]]), effect_chain_markdown(chain))
  }, "")
  doc_section("Effect chains", paste0("Worked out from the model: → follows on, ✓ is handled or accepted, ⚠ is a loss nobody has decided about.\n\n",
                                      paste(parts, collapse = "\n")))
}

# Failure checks that land inside a viewpoint.
viewpoint_failure_findings <- function(con, viewpoint, scope) {
  findings <- failure_findings(con)
  if (nrow(findings) == 0L) return(character(0))
  shown <- viewpoint_failures(con, viewpoint, scope)$id
  ports <- DBI::dbGetQuery(con, "SELECT id, element_id FROM port")
  fields <- DBI::dbGetQuery(con, "SELECT id, interface_id FROM interface_field")
  # A missing-case check shows where its cause would. Signal loss is about software that
  # receives data, so it goes with "stale", which only software viewpoints cover.
  rule_cause <- c(missing_signal_loss = "stale", missing_power_loss = "lost", missing_over_temperature = "over_temperature", missing_out_of_range = "out_of_range")
  keep <- vapply(seq_len(nrow(findings)), function(i) {
    row <- findings[i, ]
    if (row$subject_kind == "failure_mode") return(row$subject_id %in% shown)
    if (!rule_cause[[row$rule]] %in% viewpoint$failure_causes) return(FALSE)
    switch(row$subject_kind,
           port = ports$element_id[match(row$subject_id, ports$id)] %in% scope$components$id,
           element = row$subject_id %in% scope$components$id,
           interface_field = fields$interface_id[match(row$subject_id, fields$id)] %in% scope$interfaces$id,
           FALSE)
  }, TRUE)
  findings$text[keep]
}

viewpoint_requirements <- function(con, scope) {
  rows <- DBI::dbGetQuery(con, "SELECT r.id, r.short_title, r.text, r.verification_method, l.target_id
                                  FROM requirement r JOIN link l ON l.link_type = 'satisfied_by' AND l.source_kind = 'requirement' AND l.source_id = r.id
                                 WHERE r.status = 'committed' ORDER BY r.id")
  rows[rows$target_id %in% scope$components$id, , drop = FALSE]
}

viewpoint_section_requirements <- function(con, viewpoint, scope, source) {
  rows <- viewpoint_requirements(con, scope)
  if (nrow(rows) == 0L) return(doc_section("Requirements", "_None allocated to these components._"))
  ids <- unique(rows$id)
  first <- rows[match(ids, rows$id), , drop = FALSE]
  allocated <- vapply(ids, function(id) paste(scope$components$name[match(rows$target_id[rows$id == id], scope$components$id)], collapse = ", "), "", USE.NAMES = FALSE)
  # Test cases and state machine assertions, each with its latest result.
  result <- vapply(ids, function(id) {
    evidence <- requirement_evidence(con, id)
    tests <- ifelse(is.na(evidence$result), sprintf("%s not run", evidence$id),
                    paste0(sprintf("%s %s", evidence$id, toupper(evidence$result)), ifelse(is.na(evidence$stale), "", " (stale)")))
    assertions <- DBI::dbGetQuery(con, "
      SELECT a.id, (SELECT r.result FROM sm_run r WHERE r.assertion_id = a.id ORDER BY r.id DESC LIMIT 1) AS result
        FROM link l JOIN sm_assertion a ON a.id = l.target_id
       WHERE l.link_type = 'verified_by' AND l.source_kind = 'requirement' AND l.source_id = ? AND l.target_kind = 'sm_assertion'
       ORDER BY a.id", params = list(id))
    checks <- ifelse(is.na(assertions$result), sprintf("%s not run", assertions$id), sprintf("%s %s", assertions$id, toupper(assertions$result)))
    if (length(c(tests, checks)) == 0L) "Nothing linked yet" else paste(c(tests, checks), collapse = ", ")
  }, "", USE.NAMES = FALSE)
  doc_section("Requirements", md_table(c("ID", "Requirement", "Allocated to", "Verified by", "Latest result"),
                                       data.frame(ids, first$text, allocated, or_dash(first$verification_method), result)))
}

# Gaps and placeholders inside the viewpoint's scope, so each reader sees what's still open
# in their part of the model.
viewpoint_open_items <- function(con, viewpoint, scope) {
  in_scope <- scope$components
  placeholders <- DBI::dbGetQuery(con, "SELECT v.entity_kind, v.owner_kind, v.owner_id, coalesce(d.label, v.item) AS item FROM v_placeholders v
                                         LEFT JOIN property_value pv ON v.entity_kind = 'property_value' AND pv.id = v.entity_id
                                         LEFT JOIN property_def d ON d.id = pv.property_def_id")
  name_of <- function(id) scope$all$name[match(id, scope$all$id)]
  items <- c(
    with(placeholders[placeholders$owner_kind == "element" & placeholders$owner_id %in% in_scope$id, , drop = FALSE],
         sprintf("%s: %s is a placeholder.", name_of(owner_id), item)),
    unlist(lapply(seq_len(nrow(scope$interfaces)), function(i) {
      icd_interface_section(con, scope$interfaces[i, ], connection_rows(con))$open_items
    })),
    with(DBI::dbGetQuery(con, "SELECT element_id, name FROM v_unconnected_ports"),
         sprintf("%s: port %s has no connector.", name_of(element_id[element_id %in% in_scope$id]), name[element_id %in% in_scope$id])),
    with(DBI::dbGetQuery(con, "SELECT id, name FROM v_unhosted_software"),
         sprintf("%s: nothing is recorded for it to run on.", name[id %in% in_scope$id])),
    sprintf("%s: domain not set, so it's unclear whose document it belongs in.", in_scope$name[is.na(in_scope$domain)]),
    sprintf("%s is an unreviewed AI draft.", in_scope$name[in_scope$review_state == "unreviewed"]),
    if ("states" %in% viewpoint$sections) {
      unhandled <- DBI::dbGetQuery(con, "SELECT sm_id, state_name, event_name FROM v_sm_unhandled")
      unhandled <- unhandled[unhandled$sm_id %in% scope$machines$id, , drop = FALSE]
      c(sprintf("%s: %s isn't handled in %s.", scope$machines$name[match(unhandled$sm_id, scope$machines$id)], unhandled$event_name, unhandled$state_name),
        with(placeholders[placeholders$entity_kind == "sm_transition" & placeholders$owner_id %in% scope$machines$id, , drop = FALSE],
             sprintf("%s: %s time is a placeholder.", scope$machines$name[match(owner_id, scope$machines$id)], sub(" hold$", "", item))))
    },
    if ("failures" %in% viewpoint$sections) viewpoint_failure_findings(con, viewpoint, scope),
    if ("budgets" %in% viewpoint$sections) {
      budgets <- budget_status(con)
      sprintf("%s budget is over its limit: %s.", budgets$label[!budgets$pass], budgets$text[!budgets$pass])
    }
  )
  unique(items)
}

viewpoint_section_open_items <- function(con, viewpoint, scope, source) {
  doc_section("Open items", doc_bullets(viewpoint_open_items(con, viewpoint, scope)))
}

# The document's Markdown. `source` labels where the model came from.
viewpoint_markdown <- function(con, viewpoint_id, source = "the working copy") {
  viewpoint <- read_viewpoint(con, viewpoint_id)
  scope <- viewpoint_scope(con, viewpoint)
  parts <- vapply(viewpoint$sections, function(section) {
    get(paste0("viewpoint_section_", section), mode = "function")(con, viewpoint, scope, source)
  }, "", USE.NAMES = FALSE)
  paste(parts, collapse = "\n")
}

viewpoint_title <- function(con, viewpoint_id) {
  sprintf("%s: %s view", get_setting(con, "project_name", "System"), read_viewpoint(con, viewpoint_id)$name)
}

# Runs `code(con)` against the model as it was at a baseline. The project is copied and the
# copy rolled back, so the working copy is never touched.
with_baseline_model <- function(con, baseline_id, code) {
  baseline <- DBI::dbGetQuery(con, "SELECT id, name, created_at FROM baseline WHERE id = ?", params = list(baseline_id))
  if (nrow(baseline) == 0L) refuse("There's no baseline ", baseline_id, ".")
  path <- tempfile(fileext = ".sqlite")
  copy <- open_project_db(path)
  on.exit({
    DBI::dbDisconnect(copy)
    unlink(paste0(path, c("", "-wal", "-shm")))
  }, add = TRUE)
  RSQLite::sqliteCopyDatabase(con, copy)
  rollback_to_baseline(copy, baseline_id)
  code(copy, sprintf("baseline %s, %s (%s)", baseline$id, baseline$name, substr(baseline$created_at, 1L, 10L)))
}

# The document for a viewpoint, as Markdown, from the working copy or a baseline.
viewpoint_document <- function(con, viewpoint_id, baseline_id = NULL) {
  if (is.null(baseline_id) || !nzchar(baseline_id) || identical(baseline_id, "working")) {
    return(viewpoint_markdown(con, viewpoint_id, sprintf("the working copy, %s", format(Sys.time(), "%Y-%m-%d %H:%M"))))
  }
  with_baseline_model(con, baseline_id, function(model, source) viewpoint_markdown(model, viewpoint_id, source))
}

render_viewpoint <- function(con, viewpoint_id, path, format = c("html", "pdf"), baseline_id = NULL) {
  viewpoint <- read_viewpoint(con, viewpoint_id)
  render_quarto_document(viewpoint_document(con, viewpoint_id, baseline_id), viewpoint_title(con, viewpoint_id),
                         path, format, sprintf("The %s document", viewpoint$name))
}
