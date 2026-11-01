# The one write path. All mutations of a project's tables go through here
# (SCHEMA §6); a static test keeps it that way.

refuse <- function(...) {
  stop(structure(class = c("modelbench_refused", "error", "condition"),
                 list(message = paste0(...), call = NULL)))
}

# Joins an open transaction if there is one, so one user action stays atomic.
in_tx <- function(con, code) {
  if (RSQLite::sqliteIsTransacting(con)) code else DBI::dbWithTransaction(con, code)
}

allocate_id <- function(con, prefix) {
  format <- ID_FORMATS[prefix]
  if (is.na(format)) {
    custom <- DBI::dbGetQuery(con, "SELECT value FROM project_meta WHERE key LIKE 'id_prefix_%' AND value = ?",
                              params = list(prefix))
    if (nrow(custom) == 0L) stop("Unknown id prefix: ", prefix, call. = FALSE)
    format <- paste0(prefix, "-%03d")
  }
  in_tx(con, {
    DBI::dbExecute(con, "INSERT OR IGNORE INTO id_counter (prefix, next_num) VALUES (?, 1)", params = list(prefix))
    num <- DBI::dbGetQuery(con, "SELECT next_num FROM id_counter WHERE prefix = ?", params = list(prefix))$next_num
    DBI::dbExecute(con, "UPDATE id_counter SET next_num = next_num + 1 WHERE prefix = ?", params = list(prefix))
    sprintf(format, as.integer(num))
  })
}

new_change_set <- function(con) allocate_id(con, "CS")

# Runs `action(change_set)` in one transaction; every revision it writes shares the id.
with_change_set <- function(con, action) {
  in_tx(con, action(new_change_set(con)))
}

parent_chain_has <- function(con, kind, start, id) {
  sql <- sprintf("SELECT parent_id FROM %s WHERE id = ?", kind)
  current <- start
  seen <- character(0)
  while (length(current) == 1L && !is.na(current)) {
    if (current == id) return(TRUE)
    if (current %in% seen) return(TRUE)
    seen <- c(seen, current)
    current <- DBI::dbGetQuery(con, sql, params = list(current))$parent_id
  }
  FALSE
}

polymorphic_exists <- function(con, kind, id) {
  !is.null(read_entity(con, kind, id))
}

# Rules the database can't express (SCHEMA §5). `row` is the row as it would be.
validate_entity <- function(con, kind, id, row, existing = NULL) {
  # Commit re-runs the gate in core. A committed requirement whose text isn't
  # changing keeps its standing, so a later model edit can't lock its details.
  if (kind == "requirement" && identical(row$status, "committed") &&
      (is.null(existing) || !identical(existing$status, "committed") || !identical(existing$text, row$text))) {
    gate <- check_requirement(row$text, model_terms(con))
    if (!gate$passes_gate) {
      refuse("Can't commit ", id, ": ", paste(gate_blockers(gate), collapse = " "))
    }
  }
  if (kind %in% c("element", "requirement") && !is.null(row$parent_id) && !is.na(row$parent_id) &&
      parent_chain_has(con, kind, row$parent_id, id)) {
    refuse("Can't save ", id, ": its parent ", row$parent_id, " would make a cycle.")
  }
  if (kind == "connector") {
    check <- check_connector(con, row$source_port_id, row$target_port_id)
    if (!check$ok) refuse(check$reason)
  }
  if (kind == "property_value") {
    unit_problem <- check_value_unit(con, row$property_def_id, row$unit)
    if (!is.null(unit_problem)) refuse(unit_problem)
    if (!row$owner_kind %in% PROPERTY_OWNER_KINDS || !polymorphic_exists(con, row$owner_kind, row$owner_id)) {
      refuse("Can't save ", id, ": owner ", row$owner_kind, " ", row$owner_id, " doesn't exist.")
    }
  }
  if (kind == "link") {
    for (end in c("source", "target")) {
      end_kind <- row[[paste0(end, "_kind")]]
      end_id <- row[[paste0(end, "_id")]]
      if (!end_kind %in% LINK_END_KINDS || !polymorphic_exists(con, end_kind, end_id)) {
        refuse("Can't save ", id, ": ", end, " ", end_kind, " ", end_id, " doesn't exist.")
      }
    }
  }
  invisible(TRUE)
}

audit_actor <- function(author) if (author %in% c("ai", "system")) author else "engineer"

write_revision <- function(con, kind, id, row, author, note, change_set, action, deleted = 0L) {
  json <- canonical_json(row)
  DBI::dbExecute(con, "
    INSERT INTO revision (entity_kind, entity_id, rev, hash, data_json, deleted, changed_by, change_note, change_set)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(kind, id, row$rev, sha256(json), json, deleted, author,
                  if (is.null(note)) NA_character_ else note, change_set))
  DBI::dbExecute(con, "INSERT INTO audit_log (actor, action, entity_kind, entity_id, detail_json) VALUES (?, ?, ?, ?, ?)",
                 params = list(audit_actor(author), action, kind, id,
                               canonical_json(list(rev = row$rev, change_set = change_set))))
  invisible(NULL)
}

null_to_na <- function(fields) lapply(fields, function(x) if (is.null(x)) NA else x)

next_rev_after_delete <- function(con, kind, id) {
  last <- DBI::dbGetQuery(con, "SELECT max(rev) AS rev FROM revision WHERE entity_kind = ? AND entity_id = ?",
                          params = list(kind, id))$rev
  if (is.na(last)) 1L else as.integer(last) + 1L
}

# Creates or updates one entity and writes its revision and audit row in one
# transaction. Returns kind, id, rev, change_set and the UI area to invalidate.
write_entity <- function(con, kind, id = NULL, fields = list(), author = "engineer",
                         note = NULL, change_set = NULL) {
  check_kind(kind)
  columns <- DBI::dbListFields(con, kind)
  unknown <- setdiff(names(fields), columns)
  if (length(unknown) > 0L) stop("Unknown ", kind, " fields: ", paste(unknown, collapse = ", "), call. = FALSE)
  if ("rev" %in% names(fields)) stop("rev is set by the write path", call. = FALSE)
  fields <- null_to_na(fields)

  in_tx(con, {
    if (is.null(change_set)) change_set <- new_change_set(con)
    if (is.null(id)) id <- allocate_id(con, entity_prefix(con, kind, fields))
    key <- entity_key_values(kind, id)
    if (length(intersect(names(key), names(fields))) > 0L) stop("Key columns can't be changed", call. = FALSE)

    existing <- read_entity(con, kind, id)
    merged <- if (is.null(existing)) c(key, fields) else utils::modifyList(existing, fields, keep.null = TRUE)
    validate_entity(con, kind, id, merged, existing)

    if (is.null(existing)) {
      insert <- c(key, fields, list(rev = next_rev_after_delete(con, kind, id)))
      DBI::dbExecute(con, sprintf("INSERT INTO %s (%s) VALUES (%s)", quote_ident(con, kind),
                                  paste(vapply(names(insert), quote_ident, "", con = con), collapse = ", "),
                                  paste(rep("?", length(insert)), collapse = ", ")),
                     params = unname(insert))
    } else {
      sets <- sprintf("%s = ?", vapply(names(fields), quote_ident, "", con = con))
      sets <- c(sets, "rev = rev + 1")
      if ("updated_at" %in% columns && !"updated_at" %in% names(fields)) {
        sets <- c(sets, "updated_at = strftime('%Y-%m-%dT%H:%M:%SZ','now')")
      }
      where <- paste(sprintf("%s = ?", vapply(names(key), quote_ident, "", con = con)), collapse = " AND ")
      DBI::dbExecute(con, sprintf("UPDATE %s SET %s WHERE %s", quote_ident(con, kind), paste(sets, collapse = ", "), where),
                     params = unname(c(fields, key)))
    }

    row <- read_entity(con, kind, id)
    write_revision(con, kind, id, row, author, note, change_set,
                   if (is.null(existing)) "create" else "update")
    list(kind = kind, id = id, rev = row$rev, change_set = change_set, area = unname(ENTITY_AREAS[kind]))
  })
}

direct_dependents <- function(con, kind, id) {
  found <- list()
  for (child in ENTITY_CHILDREN[[kind]]) {
    ids <- DBI::dbGetQuery(con, sprintf("SELECT %s AS id FROM %s WHERE %s = ?", entity_id_sql(con, child[1]),
                                        quote_ident(con, child[1]), quote_ident(con, child[2])),
                           params = list(id))$id
    found <- c(found, lapply(ids, function(x) list(kind = child[1], id = x)))
  }
  if (kind %in% PROPERTY_OWNER_KINDS) {
    ids <- DBI::dbGetQuery(con, "SELECT id FROM property_value WHERE owner_kind = ? AND owner_id = ?",
                           params = list(kind, id))$id
    found <- c(found, lapply(ids, function(x) list(kind = "property_value", id = x)))
  }
  if (kind %in% LINK_END_KINDS) {
    ids <- DBI::dbGetQuery(con, paste("SELECT id FROM link WHERE (source_kind = ?1 AND source_id = ?2)",
                                     "OR (target_kind = ?1 AND target_id = ?2)"),
                           params = list(kind, id))$id
    found <- c(found, lapply(ids, function(x) list(kind = "link", id = x)))
  }
  found
}

# The entities that go when this one goes, deepest first, with no repeats.
entity_dependents <- function(con, kind, id) {
  state <- new.env()
  state$seen <- paste(kind, id)
  state$found <- list()
  visit <- function(parent_kind, parent_id) {
    for (child in direct_dependents(con, parent_kind, parent_id)) {
      tag <- paste(child$kind, child$id)
      if (tag %in% state$seen) next
      state$seen <- c(state$seen, tag)
      visit(child$kind, child$id)
      state$found <- c(state$found, list(child))
    }
  }
  visit(kind, id)
  state$found
}

delete_one <- function(con, kind, id, author, note, change_set) {
  row <- read_entity(con, kind, id)
  if (is.null(row)) return(invisible(NULL))
  row$rev <- row$rev + 1L
  write_revision(con, kind, id, row, author, note, change_set, "delete", deleted = 1L)
  key <- entity_key_values(kind, id)
  where <- paste(sprintf("%s = ?", vapply(names(key), quote_ident, "", con = con)), collapse = " AND ")
  DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE %s", quote_ident(con, kind), where), params = unname(key))
  invisible(NULL)
}

# Deletes an entity and its dependents, each with a final `deleted` revision.
delete_entity <- function(con, kind, id, author = "engineer", note = NULL, change_set = NULL) {
  check_kind(kind)
  if (is.null(read_entity(con, kind, id))) refuse("Can't delete ", kind, " ", id, ": it doesn't exist.")
  if (kind == "requirement") {
    held <- DBI::dbGetQuery(con, "SELECT id FROM sm_assertion WHERE requirement_id = ?", params = list(id))$id
    if (length(held) > 0L) refuse("Can't delete ", id, ": assertion ", held[1], " checks it.")
  }
  in_tx(con, {
    if (is.null(change_set)) change_set <- new_change_set(con)
    for (dependent in entity_dependents(con, kind, id)) {
      delete_one(con, dependent$kind, dependent$id, author, note, change_set)
    }
    delete_one(con, kind, id, author, note, change_set)
    list(kind = kind, id = id, change_set = change_set, area = unname(ENTITY_AREAS[kind]))
  })
}

entity_writer <- function(kind) {
  force(kind)
  function(con, id = NULL, fields = list(), author = "engineer", note = NULL, change_set = NULL) {
    write_entity(con, kind, id, fields, author, note, change_set)
  }
}

write_stereotype <- entity_writer("stereotype")
write_property_def <- entity_writer("property_def")
write_property_value <- entity_writer("property_value")
write_budget <- entity_writer("budget")
write_analysis_limit <- entity_writer("analysis_limit")
write_element <- entity_writer("element")
write_interface <- entity_writer("interface")
write_interface_field <- entity_writer("interface_field")
write_port <- entity_writer("port")
write_connector <- entity_writer("connector")
write_requirement <- entity_writer("requirement")
write_link <- entity_writer("link")
write_test_case <- entity_writer("test_case")
write_scenario <- entity_writer("scenario")
write_seq_lifeline <- entity_writer("seq_lifeline")
write_seq_step <- entity_writer("seq_step")
write_act_node <- entity_writer("act_node")
write_act_edge <- entity_writer("act_edge")
write_state_machine <- entity_writer("state_machine")
write_sm_state <- entity_writer("sm_state")
write_sm_event <- entity_writer("sm_event")
write_sm_event_scope <- entity_writer("sm_event_scope")
write_sm_variable <- entity_writer("sm_variable")
write_sm_transition <- entity_writer("sm_transition")
write_sm_ignore <- entity_writer("sm_ignore")
write_sm_assertion <- entity_writer("sm_assertion")

# Tables that aren't revisioned: settings, registries and AI records.
PLAIN_TABLES <- c("project_meta", "id_counter", "unit", "glossary", "llm_call", "ai_proposal")

write_plain <- function(con, table, fields) {
  if (!table %in% PLAIN_TABLES) stop(table, " isn't a plain table", call. = FALSE)
  fields <- null_to_na(fields)
  DBI::dbExecute(con, sprintf("INSERT INTO %s (%s) VALUES (%s)", quote_ident(con, table),
                              paste(vapply(names(fields), quote_ident, "", con = con), collapse = ", "),
                              paste(rep("?", length(fields)), collapse = ", ")),
                 params = unname(fields))
  invisible(NULL)
}

set_setting <- function(con, key, value) {
  DBI::dbExecute(con, "INSERT INTO project_meta (key, value) VALUES (?, ?)
                       ON CONFLICT (key) DO UPDATE SET value = excluded.value",
                 params = list(key, as.character(value)))
  invisible(NULL)
}

upsert_glossary <- function(con, term, definition = NULL, element_id = NULL) {
  DBI::dbExecute(con, "INSERT INTO glossary (term, definition, element_id) VALUES (?, ?, ?)
                       ON CONFLICT (term) DO UPDATE SET definition = excluded.definition, element_id = excluded.element_id",
                 params = null_to_na(list(term, definition, element_id)))
  invisible(NULL)
}

delete_glossary <- function(con, term) {
  DBI::dbExecute(con, "DELETE FROM glossary WHERE term = ?", params = list(term))
  invisible(NULL)
}

# Replaces how a requirement's terms resolved. Derived from the gate, so not revisioned.
write_requirement_terms <- function(con, requirement_id, terms) {
  DBI::dbExecute(con, "DELETE FROM requirement_term WHERE requirement_id = ?", params = list(requirement_id))
  for (i in seq_len(nrow(terms))) {
    DBI::dbExecute(con, "INSERT INTO requirement_term (requirement_id, term, role, resolution, element_id) VALUES (?, ?, ?, ?, ?)",
                   params = list(requirement_id, terms$term[i], terms$role[i], terms$resolution[i], terms$element_id[i]))
  }
  invisible(NULL)
}

insert_triage_row <- function(con, source_name, row_no, raw_text, raw_json, gate_json) {
  DBI::dbExecute(con, "INSERT INTO import_triage (source_name, row_no, raw_text, raw_json, gate_json) VALUES (?, ?, ?, ?, ?)",
                 params = list(source_name, row_no, raw_text, raw_json, gate_json))
  invisible(NULL)
}

update_triage_row <- function(con, id, status, requirement_id = NULL) {
  DBI::dbExecute(con, "UPDATE import_triage SET status = ?, requirement_id = ? WHERE id = ?",
                 params = null_to_na(list(status, requirement_id, id)))
  invisible(NULL)
}

upsert_analysis_result <- function(con, analysis, scope_id, value, unit, status, inputs_hash, detail_json) {
  DBI::dbExecute(con, "
    INSERT INTO analysis_result (analysis, scope_id, value, unit, status, inputs_hash, detail_json)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    ON CONFLICT (analysis, scope_id) DO UPDATE SET value = excluded.value, unit = excluded.unit, status = excluded.status,
      inputs_hash = excluded.inputs_hash, detail_json = excluded.detail_json,
      computed_at = strftime('%Y-%m-%dT%H:%M:%SZ','now')",
    params = null_to_na(list(analysis, scope_id, value, unit, status, inputs_hash, detail_json)))
  invisible(NULL)
}

# Positions aren't revisioned: moving a box isn't a model change (SCHEMA §4).
save_layout <- function(con, view_id, items) {
  for (i in seq_len(nrow(items))) {
    DBI::dbExecute(con, "
      INSERT INTO layout (view_id, item_kind, item_id, x, y, w, h, extra_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
      ON CONFLICT (view_id, item_kind, item_id) DO UPDATE SET x = excluded.x, y = excluded.y,
        w = coalesce(excluded.w, layout.w), h = coalesce(excluded.h, layout.h),
        extra_json = coalesce(excluded.extra_json, layout.extra_json)",
      params = list(view_id, items$item_kind[i], items$item_id[i], items$x[i], items$y[i],
                    items$w[i], items$h[i], items$extra_json[i]))
  }
  invisible(NULL)
}

delete_layout_item <- function(con, view_id, item_kind, item_id) {
  DBI::dbExecute(con, "DELETE FROM layout WHERE view_id = ? AND item_kind = ? AND item_id = ?",
                 params = list(view_id, item_kind, item_id))
  invisible(NULL)
}

log_llm_call <- function(con, fields) {
  fields <- null_to_na(fields)
  DBI::dbExecute(con, sprintf("INSERT INTO llm_call (%s) VALUES (%s)", paste(names(fields), collapse = ", "),
                              paste(rep("?", length(fields)), collapse = ", ")), params = unname(fields))
  DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
}

write_llm_cache <- function(con, task, key_hash, call_id) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO llm_cache (task, key_hash, llm_call_id) VALUES (?, ?, ?)",
                 params = list(task, key_hash, call_id))
  invisible(NULL)
}

insert_proposal <- function(con, fields) {
  id <- allocate_id(con, "PR")
  fields <- null_to_na(c(list(id = id), fields))
  DBI::dbExecute(con, sprintf("INSERT INTO ai_proposal (%s) VALUES (%s)", paste(names(fields), collapse = ", "),
                              paste(rep("?", length(fields)), collapse = ", ")), params = unname(fields))
  id
}

update_proposal <- function(con, id, fields) {
  fields <- null_to_na(c(fields, list(resolved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))))
  sets <- paste(sprintf("%s = ?", names(fields)), collapse = ", ")
  DBI::dbExecute(con, sprintf("UPDATE ai_proposal SET %s WHERE id = ?", sets), params = c(unname(fields), list(id)))
  invisible(NULL)
}

insert_conflict <- function(con, requirement_a, requirement_b, explanation, llm_call_id) {
  id <- allocate_id(con, "CF")
  DBI::dbExecute(con, "INSERT INTO requirement_conflict (id, requirement_a, requirement_b, explanation, llm_call_id) VALUES (?, ?, ?, ?, ?)",
                 params = null_to_na(list(id, requirement_a, requirement_b, explanation, llm_call_id)))
  id
}

log_ai_drop <- function(con, detail) {
  DBI::dbExecute(con, "INSERT INTO audit_log (actor, action, detail_json) VALUES ('system', 'ai_dropped', ?)",
                 params = list(canonical_json(detail)))
  invisible(NULL)
}

set_task_route <- function(con, task, fields) {
  fields <- null_to_na(fields)
  sets <- paste(sprintf("%s = ?", names(fields)), collapse = ", ")
  DBI::dbExecute(con, sprintf("UPDATE task_route SET %s WHERE task = ?", sets), params = c(unname(fields), list(task)))
  invisible(NULL)
}

set_provider_base_url <- function(con, provider_id, base_url) {
  DBI::dbExecute(con, "UPDATE provider SET base_url = ? WHERE id = ?", params = list(base_url, provider_id))
  invisible(NULL)
}

# Records evidence against the requirement's current context (SCHEMA §7).
write_verification_record <- function(con, requirement_id, test_case_id, result, recorded_by,
                                      evidence_ref = NULL, note = NULL, id = NULL) {
  in_tx(con, {
    if (is.null(id)) id <- allocate_id(con, "VR")
    context <- canonical_json(verification_context(con, requirement_id))
    DBI::dbExecute(con, "
      INSERT INTO verification_record
        (id, requirement_id, test_case_id, result, evidence_ref, context_json, context_hash, recorded_by, note)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = unname(null_to_na(list(id, requirement_id, test_case_id, result, evidence_ref,
                                      context, sha256(context), recorded_by, note))))
    id
  })
}

# Writes a baseline with no blocker check. Only the sample builder calls this
# directly, for a historical baseline; create_baseline() (Phase 5) checks
# blockers first (SCHEMA §6).
.write_baseline_items <- function(con, id, name, note, created_by, is_safety = 0L) {
  in_tx(con, {
    DBI::dbExecute(con, "INSERT INTO baseline (id, name, note, model_hash, is_safety, created_by) VALUES (?, ?, ?, ?, ?, ?)",
                   params = list(id, name, note, model_hash(con), is_safety, created_by))
    items <- baselineable_entities(con)
    for (i in seq_len(nrow(items))) {
      DBI::dbExecute(con, "
        INSERT INTO baseline_item (baseline_id, entity_kind, entity_id, rev, hash)
        SELECT ?, entity_kind, entity_id, rev, hash FROM revision
         WHERE entity_kind = ? AND entity_id = ? AND rev = ?",
        params = list(id, items$entity_kind[i], items$entity_id[i], items$rev[i]))
    }
    id
  })
}
