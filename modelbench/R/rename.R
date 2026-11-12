# Some things are referred to by name rather than id: an interface field by its name, a
# component inside prop() in guards and effects, and an interface in a connector's label.
# A rename carries through to those, in the same change set, so Undo reverses all of it.

carry_rename <- function(con, kind, row, old, author, change_set) {
  new <- row$name
  note <- sprintf("Follows the rename of %s to %s", old, new)
  rewrite <- function(target, ids, fields) for (id in ids) write_entity(con, target, id, fields, author, note, change_set)
  if (kind == "interface_field") {
    named <- function(table, interface_column, extra = "") {
      DBI::dbGetQuery(con, sprintf("SELECT id FROM %s WHERE %s = ? AND field_name = ?%s", table, interface_column, extra),
                      params = list(row$interface_id, old))$id
    }
    rewrite("failure_mode", named("failure_mode", "subject_id", " AND subject_kind = 'interface'"), list(field_name = new))
    rewrite("seq_step", named("seq_step", "interface_id"), list(field_name = new))
    rewrite("sm_event", named("sm_event", "interface_id"), list(field_name = new))
  }
  if (kind == "element") {
    from <- sprintf("prop(%s.", old)
    to <- sprintf("prop(%s.", new)
    swap <- function(x) if (is.na(x)) x else gsub(from, to, x, fixed = TRUE)
    transitions <- DBI::dbGetQuery(con, "SELECT id, guard_expr, effect_expr FROM sm_transition WHERE instr(guard_expr, ?) > 0 OR instr(effect_expr, ?) > 0",
                                   params = list(from, from))
    for (i in seq_len(nrow(transitions))) {
      rewrite("sm_transition", transitions$id[i], list(guard_expr = swap(transitions$guard_expr[i]), effect_expr = swap(transitions$effect_expr[i])))
    }
    states <- DBI::dbGetQuery(con, "SELECT id, entry_effect FROM sm_state WHERE instr(entry_effect, ?) > 0", params = list(from))
    for (i in seq_len(nrow(states))) rewrite("sm_state", states$id[i], list(entry_effect = swap(states$entry_effect[i])))
  }
  if (kind == "interface") {
    labelled <- DBI::dbGetQuery(con, "SELECT DISTINCT c.id FROM connector c JOIN port p ON p.id = c.source_port_id
                                       WHERE p.interface_id = ? AND c.label = ?", params = list(row$id, old))$id
    rewrite("connector", labelled, list(label = new))
  }
  invisible(NULL)
}

# References by field name to fields being deleted, cleared rather than left dangling.
field_name_clears <- function(con, field_ids) {
  updates <- list()
  for (field_id in field_ids) {
    field <- read_entity(con, "interface_field", field_id)
    if (is.null(field)) next
    for (spec in list(c("failure_mode", "subject_id", " AND subject_kind = 'interface'"), c("seq_step", "interface_id", ""), c("sm_event", "interface_id", ""))) {
      ids <- DBI::dbGetQuery(con, sprintf("SELECT id FROM %s WHERE %s = ? AND field_name = ?%s", spec[1], spec[2], spec[3]),
                             params = list(field$interface_id, field$name))$id
      for (id in ids) updates[[length(updates) + 1L]] <- list(kind = spec[1], id = id, fields = list(field_name = NA_character_))
    }
  }
  updates
}
