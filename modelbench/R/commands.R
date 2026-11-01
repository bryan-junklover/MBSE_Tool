# Undo and redo. Undoing never rewrites history: it writes new revisions that restore
# the earlier state, and that new change set is what redo undoes (SCHEMA §6).

new_command_stack <- function() list(undo = list(), redo = list())

# Records a model edit or a layout move. A new command clears redo.
push_command <- function(stack, command) {
  list(undo = c(stack$undo, list(command)), redo = list())
}

change_command <- function(change_set, label = "edit") list(type = "changes", change_set = change_set, label = label)

layout_command <- function(view_id, before, after, label = "move") {
  list(type = "layout", view_id = view_id, before = before, after = after, label = label)
}

restorable_fields <- function(kind, data) {
  data[setdiff(names(data), c(entity_key_cols(kind), "rev", "created_at", "updated_at"))]
}

# Writes new revisions putting back the state from before a change set.
revert_change_set <- function(con, change_set, author = "engineer", note = "Undo") {
  revisions <- DBI::dbGetQuery(con, "SELECT rowid, entity_kind, entity_id, rev FROM revision WHERE change_set = ? ORDER BY rowid",
                               params = list(change_set))
  if (nrow(revisions) == 0L) stop("No change set ", change_set, call. = FALSE)
  firsts <- revisions[!duplicated(revisions[c("entity_kind", "entity_id")]), , drop = FALSE]
  with_change_set(con, function(new_set) {
    for (i in rev(seq_len(nrow(firsts)))) {
      kind <- firsts$entity_kind[i]
      id <- firsts$entity_id[i]
      before <- DBI::dbGetQuery(con, "SELECT data_json, deleted FROM revision WHERE entity_kind = ? AND entity_id = ? AND rev < ? ORDER BY rev DESC LIMIT 1",
                                params = list(kind, id, firsts$rev[i]))
      if (nrow(before) == 0L && firsts$rev[i] > 1L) refuse("Can't undo: ", id, " has no earlier revision.")
      if (nrow(before) == 0L || before$deleted == 1L) {
        if (!is.null(read_entity(con, kind, id))) delete_entity(con, kind, id, author, note, new_set)
      } else {
        data <- jsonlite::fromJSON(before$data_json, simplifyVector = FALSE)
        write_entity(con, kind, id, restorable_fields(kind, data), author, note, new_set)
      }
    }
    new_set
  })
}

layout_rows <- function(con, view_id, item_ids = NULL) {
  rows <- DBI::dbGetQuery(con, "SELECT item_kind, item_id, x, y, w, h, extra_json FROM layout WHERE view_id = ?", params = list(view_id))
  if (is.null(item_ids)) rows else rows[rows$item_id %in% item_ids, , drop = FALSE]
}

apply_layout_snapshot <- function(con, view_id, snapshot, other) {
  gone <- setdiff(other$item_id, snapshot$item_id)
  for (id in gone) delete_layout_item(con, view_id, other$item_kind[match(id, other$item_id)], id)
  save_layout(con, view_id, snapshot)
}

undo_command <- function(con, stack, author = "engineer") {
  if (length(stack$undo) == 0L) return(stack)
  command <- stack$undo[[length(stack$undo)]]
  if (command$type == "changes") {
    opposite <- change_command(revert_change_set(con, command$change_set, author, "Undo"), command$label)
  } else {
    apply_layout_snapshot(con, command$view_id, command$before, command$after)
    opposite <- layout_command(command$view_id, command$after, command$before, command$label)
  }
  list(undo = stack$undo[-length(stack$undo)], redo = c(stack$redo, list(opposite)))
}

redo_command <- function(con, stack, author = "engineer") {
  if (length(stack$redo) == 0L) return(stack)
  command <- stack$redo[[length(stack$redo)]]
  if (command$type == "changes") {
    opposite <- change_command(revert_change_set(con, command$change_set, author, "Redo"), command$label)
  } else {
    apply_layout_snapshot(con, command$view_id, command$before, command$after)
    opposite <- layout_command(command$view_id, command$after, command$before, command$label)
  }
  list(undo = c(stack$undo, list(opposite)), redo = stack$redo[-length(stack$redo)])
}
