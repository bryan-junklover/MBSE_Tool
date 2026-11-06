# Baselines, compare and rollback (SCHEMA §6).

next_baseline_id <- function(con) {
  num <- DBI::dbGetQuery(con, "SELECT next_num FROM id_counter WHERE prefix = 'BL'")$next_num
  sprintf(ID_FORMATS[["BL"]], if (length(num) == 1L) as.integer(num) else 1L)
}

# Everything stopping a baseline, one row each.
baseline_blockers <- function(con) {
  detail <- baseline_blockers_detail(con)
  rbind(
    data.frame(group = rep("placeholder", nrow(detail$placeholders)), id = detail$placeholders$entity_id, text = detail$placeholders$text, stringsAsFactors = FALSE),
    data.frame(group = rep("unreviewed", nrow(detail$unreviewed)), id = detail$unreviewed$entity_id, text = detail$unreviewed$label, stringsAsFactors = FALSE),
    data.frame(group = rep("stale", nrow(detail$stale)), id = detail$stale$id, text = detail$stale$text, stringsAsFactors = FALSE)
  )
}

# What doesn't block a baseline but should be seen before taking one. Each row is a kind
# and a line in words. The list is written into the baseline's note when it's taken.
baseline_warnings <- function(con) {
  rows <- list()
  add <- function(kind, text) if (length(text) > 0L) rows[[length(rows) + 1L]] <<- data.frame(kind = kind, text = text)
  budgets <- budget_status(con)
  add("budget", sprintf("%s over %s is over its limit: %s.", budgets$label[!budgets$pass], budgets$scope_name[!budgets$pass], budgets$text[!budgets$pass]))
  incomplete <- !is.na(budgets$note) & grepl("^excludes", budgets$note)
  add("budget", sprintf("%s over %s leaves out values still to be set (%s).", budgets$label[incomplete], budgets$scope_name[incomplete], budgets$note[incomplete]))
  analyses <- analysis_status(con)
  missed <- analyses[!analyses$pass, , drop = FALSE]
  add("analysis", vapply(seq_len(nrow(missed)), function(i) sprintf("%s misses its limit: %s.", term_to_title(missed$analysis[i]), missed$text[i]), ""))
  for (sm in DBI::dbGetQuery(con, "SELECT id FROM state_machine ORDER BY id")$id) {
    checks <- live_assertions(con, sm)
    add("assertion", sprintf("Assertion %s fails: %s", checks$id[checks$live_result != "pass"], checks$live_message[checks$live_result != "pass"]))
    pairs <- unhandled_pairs(con, sm)
    if (nrow(pairs) > 0L) add("state_machine", sprintf("%s has %d event/state pair%s nothing handles.", read_entity(con, "state_machine", sm)$name, nrow(pairs), if (nrow(pairs) == 1L) "" else "s"))
  }
  findings <- failure_findings(con)
  open <- findings$rule %in% c("open_response", "handled_unlinked", "handled_unverified")
  add("failure", findings$text[open])
  unallocated <- DBI::dbGetQuery(con, "SELECT id FROM v_unallocated_requirements ORDER BY id")$id
  if (length(unallocated) > 0L) add("requirement", sprintf("%d committed requirement%s %s allocated to nothing: %s.", length(unallocated),
                                                         if (length(unallocated) == 1L) "" else "s", if (length(unallocated) == 1L) "is" else "are", paste(unallocated, collapse = ", ")))
  do.call(rbind, c(list(data.frame(kind = character(0), text = character(0))), rows))
}

# Refused while blockers exist: returns them, and writes nothing.
# Warnings don't stop it; they're recorded in the note as what was known when it was taken.
create_baseline <- function(con, name, note = NULL, author = "engineer", warnings = baseline_warnings(con)) {
  blockers <- baseline_blockers(con)
  if (nrow(blockers) > 0L) return(list(ok = FALSE, blockers = blockers))
  if (nrow(warnings) > 0L) {
    known <- paste(c(sprintf("Taken with %d known issue%s:", nrow(warnings), if (nrow(warnings) == 1L) "" else "s"), paste0("- ", warnings$text)), collapse = "\n")
    note <- if (is.null(note) || !nzchar(trimws(note))) known else paste(trimws(note), known, sep = "\n\n")
  }
  in_tx(con, {
    id <- allocate_id(con, "BL")
    .write_baseline_items(con, id, name, if (is.null(note)) NA_character_ else note, author)
    list(ok = TRUE, id = id, area = "baseline")
  })
}

baseline_list <- function(con) DBI::dbGetQuery(con, "SELECT id, name, note, is_safety, created_at FROM baseline ORDER BY created_at DESC, id DESC")

# The latest proper baseline, which Compare starts from.
latest_baseline <- function(con) {
  rows <- baseline_list(con)
  rows <- rows[rows$is_safety == 0L, , drop = FALSE]
  if (nrow(rows) == 0L) NA_character_ else rows$id[1]
}

compare_side <- function(con, side) {
  if (identical(side, "working")) return(baselineable_entities(con))
  DBI::dbGetQuery(con, "SELECT entity_kind, entity_id, rev FROM baseline_item WHERE baseline_id = ?", params = list(side))
}

VOLATILE_FIELDS <- c("rev", "created_at", "updated_at")

revision_data <- function(con, kind, id, rev) {
  json <- DBI::dbGetQuery(con, "SELECT data_json FROM revision WHERE entity_kind = ? AND entity_id = ? AND rev = ?", params = list(kind, id, rev))$data_json
  if (length(json) == 0L) return(NULL)
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

field_diff <- function(before, after) {
  fields <- setdiff(union(names(before), names(after)), VOLATILE_FIELDS)
  changed <- Filter(function(field) !identical(before[[field]], after[[field]]), fields)
  lapply(changed, function(field) list(field = field, before = before[[field]], after = after[[field]]))
}

# A name a person would recognise for a changed entity.
entity_title <- function(con, kind, id, data) {
  switch(
    kind,
    property_value = {
      label <- DBI::dbGetQuery(con, "SELECT label FROM property_def WHERE id = ?", params = list(data$property_def_id))$label
      owner <- if (identical(data$owner_kind, "element")) DBI::dbGetQuery(con, "SELECT name FROM element WHERE id = ?", params = list(data$owner_id))$name else data$owner_id
      sprintf("%s · %s", if (length(owner) == 1L) owner else data$owner_id, if (length(label) == 1L) label else id)
    },
    requirement = if (is.null(data$short_title)) id else sprintf("%s · %s", id, data$short_title),
    interface_field = {
      interface <- DBI::dbGetQuery(con, "SELECT name FROM interface WHERE id = ?", params = list(data$interface_id))$name
      sprintf("%s · %s", if (length(interface) == 1L) interface else data$interface_id, data$name)
    },
    if (!is.null(data$name)) data$name else if (!is.null(data$label)) data$label else id
  )
}

# "5000 → 4200 mAh", in words of the fields that matter.
entity_change_text <- function(kind, status, data, diff) {
  if (status == "added") {
    if (kind == "interface_field") return(paste(c(data$value_type, data$unit), collapse = ", "))
    return("added")
  }
  if (status == "removed") return("removed")
  value_diff <- Filter(function(d) d$field %in% c("num_value", "text_value", "value_state"), diff)
  if (kind == "property_value" && length(value_diff) > 0L) {
    first <- value_diff[[1]]
    show <- function(x) if (is.null(x)) "[value]" else as.character(x)
    return(sprintf("%s → %s%s", show(first$before), show(first$after), if (is.null(data$unit)) "" else paste0(" ", data$unit)))
  }
  if (length(diff) == 0L) return("no field changes")
  text_diff <- Filter(function(d) d$field == "text", diff)
  if (kind == "requirement" && length(text_diff) == 1L) {
    words <- word_diff(text_diff[[1]]$before, text_diff[[1]]$after)
    # The words either side of a change read better with what doesn't change beside them.
    changed <- which(words$op != "keep")
    around <- sort(unique(c(changed, changed + 1L)))
    around <- around[around <= nrow(words) & (words$op[around] != "keep" | around %in% (changed + 1L))]
    before <- words$word[around][words$op[around] != "ins"]
    after <- words$word[around][words$op[around] != "del"]
    return(sprintf("%s → %s", sub("[.,;]$", "", paste(before, collapse = " ")), sub("[.,;]$", "", paste(after, collapse = " "))))
  }
  paste(vapply(diff, function(d) d$field, ""), collapse = ", ")
}

# Entity changes between two sides, as rows with their field diffs and change sets.
compare_entities <- function(con, a, b = "working") {
  left <- compare_side(con, a)
  right <- compare_side(con, b)
  merged <- merge(left, right, by = c("entity_kind", "entity_id"), all = TRUE, suffixes = c("_a", "_b"))
  merged <- merged[is.na(merged$rev_a) | is.na(merged$rev_b) | merged$rev_a != merged$rev_b, , drop = FALSE]
  if (nrow(merged) == 0L) return(list())
  lapply(seq_len(nrow(merged)), function(i) {
    row <- merged[i, ]
    status <- if (is.na(row$rev_a)) "added" else if (is.na(row$rev_b)) "removed" else "modified"
    before <- if (is.na(row$rev_a)) NULL else revision_data(con, row$entity_kind, row$entity_id, row$rev_a)
    after <- if (is.na(row$rev_b)) NULL else revision_data(con, row$entity_kind, row$entity_id, row$rev_b)
    data <- if (is.null(after)) before else after
    rev_for_set <- if (is.na(row$rev_b)) {
      DBI::dbGetQuery(con, "SELECT max(rev) AS rev FROM revision WHERE entity_kind = ? AND entity_id = ?", params = list(row$entity_kind, row$entity_id))$rev
    } else {
      row$rev_b
    }
    set <- DBI::dbGetQuery(con, "SELECT change_set, change_note, changed_by, changed_at, rowid FROM revision WHERE entity_kind = ? AND entity_id = ? AND rev = ?",
                           params = list(row$entity_kind, row$entity_id, rev_for_set))
    diff <- if (status == "modified") field_diff(before, after) else list()
    list(kind = row$entity_kind, id = row$entity_id, status = status, rev_a = row$rev_a, rev_b = row$rev_b, change_set = set$change_set, note = set$change_note,
         changed_by = set$changed_by, changed_at = set$changed_at, order = set$rowid, diff = diff, title = entity_title(con, row$entity_kind, row$entity_id, data),
         change_text = entity_change_text(row$entity_kind, status, data, diff))
  })
}

# One card per change set, titled by its first entity (SCHEMA §6).
compare_baselines <- function(con, a, b = "working") {
  entities <- compare_entities(con, a, b)
  # A revision that changed nothing, such as after a rollback, isn't a change to show.
  entities <- Filter(function(e) e$status != "modified" || length(e$diff) > 0L, entities)
  if (length(entities) == 0L) return(list())
  entities <- entities[order(vapply(entities, function(e) e$order, 0))]
  sets <- unique(vapply(entities, function(e) e$change_set, ""))
  lapply(sets, function(set) {
    members <- Filter(function(e) identical(e$change_set, set), entities)
    first <- members[[1]]
    list(change_set = set, title = first$title, kind = first$kind, status = first$status, change_text = first$change_text, note = first$note,
         changed_by = first$changed_by, changed_at = first$changed_at, entities = members)
  })
}

restore_order <- function(kinds) match(kinds, SAMPLE_BUILD_ORDER)

# Puts the model back as it was at a baseline, through the write path, after saving a
# safety baseline. History is never rewritten, and rolling back to the safety baseline
# undoes it. Draft requirements aren't part of a baseline, so they're left alone.
rollback_to_baseline <- function(con, baseline_id, author = "engineer") {
  in_tx(con, {
    safety <- allocate_id(con, "BL")
    .write_baseline_items(con, safety, sprintf("Safety before rollback to %s", baseline_id), NA_character_, author, is_safety = 1L)
    entities <- compare_entities(con, baseline_id, "working")
    change_set <- new_change_set(con)
    note <- sprintf("Rollback to %s", baseline_id)
    restores <- Filter(function(e) e$status != "added", entities)
    restores <- restores[order(restore_order(vapply(restores, function(e) e$kind, "")))]
    # A restored parent goes in before its children.
    depth <- vapply(restores, function(entity) {
      data <- revision_data(con, entity$kind, entity$id, entity$rev_a)
      steps <- 0L
      parent <- data$parent_id
      while (!is.null(parent) && steps < 100L) {
        steps <- steps + 1L
        row <- DBI::dbGetQuery(con, sprintf("SELECT data_json FROM revision WHERE entity_kind = ? AND entity_id = ? ORDER BY rev DESC LIMIT 1"), params = list(entity$kind, parent))
        parent <- if (nrow(row) == 1L) jsonlite::fromJSON(row$data_json, simplifyVector = FALSE)$parent_id else NULL
      }
      steps
    }, 0L)
    restores <- restores[order(restore_order(vapply(restores, function(e) e$kind, "")), depth)]
    for (entity in restores) {
      data <- revision_data(con, entity$kind, entity$id, entity$rev_a)
      write_entity(con, entity$kind, entity$id, restorable_fields(entity$kind, data), author, note, change_set)
    }
    # Additions go last: until the rest is put back, kept items may still point at them, such
    # as a component moved under a parent added since the baseline.
    additions <- Filter(function(e) e$status == "added", entities)
    additions <- additions[order(-restore_order(vapply(additions, function(e) e$kind, "")))]
    for (entity in additions) {
      if (!is.null(read_entity(con, entity$kind, entity$id))) delete_entity(con, entity$kind, entity$id, author, note, change_set)
    }
    list(safety = safety, change_set = change_set, area = "baseline", changed = length(entities))
  })
}
