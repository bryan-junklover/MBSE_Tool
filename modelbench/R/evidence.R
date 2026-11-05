# Evidence: what a verification record was recorded against, and whether that still
# holds (SCHEMA §7).

entity_value_text <- function(con, kind, id, rev = NULL) {
  if (kind != "property_value") return(NA_character_)
  row <- if (is.null(rev)) read_entity(con, kind, id) else read_entity_rev(con, kind, id, rev)
  if (is.null(row)) return(NA_character_)
  value <- if (!is.null(row$num_value) && !is.na(row$num_value)) format(row$num_value, trim = TRUE, drop0trailing = TRUE) else if (!is.null(row$text_value) && !is.na(row$text_value)) row$text_value else "[value]"
  unit <- if (is.null(row$unit) || is.na(row$unit)) "" else paste0(" ", row$unit)
  paste0(value, unit)
}

# The entries that differ between a record's stored context and the current one.
context_changes <- function(stored, current) {
  key <- function(entries) vapply(entries, function(entry) paste(entry$kind, entry$id), "")
  stored_keys <- key(stored)
  current_keys <- key(current)
  changes <- list()
  for (i in seq_along(stored)) {
    at <- match(stored_keys[i], current_keys)
    if (is.na(at)) {
      changes[[length(changes) + 1L]] <- list(kind = stored[[i]]$kind, id = stored[[i]]$id, change = "removed", was = stored[[i]]$rev, now = NA_integer_)
    } else if (!identical(as.integer(stored[[i]]$rev), as.integer(current[[at]]$rev))) {
      changes[[length(changes) + 1L]] <- list(kind = stored[[i]]$kind, id = stored[[i]]$id, change = "revised", was = stored[[i]]$rev, now = current[[at]]$rev)
    }
  }
  for (i in seq_along(current)) {
    if (!current_keys[i] %in% stored_keys) {
      changes[[length(changes) + 1L]] <- list(kind = current[[i]]$kind, id = current[[i]]$id, change = "added", was = NA_integer_, now = current[[i]]$rev)
    }
  }
  changes
}

describe_change <- function(con, change) {
  if (change$kind == "property_value" && change$change == "revised") {
    label <- DBI::dbGetQuery(con, "SELECT d.label FROM property_value v JOIN property_def d ON d.id = v.property_def_id WHERE v.id = ?", params = list(change$id))$label
    was <- entity_value_text(con, change$kind, change$id, change$was)
    if (!is.na(was)) return(sprintf("%s was %s, now %s", label, was, entity_value_text(con, change$kind, change$id)))
  }
  sprintf("%s %s %s", gsub("_", " ", change$kind), change$id, change$change)
}

# One row per record whose context no longer matches, with what changed. Only the
# latest record for a test case counts: re-recording is how evidence becomes current.
stale_evidence <- function(con) {
  records <- DBI::dbGetQuery(con, "
    SELECT v.* FROM verification_record v
     WHERE v.rowid = (SELECT max(o.rowid) FROM verification_record o
                       WHERE o.requirement_id = v.requirement_id AND coalesce(o.test_case_id, '') = coalesce(v.test_case_id, ''))
     ORDER BY v.id")
  empty <- data.frame(id = character(0), requirement_id = character(0), test_case_id = character(0), text = character(0), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(records)), function(i) {
    record <- records[i, ]
    current <- verification_context(con, record$requirement_id)
    if (identical(sha256(canonical_json(current)), record$context_hash)) return(NULL)
    stored <- jsonlite::fromJSON(record$context_json, simplifyVector = FALSE)
    changes <- context_changes(stored, current)
    data.frame(id = record$id, requirement_id = record$requirement_id, test_case_id = record$test_case_id,
               text = paste(vapply(changes, function(change) describe_change(con, change), ""), collapse = "; "), stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(empty), rows))
}

# Test cases verifying a requirement, each with its latest record and whether it's stale.
requirement_evidence <- function(con, requirement_id) {
  cases <- DBI::dbGetQuery(con, "
    SELECT t.id, t.name, t.method FROM test_case t JOIN link l ON l.target_kind = 'test_case' AND l.target_id = t.id
     WHERE l.link_type = 'verified_by' AND l.source_kind = 'requirement' AND l.source_id = ? ORDER BY t.id", params = list(requirement_id))
  stale <- stale_evidence(con)
  # rep() keeps the columns when there are no cases, which a bare NA can't fill.
  cases$result <- rep(NA_character_, nrow(cases))
  cases$recorded_at <- rep(NA_character_, nrow(cases))
  cases$stale <- rep(NA_character_, nrow(cases))
  for (i in seq_len(nrow(cases))) {
    latest <- DBI::dbGetQuery(con, "SELECT id, result, recorded_at FROM verification_record WHERE requirement_id = ? AND test_case_id = ? ORDER BY rowid DESC LIMIT 1",
                              params = list(requirement_id, cases$id[i]))
    if (nrow(latest) == 1L) {
      cases$result[i] <- latest$result
      cases$recorded_at[i] <- latest$recorded_at
      hit <- stale$text[stale$id == latest$id]
      if (length(hit) == 1L) cases$stale[i] <- hit
    }
  }
  cases
}

add_test_case <- function(con, requirement_id, name, method, author = "engineer") {
  if (!nzchar(trimws(name))) refuse("A test case needs a name.")
  with_change_set(con, function(change_set) {
    written <- write_test_case(con, fields = list(name = trimws(name), method = method), author = author, change_set = change_set)
    write_link(con, fields = list(link_type = "verified_by", source_kind = "requirement", source_id = requirement_id,
                                  target_kind = "test_case", target_id = written$id), author = author, change_set = change_set)
    written
  })
}
