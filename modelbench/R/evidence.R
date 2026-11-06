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

# A stored entry's content hash. Records made before hashes were kept have only a rev, so
# the hash is worked out from that revision.
stored_entry_hash <- function(con, entry) {
  if (!is.null(entry$h)) return(entry$h)
  context_content_hash(entry$kind, read_entity_rev(con, entry$kind, entry$id, entry$rev))
}

# The entries that differ between a record's stored context and the current one.
context_changes <- function(stored, current, con = NULL) {
  key <- function(entries) vapply(entries, function(entry) paste(entry$kind, entry$id), "")
  stored_keys <- key(stored)
  current_keys <- key(current)
  changes <- list()
  for (i in seq_along(stored)) {
    at <- match(stored_keys[i], current_keys)
    if (is.na(at)) {
      changes[[length(changes) + 1L]] <- list(kind = stored[[i]]$kind, id = stored[[i]]$id, change = "removed", was = stored[[i]]$rev, now = NA_integer_)
    } else if (if (is.null(con)) !identical(as.integer(stored[[i]]$rev), as.integer(current[[at]]$rev)) else !identical(stored_entry_hash(con, stored[[i]]), current[[at]]$h)) {
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
    changes <- context_changes(stored, current, con)
    if (length(changes) == 0L) return(NULL)
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

# State machine assertions that verify a requirement, each checked now and with its latest
# recorded run.
requirement_assertions <- function(con, requirement_id) {
  rows <- DBI::dbGetQuery(con, "
    SELECT a.*, m.name AS machine, r.result AS recorded, r.run_at, r.model_hash
      FROM link l JOIN sm_assertion a ON a.id = l.target_id JOIN state_machine m ON m.id = a.sm_id
      LEFT JOIN sm_run r ON r.id = (SELECT max(id) FROM sm_run WHERE assertion_id = a.id)
     WHERE l.link_type = 'verified_by' AND l.source_kind = 'requirement' AND l.source_id = ? AND l.target_kind = 'sm_assertion'
     ORDER BY a.id", params = list(requirement_id))
  rows$live <- rep(NA_character_, nrow(rows))
  rows$live_message <- rep(NA_character_, nrow(rows))
  for (i in seq_len(nrow(rows))) {
    outcome <- run_assertion_check(load_machine(con, rows$sm_id[i]), rows[i, ])
    rows$live[i] <- outcome$result
    rows$live_message[i] <- outcome$message
  }
  rows
}

# Whether a requirement's verification holds: at least one check passes, and none fails.
# A test counts by its latest current result; an assertion by how it runs now.
requirement_verification <- function(con, requirement_id) {
  cases <- requirement_evidence(con, requirement_id)
  checks <- requirement_assertions(con, requirement_id)
  passing <- sum(cases$result %in% "pass" & is.na(cases$stale)) + sum(checks$live %in% "pass")
  failing <- sum(cases$result %in% c("fail", "error")) + sum(checks$live %in% c("fail", "error"))
  list(verified = passing > 0L && failing == 0L, passing = passing, failing = failing,
       linked = nrow(cases) + nrow(checks))
}
