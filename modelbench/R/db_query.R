# Read-only queries: canonical JSON, hashes, revisions and evidence context.

sort_keys <- function(x) {
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) x <- x[order(names(x), method = "radix")]
  lapply(x, sort_keys)
}

# Sorted keys, no whitespace, full precision: safe to hash.
canonical_json <- function(x) {
  as.character(jsonlite::toJSON(sort_keys(x), auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

sha256 <- function(text) digest::digest(text, algo = "sha256", serialize = FALSE)

quote_ident <- function(con, name) as.character(DBI::dbQuoteIdentifier(con, name))

entity_id_sql <- function(con, kind) {
  paste(vapply(entity_key_cols(kind), quote_ident, "", con = con), collapse = " || '|' || ")
}

# The current row as a list, or NULL when it doesn't exist.
read_entity <- function(con, kind, id) {
  check_kind(kind)
  key <- entity_key_values(kind, id)
  where <- paste(sprintf("%s = ?", vapply(names(key), quote_ident, "", con = con)), collapse = " AND ")
  row <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE %s", quote_ident(con, kind), where),
                         params = unname(key))
  if (nrow(row) == 0L) NULL else as.list(row)
}

# An entity's state at a revision, as a list, or NULL.
read_entity_rev <- function(con, kind, id, rev) {
  row <- DBI::dbGetQuery(con, "SELECT data_json FROM revision WHERE entity_kind = ? AND entity_id = ? AND rev = ?",
                         params = list(kind, id, rev))
  if (nrow(row) == 0L) NULL else jsonlite::fromJSON(row$data_json, simplifyVector = TRUE)
}

# One row per live revisioned entity: entity_kind, entity_id, rev.
live_entities <- function(con) {
  rows <- lapply(revisioned_kinds(), function(kind) {
    DBI::dbGetQuery(con, sprintf("SELECT ? AS entity_kind, %s AS entity_id, rev FROM %s",
                                 entity_id_sql(con, kind), quote_ident(con, kind)),
                    params = list(kind))
  })
  do.call(rbind, rows)
}

model_hash <- function(con) {
  items <- live_entities(con)
  lines <- sort(paste(items$entity_kind, items$entity_id, items$rev, sep = "|"), method = "radix")
  sha256(paste(lines, collapse = "\n"))
}

# What a baseline holds, and what the working copy is compared on: every live
# entity except draft and deprecated requirements (SCHEMA §6).
baselineable_entities <- function(con) {
  items <- live_entities(con)
  excluded <- DBI::dbGetQuery(con, "SELECT id FROM requirement WHERE status <> 'committed'")$id
  items[!(items$entity_kind == "requirement" & items$entity_id %in% excluded), , drop = FALSE]
}

# The (kind, id, rev) entries a verification record depends on (SCHEMA §7).
# Analysis inputs (Phase 2) and state machine content (Phase 4) join this list.
verification_context <- function(con, requirement_id) {
  requirement <- DBI::dbGetQuery(con, "SELECT 'requirement' AS kind, id, rev FROM requirement WHERE id = ?",
                                 params = list(requirement_id))
  if (nrow(requirement) == 0L) stop("No requirement ", requirement_id, call. = FALSE)
  elements <- DBI::dbGetQuery(con, "
    SELECT 'element' AS kind, e.id, e.rev FROM element e
      JOIN link l ON l.target_kind = 'element' AND l.target_id = e.id
     WHERE l.link_type = 'satisfied_by' AND l.source_kind = 'requirement' AND l.source_id = ?",
                              params = list(requirement_id))
  values <- DBI::dbGetQuery(con, "
    SELECT 'property_value' AS kind, v.id, v.rev FROM property_value v
      JOIN link l ON l.target_kind = 'element' AND l.target_id = v.owner_id
     WHERE v.owner_kind = 'element' AND l.link_type = 'satisfied_by'
       AND l.source_kind = 'requirement' AND l.source_id = ?",
                            params = list(requirement_id))
  context <- rbind(requirement, elements, values)
  context <- context[order(context$kind, context$id, method = "radix"), , drop = FALSE]
  lapply(seq_len(nrow(context)), function(i) as.list(context[i, , drop = FALSE]))
}
