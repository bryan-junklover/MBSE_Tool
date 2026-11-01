# Builds the Survey UAV project through the write path, so it has real history:
# BL-03 form, baseline BL-03, then the three changes since (BUILD_PLAN Phase 0).

# Parents before children, for tables with a parent_id.
parents_first <- function(rows) {
  ordered <- rows[0, , drop = FALSE]
  remaining <- rows
  while (nrow(remaining) > 0L) {
    ready <- is.na(remaining$parent_id) | remaining$parent_id %in% ordered$id
    if (!any(ready)) stop("parent_id cycle in the sample", call. = FALSE)
    ordered <- rbind(ordered, remaining[ready, , drop = FALSE])
    remaining <- remaining[!ready, , drop = FALSE]
  }
  ordered
}

SAMPLE_BUILD_ORDER <- c(
  "stereotype", "property_def", "element", "requirement", "budget", "analysis_limit",
  "interface", "interface_field", "port", "connector", "property_value", "test_case",
  "scenario", "seq_lifeline", "seq_step", "act_node", "act_edge", "state_machine",
  "sm_state", "sm_event", "sm_event_scope", "sm_variable", "sm_transition", "sm_ignore",
  "sm_assertion", "link"
)

# Values that differ in BL-03 form. FLD-0005 didn't exist then.
sample_bl03_overrides <- function(kind, id, fields) {
  if (kind == "property_value" && id == "V-0013") fields$num_value <- 5000
  if (kind == "property_value" && id == "V-0003") fields$num_value <- 1.47
  if (kind == "requirement" && id == "SYS-002") fields$text <- sub("within 8 km", "within 5 km", fields$text, fixed = TRUE)
  fields
}

sample_row_fields <- function(kind, row) {
  as.list(row[setdiff(names(row), c(entity_key_cols(kind), "rev"))])
}

sample_copy_plain <- function(con, source, table) {
  rows <- DBI::dbReadTable(source, table)
  # init_project_db() has already set the schema version.
  if (table == "project_meta") rows <- rows[rows$key != "schema_version", , drop = FALSE]
  for (i in seq_len(nrow(rows))) write_plain(con, table, as.list(rows[i, , drop = FALSE]))
}

build_sample_project <- function(path) {
  source <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(source), add = TRUE)
  run_sql_file(source, schema_file())
  load_sample_db(source)

  con <- init_project_db(path)
  built <- FALSE
  on.exit(if (!built) DBI::dbDisconnect(con), add = TRUE)
  author <- "engineer"

  DBI::dbWithTransaction(con, {
    for (table in c("project_meta", "id_counter", "unit")) sample_copy_plain(con, source, table)
    change_set <- new_change_set(con)
    for (kind in SAMPLE_BUILD_ORDER) {
      rows <- DBI::dbReadTable(source, kind)
      if (kind %in% c("element", "requirement")) rows <- parents_first(rows)
      for (i in seq_len(nrow(rows))) {
        id <- paste(unlist(rows[i, entity_key_cols(kind), drop = TRUE]), collapse = "|")
        if (kind == "interface_field" && id == "FLD-0005") next
        fields <- sample_bl03_overrides(kind, id, sample_row_fields(kind, rows[i, , drop = FALSE]))
        write_entity(con, kind, id, fields, author, "Sample project, BL-03 form", change_set)
      }
    }
    for (table in c("glossary", "llm_call", "ai_proposal")) sample_copy_plain(con, source, table)

    write_verification_record(con, "SYS-001", "TC-007", "pass", author, evidence_ref = "FT-2026-09-30",
                              note = "37.2 min measured", id = "VR-0001")
    # Historical: BL-03 predates the blocker check, and six blockers were already open.
    .write_baseline_items(con, "BL-03", "Baseline 3", "Sample project before the battery change", author)
  })

  with_change_set(con, function(change_set) {
    write_property_value(con, "V-0013", list(num_value = 4200), author, "Smaller battery", change_set)
    write_property_value(con, "V-0003", list(num_value = 1.36), author, "Smaller battery", change_set)
  })
  sys_002 <- DBI::dbGetQuery(source, "SELECT text FROM requirement WHERE id = 'SYS-002'")$text
  write_requirement(con, "SYS-002", list(text = sys_002), author, "Longer telemetry range")
  field <- DBI::dbGetQuery(source, "SELECT * FROM interface_field WHERE id = 'FLD-0005'")
  write_interface_field(con, "FLD-0005", sample_row_fields("interface_field", field), author, "Heading accuracy added")

  built <- TRUE
  con
}
