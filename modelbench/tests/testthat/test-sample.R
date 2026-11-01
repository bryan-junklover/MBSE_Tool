# SCHEMA §10 holds for the raw sample and for the project built through the write path.

expect_sample_facts <- function(con) {
  expect_identical(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0L)
  expect_identical(count_rows(con, "v_placeholders"), 4L)
  expect_setequal(DBI::dbGetQuery(con, "SELECT entity_id FROM v_placeholders")$entity_id,
                  c("V-0014", "V-0016", "FLD-0005", "T-09"))
  expect_setequal(DBI::dbGetQuery(con, "SELECT requirement_id FROM v_draft_placeholders")[[1]], c("SYS-031", "SYS-007a"))
  expect_setequal(DBI::dbGetQuery(con, "SELECT entity_id FROM v_unreviewed")[[1]], c("C-010", "F8", "ST-11"))
  unhandled <- DBI::dbGetQuery(con, "SELECT event_name || ' x ' || state_name FROM v_sm_unhandled")[[1]]
  expect_setequal(unhandled, c("battery_low x Hold position", "link_lost x Return to home", "link_lost x Land"))
  expect_identical(DBI::dbGetQuery(con, "SELECT port_id FROM v_unconnected_ports")[[1]], "P-0019")
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM v_unallocated_requirements")[[1]], "SYS-021")
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM v_unallocated_functions")[[1]], "F8")
  expect_identical(count_rows(con, "v_baseline_blockers"), 7L)
  # Roll-ups apply multiplicity, so Propulsion's per-unit values count four times.
  totals <- DBI::dbGetQuery(con, "
    SELECT v.property_def_id AS def, sum(v.num_value * e.multiplicity) AS total
      FROM property_value v JOIN element e ON e.id = v.owner_id
     WHERE v.owner_kind = 'element' AND v.value_state = 'set' AND v.property_def_id IN ('PD-001', 'PD-002')
     GROUP BY v.property_def_id ORDER BY def")
  expect_equal(totals$total, c(3.31, 412))
}

test_that("the raw sample matches SCHEMA §10", {
  expect_sample_facts(local_sample())
})

test_that("the built sample matches SCHEMA §10", {
  expect_sample_facts(local_built_sample())
})

test_that("the sample has functional flows, each realised by a connector", {
  con <- local_sample()
  flows <- DBI::dbGetQuery(con, "
    SELECT c.id FROM connector c JOIN port p ON p.id = c.source_port_id JOIN element e ON e.id = p.element_id
     WHERE e.kind = 'function'")$id
  expect_length(flows, 9L)
  realised <- DBI::dbGetQuery(con, "
    SELECT l.source_id FROM link l JOIN connector t ON t.id = l.target_id WHERE l.link_type = 'realized_by'")$source_id
  expect_setequal(realised, flows)
})

test_that("the sample has the four scenarios of artboard 9", {
  expect_setequal(DBI::dbGetQuery(local_sample(), "SELECT id FROM scenario")$id, c("SC-001", "SC-002", "SC-003", "SC-004"))
})

test_that("every counter is ahead of the ids in use", {
  con <- local_sample()
  for (kind in names(ENTITY_PREFIXES)) {
    ids <- DBI::dbGetQuery(con, sprintf("SELECT id FROM %s", kind))$id
    prefix <- ENTITY_PREFIXES[[kind]]
    numbers <- suppressWarnings(as.integer(sub(paste0("^", prefix, "-?"), "", ids)))
    numbers <- numbers[!is.na(numbers)]
    if (length(numbers) == 0L) next
    counter <- DBI::dbGetQuery(con, "SELECT next_num FROM id_counter WHERE prefix = ?", params = list(prefix))$next_num
    expect_gt(counter, max(numbers), label = prefix)
  }
})

test_that("built rows equal the raw sample in every revisioned table", {
  raw <- local_sample()
  built <- local_built_sample()
  drop <- c("created_at", "updated_at", "rev")
  for (kind in revisioned_kinds()) {
    keys <- entity_key_cols(kind)
    columns <- setdiff(DBI::dbListFields(raw, kind), drop)
    read <- function(con) {
      rows <- DBI::dbReadTable(con, kind)[columns]
      rows <- rows[do.call(order, c(unname(rows[keys]), list(method = "radix"))), , drop = FALSE]
      rownames(rows) <- NULL
      rows
    }
    expect_equal(read(built), read(raw), info = kind)
  }
})

test_that("BL-03 and the changes since it match SCHEMA §10", {
  con <- local_built_sample()
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM baseline")$id, "BL-03")

  at <- function(kind, id) {
    rev <- DBI::dbGetQuery(con, "SELECT rev FROM baseline_item WHERE baseline_id = 'BL-03' AND entity_kind = ? AND entity_id = ?",
                           params = list(kind, id))$rev
    if (length(rev) == 0L) NULL else read_entity_rev(con, kind, id, rev)
  }
  expect_identical(at("property_value", "V-0013")$num_value, 5000L)
  expect_identical(at("property_value", "V-0003")$num_value, 1.47)
  expect_match(at("requirement", "SYS-002")$text, "within 5 km", fixed = TRUE)
  expect_match(read_entity(con, "requirement", "SYS-002")$text, "within 8 km", fixed = TRUE)
  expect_null(at("interface_field", "FLD-0005"))

  # Same diff the Phase 5 compare will make: drafts are out on both sides.
  base <- DBI::dbGetQuery(con, "SELECT entity_kind, entity_id, rev AS base_rev FROM baseline_item WHERE baseline_id = 'BL-03'")
  merged <- merge(baselineable_entities(con), base, all = TRUE)
  changed <- merged[is.na(merged$base_rev) | is.na(merged$rev) | merged$rev != merged$base_rev, ]
  expect_setequal(changed$entity_id, c("V-0013", "V-0003", "SYS-002", "FLD-0005"))

  sets <- DBI::dbGetQuery(con, "SELECT change_set, group_concat(entity_id) AS ids FROM revision
                                 WHERE entity_id IN ('V-0013', 'V-0003', 'SYS-002', 'FLD-0005')
                                   AND change_set <> (SELECT min(change_set) FROM revision)
                                 GROUP BY change_set")
  expect_identical(nrow(sets), 3L)
  expect_setequal(sets$ids[grepl("V-", sets$ids)] |> strsplit(",") |> unlist(), c("V-0013", "V-0003"))
})

test_that("BL-03 leaves out drafts, and its six open blockers predate it", {
  con <- local_built_sample()
  items <- DBI::dbGetQuery(con, "SELECT entity_id FROM baseline_item WHERE entity_kind = 'requirement'")$entity_id
  expect_length(items, 9L)
  expect_false(any(c("SYS-003", "SYS-007", "SYS-031", "SYS-007a") %in% items))
  blockers <- DBI::dbGetQuery(con, "SELECT entity_id FROM v_baseline_blockers")$entity_id
  expect_setequal(setdiff(blockers, "FLD-0005"), c("V-0014", "V-0016", "T-09", "C-010", "F8", "ST-11"))
})

test_that("TC-007's evidence was recorded against the BL-03 battery", {
  con <- local_built_sample()
  record <- DBI::dbGetQuery(con, "SELECT * FROM verification_record WHERE id = 'VR-0001'")
  expect_identical(record$context_hash, sha256(record$context_json))
  context <- jsonlite::fromJSON(record$context_json)
  expect_identical(context$rev[context$id == "V-0013"], 1L)
  expect_identical(context$rev[context$id == "SYS-001"], 1L)
  expect_true(all(c("C-003", "C-006", "V-0003", "V-0004") %in% context$id))
  # The record's own context no longer matches now the battery has changed.
  current <- canonical_json(verification_context(con, "SYS-001"))
  expect_false(identical(sha256(current), record$context_hash))
})

test_that("the built project has history and keeps the BL counter", {
  con <- local_built_sample()
  expect_gt(count_rows(con, "revision"), 200L)
  expect_identical(DBI::dbGetQuery(con, "SELECT next_num FROM id_counter WHERE prefix = 'BL'")$next_num, 4L)
})
