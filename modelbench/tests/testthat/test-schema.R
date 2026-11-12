test_that("split_sql ignores semicolons in comments and strings", {
  sql <- "-- one; two\nCREATE TABLE a (b TEXT DEFAULT 'x;y'); -- tail;\nSELECT 1;"
  expect_identical(split_sql(sql), c("CREATE TABLE a (b TEXT DEFAULT 'x;y')", "SELECT 1"))
})

# The newest migration's number: what every project ends up at.
latest_version <- function() max(as.integer(sub("_.*$", "", list.files(migrations_dir(), pattern = "^[0-9]+_.*[.]sql$"))))

test_that("schema runs twice and sets the version once", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  first <- init_project_db(path)
  DBI::dbDisconnect(first)
  second <- init_project_db(path)
  on.exit(DBI::dbDisconnect(second))
  expect_identical(schema_version(second), latest_version())
  expect_identical(DBI::dbGetQuery(second, "PRAGMA foreign_keys")$foreign_keys, 1L)
  expect_identical(DBI::dbGetQuery(second, "PRAGMA journal_mode")$journal_mode, "wal")
})

test_that("migrations run once, in order, and bump the version", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT);", file.path(dir, sprintf("%03d_second.sql", latest_version() + 1L)))
  writeLines("CREATE TABLE m1 (x TEXT); -- old version, skipped", file.path(dir, sprintf("%03d_first.sql", latest_version())))
  run_migrations(con, dir)
  expect_identical(schema_version(con), latest_version() + 1L)
  expect_true(DBI::dbExistsTable(con, "m2"))
  expect_false(DBI::dbExistsTable(con, "m1"))
  expect_no_error(run_migrations(con, dir))
})

test_that("a failed migration rolls back whole", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT); SELECT nope FROM nothing;", file.path(dir, sprintf("%03d_bad.sql", latest_version() + 1L)))
  expect_error(run_migrations(con, dir))
  expect_identical(schema_version(con), latest_version())
  expect_false(DBI::dbExistsTable(con, "m2"))
})

test_that("migration 002 lets an existing project record where software runs", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  # A project made before hosted_on existed: the old link table, at version 1.
  old <- open_project_db(path)
  old_schema <- sub("'hosted_on',[^\n]*\n", "", paste(readLines(schema_file()), collapse = "\n"))
  run_sql_statements(old, split_sql(old_schema))
  DBI::dbExecute(old, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id, rationale) VALUES
                        ('L-0001', 'satisfied_by', 'requirement', 'SYS-001', 'element', 'C-003', 'Battery sets endurance'),
                        ('L-0002', 'allocated_to', 'element', 'F1', 'element', 'C-001', NULL)")
  links <- DBI::dbGetQuery(old, "SELECT * FROM link ORDER BY id")
  expect_error(DBI::dbExecute(old, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                                    VALUES ('L-0999', 'hosted_on', 'element', 'C-009', 'element', 'C-008')"), "CHECK")
  DBI::dbDisconnect(old)

  con <- init_project_db(path)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(schema_version(con), latest_version())
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM link ORDER BY id"), links)
  expect_no_error(DBI::dbExecute(con, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                                       VALUES ('L-0003', 'hosted_on', 'element', 'C-009', 'element', 'C-008')"))
  expect_identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")$integrity_check, "ok")
  expect_setequal(DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'link' AND sql IS NOT NULL")$name,
                  c("idx_link_source", "idx_link_target"))
  # The views that read links still work.
  expect_no_error(DBI::dbGetQuery(con, "SELECT * FROM v_unallocated_requirements"))
})

test_that("migration 003 lets a version 2 project link failure modes", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- init_project_db(path)
  DBI::dbDisconnect(con)
  # Put the project back at version 2: the link table without the failure links.
  old <- open_project_db(path)
  run_sql_file(old, file.path(migrations_dir(), "002_hosted_on.sql"))
  DBI::dbExecute(old, "UPDATE project_meta SET value = '2' WHERE key = 'schema_version'")
  expect_error(DBI::dbExecute(old, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                                    VALUES ('L-0001', 'detected_by', 'failure_mode', 'FM-0001', 'sm_event', 'E-01')"), "CHECK")
  DBI::dbExecute(old, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                       VALUES ('L-0002', 'hosted_on', 'element', 'C-009', 'element', 'C-008')")
  DBI::dbDisconnect(old)

  con <- init_project_db(path)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(schema_version(con), latest_version())
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM link")$id, "L-0002")
  expect_no_error(DBI::dbExecute(con, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                                       VALUES ('L-0001', 'detected_by', 'failure_mode', 'FM-0001', 'sm_event', 'E-01')"))
  expect_identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")$integrity_check, "ok")
})

test_that("migration 004 rebuilds proposals and routes, keeping links that point at a proposal", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  con <- build_sample_project(path)
  id <- insert_proposal(con, list(task = "trace_coach", proposal_kind = "candidate_link", title = "Which?", payload_json = "{}"))
  DBI::dbExecute(con, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id, rationale, origin, ai_proposal_id)
                       VALUES ('L-0900', 'satisfied_by', 'requirement', 'SYS-002', 'element', 'C-001', 'From the coach', 'ai_candidate', ?)", params = list(id))
  proposals <- DBI::dbGetQuery(con, "SELECT * FROM ai_proposal ORDER BY id")
  DBI::dbExecute(con, "DELETE FROM task_route WHERE task = 'failure_suggest'")
  DBI::dbExecute(con, "UPDATE project_meta SET value = '3' WHERE key = 'schema_version'")
  DBI::dbDisconnect(con)

  con <- init_project_db(path)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(schema_version(con), 4L)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM ai_proposal ORDER BY id"), proposals)
  expect_identical(DBI::dbGetQuery(con, "SELECT ai_proposal_id FROM link WHERE id = 'L-0900'")$ai_proposal_id, id)
  expect_true("failure_suggest" %in% routes_table(con)$task)
  expect_identical(DBI::dbGetQuery(con, "PRAGMA foreign_keys")$foreign_keys, 1L)
  expect_identical(nrow(DBI::dbGetQuery(con, "PRAGMA foreign_key_check")), 0L)
  expect_match(DBI::dbGetQuery(con, "SELECT sql FROM sqlite_master WHERE name = 'v_unreviewed'")$sql, "failure_mode", fixed = TRUE)
})

test_that("a migration that leaves a broken reference rolls back, with foreign keys back on", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("INSERT INTO llm_cache (task, key_hash, llm_call_id) VALUES ('t', 'k', 999);", file.path(dir, sprintf("%03d_dangling.sql", latest_version() + 1L)))
  expect_error(run_migrations(con, dir), "1 broken reference, in llm_cache")
  expect_identical(schema_version(con), latest_version())
  expect_identical(DBI::dbGetQuery(con, "PRAGMA foreign_keys")$foreign_keys, 1L)
})

test_that("every revisioned kind has a rev column and a known id scheme", {
  con <- local_project()
  for (kind in revisioned_kinds()) {
    expect_true("rev" %in% DBI::dbListFields(con, kind), info = kind)
    expect_true(all(entity_key_cols(kind) %in% DBI::dbListFields(con, kind)), info = kind)
  }
  expect_true(all(ENTITY_PREFIXES %in% names(ID_FORMATS)))
})
