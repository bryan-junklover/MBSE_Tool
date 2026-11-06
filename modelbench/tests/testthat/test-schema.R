test_that("split_sql ignores semicolons in comments and strings", {
  sql <- "-- one; two\nCREATE TABLE a (b TEXT DEFAULT 'x;y'); -- tail;\nSELECT 1;"
  expect_identical(split_sql(sql), c("CREATE TABLE a (b TEXT DEFAULT 'x;y')", "SELECT 1"))
})

test_that("schema runs twice and sets the version once", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  first <- init_project_db(path)
  DBI::dbDisconnect(first)
  second <- init_project_db(path)
  on.exit(DBI::dbDisconnect(second))
  expect_identical(schema_version(second), 2L)
  expect_identical(DBI::dbGetQuery(second, "PRAGMA foreign_keys")$foreign_keys, 1L)
  expect_identical(DBI::dbGetQuery(second, "PRAGMA journal_mode")$journal_mode, "wal")
})

test_that("migrations run once, in order, and bump the version", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT);", file.path(dir, "003_second.sql"))
  writeLines("CREATE TABLE m1 (x TEXT); -- old version, skipped", file.path(dir, "002_first.sql"))
  run_migrations(con, dir)
  expect_identical(schema_version(con), 3L)
  expect_true(DBI::dbExistsTable(con, "m2"))
  expect_false(DBI::dbExistsTable(con, "m1"))
  expect_no_error(run_migrations(con, dir))
})

test_that("a failed migration rolls back whole", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT); SELECT nope FROM nothing;", file.path(dir, "003_bad.sql"))
  expect_error(run_migrations(con, dir))
  expect_identical(schema_version(con), 2L)
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
  expect_identical(schema_version(con), 2L)
  expect_identical(DBI::dbGetQuery(con, "SELECT * FROM link ORDER BY id"), links)
  expect_no_error(DBI::dbExecute(con, "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id)
                                       VALUES ('L-0003', 'hosted_on', 'element', 'C-009', 'element', 'C-008')"))
  expect_identical(DBI::dbGetQuery(con, "PRAGMA integrity_check")$integrity_check, "ok")
  expect_setequal(DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'link' AND sql IS NOT NULL")$name,
                  c("idx_link_source", "idx_link_target"))
  # The views that read links still work.
  expect_no_error(DBI::dbGetQuery(con, "SELECT * FROM v_unallocated_requirements"))
})

test_that("every revisioned kind has a rev column and a known id scheme", {
  con <- local_project()
  for (kind in revisioned_kinds()) {
    expect_true("rev" %in% DBI::dbListFields(con, kind), info = kind)
    expect_true(all(entity_key_cols(kind) %in% DBI::dbListFields(con, kind)), info = kind)
  }
  expect_true(all(ENTITY_PREFIXES %in% names(ID_FORMATS)))
})
