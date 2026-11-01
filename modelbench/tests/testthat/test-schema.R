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
  expect_identical(schema_version(second), 1L)
  expect_identical(DBI::dbGetQuery(second, "PRAGMA foreign_keys")$foreign_keys, 1L)
  expect_identical(DBI::dbGetQuery(second, "PRAGMA journal_mode")$journal_mode, "wal")
})

test_that("migrations run once, in order, and bump the version", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT);", file.path(dir, "002_second.sql"))
  writeLines("CREATE TABLE m1 (x TEXT); -- old version, skipped", file.path(dir, "001_first.sql"))
  run_migrations(con, dir)
  expect_identical(schema_version(con), 2L)
  expect_true(DBI::dbExistsTable(con, "m2"))
  expect_false(DBI::dbExistsTable(con, "m1"))
  expect_no_error(run_migrations(con, dir))
})

test_that("a failed migration rolls back whole", {
  con <- local_project()
  dir <- withr::local_tempdir()
  writeLines("CREATE TABLE m2 (x TEXT); SELECT nope FROM nothing;", file.path(dir, "002_bad.sql"))
  expect_error(run_migrations(con, dir))
  expect_identical(schema_version(con), 1L)
  expect_false(DBI::dbExistsTable(con, "m2"))
})

test_that("every revisioned kind has a rev column and a known id scheme", {
  con <- local_project()
  for (kind in revisioned_kinds()) {
    expect_true("rev" %in% DBI::dbListFields(con, kind), info = kind)
    expect_true(all(entity_key_cols(kind) %in% DBI::dbListFields(con, kind)), info = kind)
  }
  expect_true(all(ENTITY_PREFIXES %in% names(ID_FORMATS)))
})
