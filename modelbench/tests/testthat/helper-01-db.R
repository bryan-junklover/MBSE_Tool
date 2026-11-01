# A fresh project in a temp file, closed when the calling test ends.
local_project <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- init_project_db(path)
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

local_sample <- function(env = parent.frame()) {
  con <- local_project(env)
  load_sample_db(con)
  con
}

local_built_sample <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- build_sample_project(path)
  withr::defer(DBI::dbDisconnect(con), envir = env)
  con
}

count_rows <- function(con, view) DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s", view))$n

column_values <- function(con, sql) sort(DBI::dbGetQuery(con, sql)[[1]])

# Runs one statement in a transaction that always rolls back, so the database
# rule under test is the only thing a negative test touches.
expect_db_refuses <- function(con, sql, pattern = "constraint") {
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con))
  testthat::expect_error(DBI::dbExecute(con, sql), pattern, ignore.case = TRUE)
}
