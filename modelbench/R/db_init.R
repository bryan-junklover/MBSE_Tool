# Project databases: opening, schema, migrations and the raw sample loader.

# Splits a SQL script into statements. Semicolons inside comments and string
# literals don't end a statement, and comments are dropped.
split_sql <- function(sql) {
  chars <- strsplit(sql, "", fixed = TRUE)[[1]]
  out <- character(0)
  current <- character(0)
  i <- 1L
  n <- length(chars)
  in_string <- FALSE
  while (i <= n) {
    ch <- chars[i]
    if (in_string) {
      current <- c(current, ch)
      if (ch == "'") in_string <- FALSE
    } else if (ch == "-" && i < n && chars[i + 1L] == "-") {
      while (i <= n && chars[i] != "\n") i <- i + 1L
      next
    } else if (ch == ";") {
      out <- c(out, paste(current, collapse = ""))
      current <- character(0)
    } else {
      current <- c(current, ch)
      if (ch == "'") in_string <- TRUE
    }
    i <- i + 1L
  }
  out <- c(out, paste(current, collapse = ""))
  out <- trimws(out)
  out[nzchar(out)]
}

run_sql_statements <- function(con, statements) {
  for (statement in statements) {
    # This PRAGMA returns a row, which dbExecute() warns about.
    if (grepl("^PRAGMA journal_mode", statement, ignore.case = TRUE)) {
      DBI::dbGetQuery(con, statement)
    } else {
      DBI::dbExecute(con, statement)
    }
  }
  invisible(NULL)
}

run_sql_file <- function(con, path) {
  run_sql_statements(con, split_sql(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")))
}

open_project_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  con
}

schema_file <- function() {
  system.file("sql", "schema.sql", package = "modelbench", mustWork = TRUE)
}

migrations_dir <- function() {
  system.file("sql", "migrations", package = "modelbench")
}

# Opens the project at `path`, creating it if needed. Runs on every open, so
# everything it does is idempotent.
init_project_db <- function(path, migrations = migrations_dir()) {
  con <- open_project_db(path)
  run_sql_file(con, schema_file())
  run_migrations(con, migrations)
  con
}

schema_version <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT value FROM project_meta WHERE key = 'schema_version'")$value)
}

# Runs each migrations/NNN_description.sql with NNN above the stored version,
# once, in a transaction, and bumps the version with it (SCHEMA §9).
run_migrations <- function(con, dir = migrations_dir()) {
  files <- if (nzchar(dir)) sort(list.files(dir, pattern = "^[0-9]+_.*[.]sql$", full.names = TRUE)) else character(0)
  for (file in files) {
    version <- as.integer(sub("_.*$", "", basename(file)))
    if (version <= schema_version(con)) next
    DBI::dbWithTransaction(con, {
      run_sql_file(con, file)
      DBI::dbExecute(con, "UPDATE project_meta SET value = ? WHERE key = 'schema_version'",
                     params = list(as.character(version)))
    })
  }
  invisible(schema_version(con))
}

sample_file <- function() {
  system.file("sample", "sample_survey_uav.sql", package = "modelbench", mustWork = TRUE)
}

# Loads the sample's rows as they stand: the working copy, with no revisions.
# build_sample_project() gives the full project with history.
load_sample_db <- function(con) {
  DBI::dbWithTransaction(con, run_sql_file(con, sample_file()))
  invisible(con)
}
