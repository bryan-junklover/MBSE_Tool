# Loads the core and the shell, with the same network guard as the core's tests.
ROOT <- normalizePath(testthat::test_path("..", "..", ".."), mustWork = TRUE)
pkgload::load_all(file.path(ROOT, "modelbench"), quiet = TRUE, helpers = FALSE, export_all = TRUE)
source(file.path(ROOT, "modelbench", "tests", "testthat", "helper-00-guard.R"), local = globalenv())
for (file in list.files(file.path(ROOT, "app", "R"), full.names = TRUE)) source(file, local = globalenv())

local_sample_state <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  con <- build_sample_project(path)
  withr::defer(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), envir = env)
  shiny::isolate(new_app_state(con))
}

html_of <- function(tag) as.character(htmltools::renderTags(tag)$html)
