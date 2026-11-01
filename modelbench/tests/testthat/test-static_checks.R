# Nothing writes to a project's tables except the write path (SCHEMA §6).
# db_init.R is the one exception: it creates the schema, runs migrations and loads the raw sample.
WRITE_PATH_FILES <- c("db_write.R", "db_init.R")

test_that("database writes stay in the write path", {
  files <- list.files(test_path("..", "..", "R"), pattern = "[.]R$", full.names = TRUE)
  expect_gt(length(files), 0L)
  offenders <- character(0)
  for (file in files[!basename(files) %in% WRITE_PATH_FILES]) {
    if (any(grepl("dbExecute|dbSendStatement|dbWriteTable|dbAppendTable|dbRemoveTable", readLines(file)))) {
      offenders <- c(offenders, basename(file))
    }
  }
  expect_identical(offenders, character(0))
})

test_that("the core package doesn't depend on Shiny", {
  description <- paste(readLines(test_path("..", "..", "DESCRIPTION")), collapse = " ")
  expect_false(grepl("shiny", description, ignore.case = TRUE))
})
