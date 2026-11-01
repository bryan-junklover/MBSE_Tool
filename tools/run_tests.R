#!/usr/bin/env Rscript
# Runs the core package's tests, then the app's. Exits non-zero on any failure.
testthat::test_local("modelbench", reporter = "summary", stop_on_failure = TRUE)
testthat::test_dir("app/tests/testthat", reporter = "summary", stop_on_failure = TRUE)
