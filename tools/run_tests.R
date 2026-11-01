#!/usr/bin/env Rscript
# Runs the core package's tests. Exits non-zero on any failure.
testthat::test_local("modelbench", reporter = "summary", stop_on_failure = TRUE)
