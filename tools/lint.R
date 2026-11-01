#!/usr/bin/env Rscript
# Lints the core package, the app and tools. Exits non-zero on any finding.
lints <- c(lintr::lint_dir("modelbench/R"), lintr::lint_dir("modelbench/tests"), lintr::lint_dir("app"), lintr::lint_dir("tools"))
print(lints)
if (length(lints) > 0L) quit(status = 1L)
