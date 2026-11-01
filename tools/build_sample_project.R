#!/usr/bin/env Rscript
# Builds the Survey UAV project through the write path.
# Usage: Rscript tools/build_sample_project.R [output.sqlite]
args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args) >= 1L) args[[1]] else "survey_uav.sqlite"
if (file.exists(path)) stop(path, " already exists; choose another path or remove it first")
pkgload::load_all("modelbench", quiet = TRUE)
con <- build_sample_project(path)
cat("Built", path, "\nmodel hash:", model_hash(con), "\n")
DBI::dbDisconnect(con)
