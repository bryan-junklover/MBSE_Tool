#!/usr/bin/env Rscript
# Installs the local widget packages into the project library. Run after changing one.
for (path in list.dirs("widgets", recursive = FALSE)) {
  install.packages(path, repos = NULL, type = "source")
}
