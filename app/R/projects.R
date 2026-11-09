# The workspace folder: one SQLite file per project (SCHEMA §1).

workspace_dir <- function() {
  dir <- Sys.getenv("MODELBENCH_WORKSPACE_DIR", tools::R_user_dir("modelbench", "data"))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

list_projects <- function(dir) sort(list.files(dir, pattern = "[.]sqlite$"))

# The most recent project, or the sample the first time the app runs.
open_default_project <- function(dir) {
  files <- list_projects(dir)
  if (length(files) == 0L) return(build_sample_project(file.path(dir, "survey_uav.sqlite")))
  times <- file.mtime(file.path(dir, files))
  init_project_db(file.path(dir, files[which.max(times)]))
}

project_stem <- function(name) {
  stem <- gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(name))))
  if (!nzchar(stem)) stop("A project needs a name with letters or digits in it.", call. = FALSE)
  stem
}

# A copy of the open project under a new name, made through SQLite's backup so a project
# in use copies cleanly. The copy keeps its history and baselines.
copy_project <- function(dir, con, name) {
  path <- file.path(dir, paste0(project_stem(name), ".sqlite"))
  if (file.exists(path)) stop("There is already a project called ", project_stem(name), ".", call. = FALSE)
  copy <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(copy))
  RSQLite::sqliteCopyDatabase(con, copy)
  set_setting(copy, "project_name", trimws(name))
  path
}

# Deletes a project's file for good. The open project can't be deleted.
delete_project <- function(dir, file, open_path) {
  path <- file.path(dir, file)
  if (!file.exists(path)) stop("That project no longer exists.", call. = FALSE)
  if (identical(normalizePath(path), normalizePath(open_path))) stop("Switch to another project before deleting this one.", call. = FALSE)
  unlink(paste0(path, c("", "-wal", "-shm")))
  invisible(file)
}

create_project <- function(dir, name) {
  name <- trimws(name)
  stem <- gsub("[^a-z0-9]+", "_", tolower(name))
  stem <- gsub("^_+|_+$", "", stem)
  if (!nzchar(stem)) stop("A project needs a name with letters or digits in it.", call. = FALSE)
  path <- file.path(dir, paste0(stem, ".sqlite"))
  if (file.exists(path)) stop("There is already a project called ", stem, ".", call. = FALSE)
  con <- init_project_db(path)
  on.exit(DBI::dbDisconnect(con))
  set_setting(con, "project_name", name)
  path
}

switch_project <- function(state, path) {
  old <- shiny::isolate(state$con())
  state$selected_requirement(NULL)
  state$selected_entity(list(kind = "none", ids = character(0)))
  state$selected_interface(NULL)
  state$selected_failure(NULL)
  state$behavior_focus(NULL)
  state$return_to(NULL)
  state$open_in_write(NULL)
  state$commands(new_command_stack())
  state$con(init_project_db(path))
  DBI::dbDisconnect(old)
  invalidate_db(state, DB_AREAS)
}
