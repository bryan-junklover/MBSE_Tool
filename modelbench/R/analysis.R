# The analysis registry. Inputs are declared, so staleness and impact know what an
# analysis depends on, and the AI never supplies a figure (handoff §5.6).

ANALYSES <- new.env(parent = emptyenv())

# inputs: named list of list(property = , unit = ) for a value held by one element
# in the scope, or list(rollup = , unit = ) for a roll-up over the scope.
register_analysis <- function(name, inputs, fun, unit) {
  assign(name, list(name = name, inputs = inputs, fun = fun, unit = unit), envir = ANALYSES)
  invisible(name)
}

registered_analyses <- function() sort(ls(ANALYSES))

get_analysis <- function(name) {
  if (!exists(name, envir = ANALYSES, inherits = FALSE)) stop("No analysis called ", name, call. = FALSE)
  get(name, envir = ANALYSES)
}

resolve_analysis_input <- function(con, label, spec, scope) {
  if (!is.null(spec$rollup)) {
    result <- rollup(con, spec$rollup, scope)
    return(list(label = label, property = spec$rollup, value = unit_conversion(con, result$value, result$unit, spec$unit),
                value_ids = result$value_ids, problem = NULL))
  }
  tree <- scope_elements(con, scope)
  def <- property_definition(con, spec$property)
  values <- DBI::dbGetQuery(con, "SELECT id, owner_id, value_state, num_value, unit FROM property_value
                                   WHERE owner_kind = 'element' AND property_def_id = ?", params = list(def$id))
  values <- values[values$owner_id %in% tree$id, , drop = FALSE]
  if (nrow(values) == 0L) {
    return(list(label = label, property = spec$property, value = NA_real_, value_ids = character(0),
                problem = sprintf("%s isn't set", spec$property)))
  }
  if (nrow(values) > 1L) {
    return(list(label = label, property = spec$property, value = NA_real_, value_ids = values$id,
                problem = sprintf("%s is held by more than one element", spec$property)))
  }
  if (values$value_state == "placeholder" || is.na(values$num_value)) {
    return(list(label = label, property = spec$property, value = NA_real_, value_ids = values$id,
                problem = sprintf("%s is [value]", spec$property)))
  }
  amount <- if (is.null(spec$unit)) values$num_value else unit_conversion(con, values$num_value, values$unit, spec$unit)
  list(label = label, property = spec$property, value = amount, value_ids = values$id, problem = NULL)
}

# Runs an analysis, stores the result, and returns value, status and message.
run_analysis <- function(con, name, scope) {
  analysis <- get_analysis(name)
  inputs <- lapply(names(analysis$inputs), function(label) resolve_analysis_input(con, label, analysis$inputs[[label]], scope))
  names(inputs) <- names(analysis$inputs)
  problems <- unlist(lapply(inputs, function(input) input$problem))
  values <- lapply(inputs, function(input) input$value)
  status <- if (length(problems) > 0L) "missing_input" else "ok"
  value <- if (status == "ok") do.call(analysis$fun, values) else NA_real_
  unit <- if (length(unit_quantity(con, analysis$unit)) == 1L) analysis$unit else NA_character_
  detail <- list(inputs = lapply(inputs, function(input) input$value), problems = as.list(unname(problems)))
  upsert_analysis_result(con, name, scope, value, unit, status, sha256(canonical_json(values)), canonical_json(detail))
  list(analysis = name, scope = scope, value = value, unit = analysis$unit, status = status,
       message = if (status == "ok") NULL else paste0("can't compute: ", paste(unname(problems), collapse = ", ")))
}

# (kind, id, rev) of every value an analysis reads, for evidence context (SCHEMA §7).
analysis_input_entities <- function(con, name, scope) {
  analysis <- get_analysis(name)
  ids <- unique(unlist(lapply(names(analysis$inputs), function(label) {
    resolve_analysis_input(con, label, analysis$inputs[[label]], scope)$value_ids
  })))
  if (length(ids) == 0L) return(data.frame(kind = character(0), id = character(0), rev = integer(0)))
  rows <- DBI::dbGetQuery(con, sprintf("SELECT 'property_value' AS kind, id, rev FROM property_value WHERE id IN (%s)",
                                       paste(rep("?", length(ids)), collapse = ", ")), params = as.list(ids))
  rows
}

# The analysis inputs a requirement's verification relies on.
requirement_analysis_inputs <- function(con, requirement_id) {
  limits <- DBI::dbGetQuery(con, "SELECT analysis, scope_id FROM analysis_limit WHERE requirement_id = ?", params = list(requirement_id))
  rows <- lapply(seq_len(nrow(limits)), function(i) {
    if (limits$analysis[i] %in% registered_analyses()) analysis_input_entities(con, limits$analysis[i], limits$scope_id[i])
  })
  do.call(rbind, c(list(data.frame(kind = character(0), id = character(0), rev = integer(0))), rows))
}

# One row per analysis limit: run it and judge the result.
analysis_status <- function(con) {
  limits <- DBI::dbGetQuery(con, "SELECT * FROM analysis_limit ORDER BY analysis, scope_id")
  empty <- data.frame(analysis = character(0), scope = character(0), value = numeric(0), limit = numeric(0),
                      unit = character(0), comparison = character(0), status = character(0), pass = logical(0),
                      message = character(0), text = character(0), stringsAsFactors = FALSE)
  rows <- lapply(seq_len(nrow(limits)), function(i) {
    row <- limits[i, ]
    result <- run_analysis(con, row$analysis, row$scope_id)
    ok <- result$status == "ok"
    data.frame(analysis = row$analysis, scope = row$scope_id, value = result$value, limit = row$limit_value,
               unit = row$unit, comparison = row$comparison, status = result$status,
               pass = ok && meets_limit(result$value, row$limit_value, row$comparison),
               message = if (ok) NA_character_ else result$message,
               text = if (ok) sprintf("%.0f / %.0f %s", result$value, row$limit_value, row$unit) else result$message,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(empty), rows))
}

register_analysis(
  "endurance",
  inputs = list(capacity_mAh = list(property = "battery_capacity", unit = "mAh"),
                pack_V = list(property = "pack_voltage", unit = "V"),
                usable_fraction = list(property = "usable_fraction"),
                cruise_W = list(rollup = "power", unit = "W")),
  fun = function(capacity_mAh, pack_V, usable_fraction, cruise_W) {
    capacity_mAh / 1000 * pack_V * usable_fraction * 60 / cruise_W
  },
  unit = "min"
)
