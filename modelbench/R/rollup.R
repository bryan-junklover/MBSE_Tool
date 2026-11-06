# Roll-ups and budgets. Code computes every number (BUILD_PLAN §2).

unit_conversion <- function(con, value, from, to) {
  if (is.na(from) || is.na(to) || identical(from, to)) return(value)
  units <- DBI::dbGetQuery(con, "SELECT symbol, quantity, si_factor, si_offset FROM unit WHERE symbol IN (?, ?)",
                           params = list(from, to))
  source <- units[units$symbol == from, ]
  target <- units[units$symbol == to, ]
  if (nrow(source) == 0L || nrow(target) == 0L || source$quantity != target$quantity) {
    stop("Can't convert ", from, " to ", to, call. = FALSE)
  }
  (value * source$si_factor + source$si_offset - target$si_offset) / target$si_factor
}

# The scope and everything under it, with the multiplicity carried down the tree.
scope_elements <- function(con, scope) {
  DBI::dbGetQuery(con, "
    WITH RECURSIVE tree (id, factor) AS (
      SELECT id, multiplicity FROM element WHERE id = ?
      UNION ALL
      SELECT e.id, tree.factor * e.multiplicity FROM element e JOIN tree ON e.parent_id = tree.id
    )
    SELECT id, factor FROM tree", params = list(scope))
}

property_definition <- function(con, property) {
  def <- DBI::dbGetQuery(con, "SELECT * FROM property_def WHERE name = ? ORDER BY stereotype_id IS NOT NULL LIMIT 1",
                         params = list(property))
  if (nrow(def) == 0L) stop("No property called ", property, call. = FALSE)
  def
}

# Sums (or takes the max or min of) a property over a scope, in the property's own
# unit. A placeholder is reported as excluded, never treated as zero.
rollup <- function(con, property, scope) {
  def <- property_definition(con, property)
  if (def$rollup == "none") stop(property, " doesn't roll up", call. = FALSE)
  tree <- scope_elements(con, scope)
  values <- DBI::dbGetQuery(con, "
    SELECT id, owner_id, value_state, num_value, unit FROM property_value
     WHERE owner_kind = 'element' AND property_def_id = ?", params = list(def$id))
  values <- values[values$owner_id %in% tree$id, , drop = FALSE]
  placeholders <- values$owner_id[values$value_state == "placeholder"]
  counted <- values[values$value_state != "placeholder" & !is.na(values$num_value), , drop = FALSE]
  amounts <- vapply(seq_len(nrow(counted)), function(i) {
    unit_conversion(con, counted$num_value[i], counted$unit[i], def$unit)
  }, 0)
  factors <- tree$factor[match(counted$owner_id, tree$id)]
  total <- switch(def$rollup,
                  sum = sum(amounts * factors),
                  max = if (length(amounts) > 0L) max(amounts) else NA_real_,
                  min = if (length(amounts) > 0L) min(amounts) else NA_real_)
  list(property = property, label = def$label, scope = scope, value = total, unit = def$unit,
       excluded_placeholders = length(placeholders), excluded_owners = placeholders, value_ids = values$id)
}

format_amount <- function(value, limit = value) {
  decimals <- if (abs(limit - round(limit)) > 1e-9 || abs(value - round(value)) > 1e-9) 2L else 0L
  formatC(value, format = "f", digits = decimals)
}

meets_limit <- function(value, limit, comparison) {
  if (comparison == "max") value <= limit else value >= limit
}

# One row per budget: the roll-up against its limit.
budget_status <- function(con) {
  budgets <- DBI::dbGetQuery(con, "
    SELECT b.id, b.scope_element_id, b.limit_value, b.unit, b.comparison, b.requirement_id, d.name AS property, d.label
      FROM budget b JOIN property_def d ON d.id = b.property_def_id ORDER BY b.id")
  rows <- lapply(seq_len(nrow(budgets)), function(i) {
    budget <- budgets[i, ]
    result <- rollup(con, budget$property, budget$scope_element_id)
    # A largest or smallest roll-up with nothing recorded yet has no value. That isn't a
    # failure, so it passes and says so.
    known <- !is.na(result$value)
    value <- if (known) unit_conversion(con, result$value, result$unit, budget$unit) else NA_real_
    pass <- !known || meets_limit(value, budget$limit_value, budget$comparison)
    note <- if (!known) "no values yet" else if (result$excluded_placeholders > 0L) sprintf("excludes %d placeholder", result$excluded_placeholders) else NA_character_
    data.frame(id = budget$id, label = budget$label, scope = budget$scope_element_id, value = value,
               limit = budget$limit_value, unit = budget$unit, pass = pass, note = note,
               text = if (known) {
                 sprintf("%s / %s %s", format_amount(value, budget$limit_value), format_amount(budget$limit_value), budget$unit)
               } else {
                 sprintf("no values yet / %s %s", format_amount(budget$limit_value), budget$unit)
               },
               stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(id = character(0), label = character(0), scope = character(0), value = numeric(0),
                                   limit = numeric(0), unit = character(0), pass = logical(0), note = character(0),
                                   text = character(0), stringsAsFactors = FALSE)), rows))
}
