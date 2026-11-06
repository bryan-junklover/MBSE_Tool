# Setting up a new project from the app: units, property definitions, budgets and state
# machines. Before these, a project could only get them from the sample's SQL.

unit_list <- function(con) DBI::dbGetQuery(con, "SELECT symbol, quantity, si_symbol, si_factor, si_offset FROM unit ORDER BY quantity, si_factor, symbol")

# A unit of your own. A quantity the registry already has keeps its SI unit, so values
# in either unit convert into each other.
add_unit <- function(con, symbol, quantity, si_symbol, si_factor, si_offset = 0) {
  symbol <- trimws(symbol)
  quantity <- trimws(tolower(quantity))
  si_symbol <- trimws(si_symbol)
  factor <- suppressWarnings(as.numeric(si_factor))
  offset <- suppressWarnings(as.numeric(if (is.null(si_offset) || !nzchar(trimws(si_offset))) 0 else si_offset))
  if (!nzchar(symbol)) refuse("A unit needs a symbol, such as kn.")
  if (grepl("\\s", symbol)) refuse("A unit symbol can't have spaces in it.")
  if (!nzchar(quantity)) refuse("Say what the unit measures, such as speed.")
  if (symbol %in% unit_list(con)$symbol) refuse("There's already a unit ", symbol, ".")
  if (is.na(factor) || factor <= 0) refuse("How many SI units one ", symbol, " is has to be a number above zero.")
  if (is.na(offset)) refuse("The offset has to be a number, or blank for none.")
  known <- unique(unit_list(con)$si_symbol[unit_list(con)$quantity == quantity])
  if (length(known) > 0L && !si_symbol %in% known) refuse(quantity, " is already measured against ", known[1], ", so ", symbol, " has to convert to ", known[1], ".")
  if (!nzchar(si_symbol)) refuse("Say which SI unit it converts to.")
  write_plain(con, "unit", list(symbol = symbol, quantity = quantity, si_symbol = si_symbol, si_factor = factor, si_offset = offset))
  invisible(symbol)
}

property_def_list <- function(con) {
  DBI::dbGetQuery(con, "SELECT d.id, d.name, d.label, d.value_type, d.unit, d.rollup,
                               (SELECT count(*) FROM property_value v WHERE v.property_def_id = d.id) AS used
                          FROM property_def d ORDER BY d.label")
}

# A property any component can hold, such as Mass in kg, summed up the tree.
add_property_def <- function(con, label, value_type = "number", unit = NULL, rollup = "none", author = "engineer") {
  label <- trimws(label)
  if (!nzchar(label)) refuse("A property needs a name, such as Mass.")
  name <- term_to_snake(label)
  if (!nzchar(name)) refuse("A property name needs letters or digits in it.")
  if (name %in% property_def_list(con)$name) refuse("There's already a property called ", term_to_title(label), ".")
  if (!value_type %in% c("number", "text")) refuse("A property is a number or text.")
  blank <- is.null(unit) || is.na(unit) || !nzchar(unit)
  if (value_type == "text" && !blank) refuse("A text property has no unit.")
  if (!blank && !unit %in% unit_list(con)$symbol) refuse(unit, " isn't in the unit registry. Add it under Units first.")
  if (!rollup %in% c("none", "sum", "max", "min")) refuse("Roll-up is none, sum, max or min.")
  if (value_type == "text" && rollup != "none") refuse("Only a number property can roll up.")
  write_property_def(con, fields = list(name = name, label = label, value_type = value_type, unit = if (blank) NA_character_ else unit,
                                        rollup = rollup), author = author, note = "Added in Project settings")
}

# A property's label, roll-up or unit. Its name stays: budgets, views and guards use it.
update_property_def <- function(con, property_def_id, field, value, author = "engineer") {
  def <- read_entity(con, "property_def", property_def_id)
  if (is.null(def)) refuse("That property no longer exists.")
  value <- trimws(if (is.null(value)) "" else value)
  budgets <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM budget WHERE property_def_id = ?", params = list(property_def_id))$n
  fields <- switch(field,
    label = {
      if (!nzchar(value)) refuse("A property needs a name.")
      list(label = value)
    },
    rollup = {
      if (!value %in% c("none", "sum", "max", "min")) refuse("Roll-up is none, sum, max or min.")
      if (def$value_type != "number" && value != "none") refuse("Only a number property can roll up.")
      if (value == "none" && budgets > 0L) refuse(def$label, " has a budget, which needs a roll-up. Remove the budget first.")
      list(rollup = value)
    },
    unit = {
      if (def$value_type != "number") refuse("A text property has no unit.")
      if (!nzchar(value)) {
        if (budgets > 0L) refuse(def$label, " has a budget, which needs a unit.")
        list(unit = NA_character_)
      } else {
        if (!value %in% unit_list(con)$symbol) refuse(value, " isn't in the unit registry.")
        if (!is.na(def$unit) && !identical(unit_quantity(con, value), unit_quantity(con, def$unit))) {
          refuse(def$label, " measures ", unit_quantity(con, def$unit), ", so its unit can't become ", value, ".")
        }
        list(unit = value)
      }
    },
    refuse("A property's ", field, " can't be changed here."))
  write_property_def(con, property_def_id, fields, author, "Property edited")
}

# A property no component holds and no budget uses can go.
delete_property_def <- function(con, property_def_id, author = "engineer") {
  def <- read_entity(con, "property_def", property_def_id)
  if (is.null(def)) refuse("That property no longer exists.")
  held <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM property_value WHERE property_def_id = ?", params = list(property_def_id))$n
  if (held > 0L) refuse(def$label, " is held by ", held, " component", if (held == 1L) "" else "s", ". Remove it from them first.")
  if (DBI::dbGetQuery(con, "SELECT count(*) AS n FROM budget WHERE property_def_id = ?", params = list(property_def_id))$n > 0L) {
    refuse(def$label, " has a budget. Remove the budget first.")
  }
  delete_entity(con, "property_def", property_def_id, author)
}

budget_list <- function(con) {
  status <- budget_status(con)
  rows <- DBI::dbGetQuery(con, "SELECT id, comparison, requirement_id FROM budget ORDER BY id")
  merge(status, rows, by = "id", sort = FALSE)
}

# A limit on a property's roll-up over a part of the system, such as mass at most 3.6 kg.
add_budget <- function(con, property_def_id, scope_element_id, limit, unit, comparison = "max", requirement_id = NULL, author = "engineer") {
  def <- DBI::dbGetQuery(con, "SELECT id, label, unit, rollup FROM property_def WHERE id = ?", params = list(property_def_id))
  if (nrow(def) == 0L) refuse("Choose the property the budget limits.")
  if (def$rollup == "none") refuse(def$label, " doesn't roll up, so it can't have a budget. Give it a roll-up first.")
  scope <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE id = ? AND kind = 'component'", params = list(scope_element_id))
  if (nrow(scope) == 0L) refuse("Choose the component the budget covers.")
  value <- suppressWarnings(as.numeric(limit))
  if (is.na(value)) refuse("The limit has to be a number.")
  if (!comparison %in% c("max", "min")) refuse("A budget is a most or a least.")
  if (is.na(def$unit)) refuse(def$label, " has no unit, so a limit on it would have none either. Budgets are for quantities such as mass or power.")
  if (is.null(unit) || !nzchar(unit)) unit <- def$unit
  if (!unit %in% unit_list(con)$symbol) refuse(unit, " isn't in the unit registry.")
  if (!identical(unit_quantity(con, unit), unit_quantity(con, def$unit))) refuse(def$label, " is in ", def$unit, ", so its budget can't be in ", unit, ".")
  taken <- DBI::dbGetQuery(con, "SELECT 1 FROM budget WHERE property_def_id = ? AND scope_element_id = ?", params = list(def$id, scope$id))
  if (nrow(taken) > 0L) refuse(scope$name, " already has a ", tolower(def$label), " budget.")
  if (!is.null(requirement_id) && nzchar(requirement_id) && is.null(read_entity(con, "requirement", requirement_id))) refuse("That requirement isn't in the model.")
  write_budget(con, fields = list(property_def_id = def$id, scope_element_id = scope$id, limit_value = value,
                                  unit = unit, comparison = comparison,
                                  requirement_id = if (is.null(requirement_id) || !nzchar(requirement_id)) NA_character_ else requirement_id),
               author = author, note = "Added in Project settings")
}

# A new state machine for a component, starting in one state so the simulator can run it.
create_state_machine <- function(con, name, owner_element_id, first_state = "Idle", author = "engineer") {
  name <- trimws(name)
  first_state <- trimws(first_state)
  if (!nzchar(name)) refuse("A state machine needs a name, such as Operating modes.")
  if (!nzchar(first_state)) refuse("Name the state it starts in.")
  owner <- DBI::dbGetQuery(con, "SELECT id FROM element WHERE id = ? AND kind = 'component'", params = list(owner_element_id))
  if (nrow(owner) == 0L) refuse("Choose the component whose behaviour it describes.")
  with_change_set(con, function(change_set) {
    machine <- write_state_machine(con, fields = list(name = name, owner_element_id = owner$id), author = author, note = "New state machine", change_set = change_set)
    state <- write_sm_state(con, fields = list(sm_id = machine$id, name = first_state), author = author, change_set = change_set)
    write_state_machine(con, machine$id, list(initial_state_id = state$id), author, change_set = change_set)
    list(change_set = change_set, id = machine$id, area = "behavior")
  })
}
