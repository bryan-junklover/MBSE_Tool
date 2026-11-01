# Element gates: checks on model edits that the database can't express.

# Both ends of a connector must name the same interface, with compatible directions.
# Between a parent and its child the signal passes straight through, so directions match.
check_connector <- function(con, source_port_id, target_port_id) {
  ports <- DBI::dbGetQuery(con, "
    SELECT p.id, p.name, p.direction, p.interface_id, p.element_id, e.parent_id, i.name AS interface_name
      FROM port p JOIN element e ON e.id = p.element_id LEFT JOIN interface i ON i.id = p.interface_id
     WHERE p.id IN (?, ?)", params = list(source_port_id, target_port_id))
  source <- ports[ports$id == source_port_id, ]
  target <- ports[ports$id == target_port_id, ]
  refused <- function(reason) list(ok = FALSE, reason = paste0("Can't connect: ", reason))
  if (nrow(source) == 0L || nrow(target) == 0L) return(refused("a port doesn't exist."))
  if (identical(source$element_id, target$element_id)) return(refused("both ends are on the same element."))
  describe <- function(port) {
    sprintf("%s %s", if (is.na(port$interface_name)) port$name else port$interface_name, port$direction)
  }
  if (is.na(source$interface_id) || is.na(target$interface_id)) {
    blank <- if (is.na(source$interface_id)) source else target
    return(refused(sprintf("port %s has no interface.", blank$name)))
  }
  if (source$interface_id != target$interface_id) {
    return(refused(sprintf("%s to %s.", describe(source), describe(target))))
  }
  delegation <- identical(source$parent_id, target$element_id) || identical(target$parent_id, source$element_id)
  compatible <- if (delegation) {
    source$direction == target$direction || "inout" %in% c(source$direction, target$direction)
  } else {
    source$direction %in% c("out", "inout") && target$direction %in% c("in", "inout")
  }
  if (!compatible) return(refused(sprintf("%s to %s.", describe(source), describe(target))))
  list(ok = TRUE, reason = NULL)
}

unit_quantity <- function(con, symbol) {
  DBI::dbGetQuery(con, "SELECT quantity FROM unit WHERE symbol = ?", params = list(symbol))$quantity
}

# A value's unit must measure the same thing as its property's unit.
check_value_unit <- function(con, property_def_id, unit) {
  if (is.null(unit) || is.na(unit)) return(NULL)
  def_unit <- DBI::dbGetQuery(con, "SELECT label, unit FROM property_def WHERE id = ?", params = list(property_def_id))
  if (nrow(def_unit) == 0L || is.na(def_unit$unit)) return(NULL)
  if (identical(unit_quantity(con, unit), unit_quantity(con, def_unit$unit))) return(NULL)
  sprintf("%s is measured in %s, so %s isn't a unit for it.", def_unit$label, def_unit$unit, unit)
}

# Everything the element gates find across the model, one row per problem.
element_gate_findings <- function(con) {
  connectors <- DBI::dbGetQuery(con, "SELECT id, source_port_id, target_port_id FROM connector")
  problems <- lapply(seq_len(nrow(connectors)), function(i) {
    check <- check_connector(con, connectors$source_port_id[i], connectors$target_port_id[i])
    if (check$ok) NULL else data.frame(kind = "connector", id = connectors$id[i], problem = check$reason)
  })
  no_interface <- DBI::dbGetQuery(con, "
    SELECT 'port' AS kind, p.id, e.name || ' port ' || p.name || ' has no interface' AS problem
      FROM port p JOIN element e ON e.id = p.element_id WHERE p.interface_id IS NULL")
  bad_field_units <- DBI::dbGetQuery(con, "
    SELECT 'interface_field' AS kind, f.id, f.name || ' uses an unknown unit: ' || f.unit AS problem
      FROM interface_field f WHERE f.unit IS NOT NULL AND f.unit NOT IN (SELECT symbol FROM unit)")
  do.call(rbind, c(problems, list(no_interface, bad_field_units)))
}
