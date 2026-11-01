# What the write path knows about each revisioned entity kind.

# Tables with a two-column key are addressed as "a|b" in `revision`.
ENTITY_KEYS <- list(
  analysis_limit = c("analysis", "scope_id"),
  sm_event_scope = c("event_id", "state_id"),
  sm_ignore = c("state_id", "event_id")
)

ENTITY_AREAS <- c(
  stereotype = "model", property_def = "model", property_value = "model",
  budget = "model", analysis_limit = "model", element = "model",
  interface = "model", interface_field = "model", port = "model",
  connector = "model", requirement = "spec", link = "spec", test_case = "spec",
  scenario = "behavior", seq_lifeline = "behavior", seq_step = "behavior",
  act_node = "behavior", act_edge = "behavior", state_machine = "behavior",
  sm_state = "behavior", sm_event = "behavior", sm_event_scope = "behavior",
  sm_variable = "behavior", sm_transition = "behavior", sm_ignore = "behavior",
  sm_assertion = "behavior"
)

ENTITY_PREFIXES <- c(
  stereotype = "STY", property_def = "PD", property_value = "V", budget = "B",
  interface = "IF", interface_field = "FLD", port = "P", connector = "K",
  link = "L", test_case = "TC", scenario = "SC", seq_lifeline = "LL",
  seq_step = "ST", act_node = "N", act_edge = "G", state_machine = "SM",
  sm_state = "S", sm_event = "E", sm_variable = "VAR", sm_transition = "T",
  sm_assertion = "A"
)

# sprintf() format per prefix, matching the ids the sample uses.
ID_FORMATS <- c(
  STK = "STK-%03d", SYS = "SYS-%03d", C = "C-%03d", F = "F%d", EXT = "EXT-%d",
  IF = "IF-%03d", FLD = "FLD-%04d", P = "P-%04d", K = "K-%04d", PD = "PD-%03d",
  STY = "STY-%03d", ST = "ST-%02d", SC = "SC-%03d", SM = "SM-%03d", S = "S-%03d",
  E = "E-%02d", T = "T-%02d", VAR = "VAR-%d", LL = "LL-%d", A = "A-%03d",
  N = "N-%d", G = "G-%d", TC = "TC-%03d", VR = "VR-%04d", BL = "BL-%02d",
  PR = "PR-%04d", L = "L-%04d", V = "V-%04d", B = "B-%03d", VW = "VW-%04d",
  CS = "CS-%04d", CF = "CF-%04d"
)

revisioned_kinds <- function() names(ENTITY_AREAS)

entity_key_cols <- function(kind) {
  if (!is.null(ENTITY_KEYS[[kind]])) ENTITY_KEYS[[kind]] else "id"
}

check_kind <- function(kind) {
  if (!kind %in% revisioned_kinds()) stop("Unknown entity kind: ", kind, call. = FALSE)
  invisible(kind)
}

# Named list of key column values for an entity id.
entity_key_values <- function(kind, id) {
  cols <- entity_key_cols(kind)
  parts <- if (length(cols) > 1L) strsplit(id, "|", fixed = TRUE)[[1]] else id
  if (length(parts) != length(cols)) stop("Bad id for ", kind, ": ", id, call. = FALSE)
  stats::setNames(as.list(parts), cols)
}

# Prefixes the engineer can change in Project settings, with their defaults.
CONFIGURABLE_PREFIXES <- c(stakeholder = "STK", system = "SYS", subsystem = "SYS", component = "C")

# The prefix in force for a configurable id, from project_meta or the default.
configured_prefix <- function(con, what) {
  stored <- DBI::dbGetQuery(con, "SELECT value FROM project_meta WHERE key = ?",
                            params = list(paste0("id_prefix_", what)))$value
  if (length(stored) == 1L) stored else unname(CONFIGURABLE_PREFIXES[[what]])
}

# The id prefix for a new entity. Elements and requirements depend on their fields.
entity_prefix <- function(con, kind, fields) {
  if (kind == "element") {
    return(switch(fields$kind, component = configured_prefix(con, "component"), `function` = "F", external = "EXT"))
  }
  if (kind == "requirement") {
    level <- if (is.null(fields$level)) "system" else fields$level
    return(configured_prefix(con, level))
  }
  prefix <- ENTITY_PREFIXES[kind]
  if (is.na(prefix)) stop(kind, " has a composite key, so it needs an explicit id", call. = FALSE)
  unname(prefix)
}

# Revisioned children removed with a parent. The database cascades these, so
# core must write their final revisions first (SCHEMA §6).
ENTITY_CHILDREN <- list(
  stereotype = list(c("property_def", "stereotype_id")),
  element = list(c("port", "element_id"), c("state_machine", "owner_element_id")),
  interface = list(c("interface_field", "interface_id")),
  port = list(c("connector", "source_port_id"), c("connector", "target_port_id")),
  scenario = list(c("seq_lifeline", "scenario_id"), c("seq_step", "scenario_id"),
                  c("act_node", "scenario_id"), c("act_edge", "scenario_id")),
  seq_lifeline = list(c("seq_step", "from_lifeline_id"), c("seq_step", "to_lifeline_id")),
  act_node = list(c("act_edge", "from_node_id"), c("act_edge", "to_node_id")),
  state_machine = list(c("sm_state", "sm_id"), c("sm_event", "sm_id"), c("sm_variable", "sm_id"),
                       c("sm_transition", "sm_id"), c("sm_assertion", "sm_id")),
  sm_state = list(c("sm_transition", "from_state_id"), c("sm_transition", "to_state_id"),
                  c("sm_event_scope", "state_id"), c("sm_ignore", "state_id")),
  sm_event = list(c("sm_transition", "event_id"), c("sm_event_scope", "event_id"),
                  c("sm_ignore", "event_id"))
)

PROPERTY_OWNER_KINDS <- c("element", "connector", "interface", "requirement")
LINK_END_KINDS <- c("requirement", "element", "connector", "interface", "scenario",
                    "state_machine", "test_case", "sm_assertion")
