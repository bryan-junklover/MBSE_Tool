# Failure modes: what can fail, its cause, what happens next and how the system responds
# (docs/VIEWPOINTS_AND_FAILURES.md §3). Effects and responses only; no scoring.

FAILURE_CAUSES <- c(lost = "Lost", stale = "Stale", out_of_range = "Out of range", erroneous = "Wrong value",
                    degraded = "Degraded", failed = "Failed", open_circuit = "Open circuit", short_circuit = "Short circuit",
                    over_temperature = "Over-temperature", mechanical = "Mechanical", other = "Other")
PHYSICAL_CAUSES <- c("open_circuit", "short_circuit", "over_temperature", "mechanical")
FAILURE_RESPONSES <- c(placeholder = "Not decided", handled = "Handled", accepted = "Accepted", unhandled = "Unhandled")
FAILURE_SUBJECT_LABELS <- c(element = "Component", interface = "Interface", port = "Port", sm_state = "State")
FAILURE_TARGET_LABELS <- c(sm_event = "state machine event", requirement = "requirement", test_case = "test case",
                           sm_transition = "transition", element = "component")
FAILURE_RULES <- c(open_response = "Response not decided", handled_unlinked = "Handled, but by nothing",
                   missing_signal_loss = "Signal loss not covered", missing_power_loss = "Power loss not covered",
                   missing_over_temperature = "Over-temperature not covered", missing_out_of_range = "Out-of-range value not covered",
                   handled_unverified = "Handled by an unverified requirement")
WAIVABLE_RULES <- c("missing_signal_loss", "missing_power_loss", "missing_over_temperature", "missing_out_of_range")

capitalise <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))

# Everything a failure mode can be about, labelled, with `value` = "kind|id".
failure_subjects <- function(con) {
  root <- default_structure_root(con)
  elements <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind IN ('component', 'external') ORDER BY kind, sort_order, name")
  elements <- elements[!elements$id %in% root, , drop = FALSE]
  interfaces <- DBI::dbGetQuery(con, "SELECT id, name FROM interface WHERE layer = 'physical' ORDER BY name")
  ports <- DBI::dbGetQuery(con, "SELECT p.id, e.name || '.' || p.name AS name FROM port p JOIN element e ON e.id = p.element_id
                                  WHERE e.kind <> 'function' ORDER BY e.sort_order, e.name, p.name")
  states <- DBI::dbGetQuery(con, "SELECT s.id, m.name || ': ' || s.name AS name FROM sm_state s JOIN state_machine m ON m.id = s.sm_id ORDER BY m.id, s.rowid")
  rows <- rbind(data.frame(kind = rep("element", nrow(elements)), id = elements$id, name = elements$name),
                data.frame(kind = rep("interface", nrow(interfaces)), id = interfaces$id, name = interfaces$name),
                data.frame(kind = rep("port", nrow(ports)), id = ports$id, name = ports$name),
                data.frame(kind = rep("sm_state", nrow(states)), id = states$id, name = states$name))
  rows$value <- paste(rows$kind, rows$id, sep = "|")
  rows$label <- sprintf("%s: %s", FAILURE_SUBJECT_LABELS[rows$kind], rows$name)
  rows
}

failure_subject_label <- function(con, kind, id, field_name = NA_character_) {
  name <- switch(kind,
    element = DBI::dbGetQuery(con, "SELECT name FROM element WHERE id = ?", params = list(id))$name,
    interface = DBI::dbGetQuery(con, "SELECT name FROM interface WHERE id = ?", params = list(id))$name,
    port = DBI::dbGetQuery(con, "SELECT e.name || '.' || p.name AS name FROM port p JOIN element e ON e.id = p.element_id WHERE p.id = ?", params = list(id))$name,
    sm_state = DBI::dbGetQuery(con, "SELECT m.name || ': ' || s.name AS name FROM sm_state s JOIN state_machine m ON m.id = s.sm_id WHERE s.id = ?", params = list(id))$name,
    character(0))
  if (length(name) != 1L) name <- id
  if (!is.null(field_name) && !is.na(field_name) && nzchar(field_name)) paste0(name, ".", field_name) else name
}

# What a failure link can point at, labelled, with `value` = "kind|id".
failure_link_choices <- function(con, link_type) {
  sources <- list(
    sm_event = "SELECT e.id, m.name || ': ' || e.name AS name FROM sm_event e JOIN state_machine m ON m.id = e.sm_id ORDER BY m.id, e.name",
    requirement = "SELECT id, id || ' ' || coalesce(short_title, substr(text, 1, 50)) AS name FROM requirement WHERE status <> 'deprecated' ORDER BY id",
    test_case = "SELECT id, id || ' ' || name AS name FROM test_case ORDER BY id",
    sm_transition = "SELECT t.id, m.name || ': ' || f.name || ' → ' || e.name || ' → ' || s.name AS name FROM sm_transition t
                       JOIN state_machine m ON m.id = t.sm_id JOIN sm_state f ON f.id = t.from_state_id
                       JOIN sm_state s ON s.id = t.to_state_id JOIN sm_event e ON e.id = t.event_id ORDER BY m.id, f.rowid, e.name",
    element = "SELECT id, name FROM element WHERE kind = 'component' ORDER BY sort_order, name")
  rows <- lapply(FAILURE_LINK_TARGETS[[link_type]], function(kind) {
    found <- DBI::dbGetQuery(con, sources[[kind]])
    if (nrow(found) == 0L) return(NULL)
    data.frame(kind = kind, id = found$id, label = sprintf("%s: %s", capitalise(FAILURE_TARGET_LABELS[[kind]]), found$name))
  })
  rows <- do.call(rbind, c(list(data.frame(kind = character(0), id = character(0), label = character(0))), rows))
  rows$value <- paste(rows$kind, rows$id, sep = "|")
  rows
}

failure_target_label <- function(con, kind, id) {
  choices <- failure_link_choices(con, if (kind %in% FAILURE_LINK_TARGETS$detected_by) "detected_by" else "handled_by")
  label <- choices$label[choices$kind == kind & choices$id == id]
  if (length(label) == 1L) label else sprintf("%s %s", FAILURE_TARGET_LABELS[[kind]], id)
}

failure_links <- function(con, failure_id) {
  links <- DBI::dbGetQuery(con, "SELECT id, link_type, target_kind, target_id FROM link
                                  WHERE source_kind = 'failure_mode' AND source_id = ? AND link_type IN ('detected_by', 'handled_by') ORDER BY id",
                           params = list(failure_id))
  links$label <- vapply(seq_len(nrow(links)), function(i) failure_target_label(con, links$target_kind[i], links$target_id[i]), "")
  links
}

# Every failure mode, with what it's about and its links, for lists and documents.
failure_mode_list <- function(con) {
  rows <- DBI::dbGetQuery(con, "SELECT * FROM failure_mode ORDER BY id")
  rows$subject <- vapply(seq_len(nrow(rows)), function(i) failure_subject_label(con, rows$subject_kind[i], rows$subject_id[i], rows$field_name[i]), "")
  links <- DBI::dbGetQuery(con, "SELECT source_id, link_type, target_kind, target_id FROM link
                                  WHERE source_kind = 'failure_mode' AND link_type IN ('detected_by', 'handled_by') ORDER BY id")
  links$label <- vapply(seq_len(nrow(links)), function(i) failure_target_label(con, links$target_kind[i], links$target_id[i]), "")
  joined <- function(type) vapply(rows$id, function(id) paste(links$label[links$source_id == id & links$link_type == type], collapse = "; "), "", USE.NAMES = FALSE)
  rows$detected_by <- joined("detected_by")
  rows$handled_by <- joined("handled_by")
  rows
}

# A new failure mode, about `subject` ("kind|id"). It starts with its response not decided.
add_failure_mode <- function(con, subject, cause_kind = NULL, field_name = NULL, author = "engineer") {
  parts <- strsplit(subject, "|", fixed = TRUE)[[1]]
  if (length(parts) != 2L) refuse("Choose what fails.")
  if (is.null(cause_kind)) cause_kind <- switch(parts[1], interface = "lost", port = "lost", sm_state = "other", "failed")
  write_entity(con, "failure_mode", NULL, list(subject_kind = parts[1], subject_id = parts[2], cause_kind = cause_kind,
                                               field_name = if (is.null(field_name) || !nzchar(field_name)) NA_character_ else field_name),
               author)
}

# One field of a failure mode. Changing what fails clears a field name that no longer fits.
update_failure_mode <- function(con, failure_id, field, value, author = "engineer") {
  editable <- c("subject", "field_name", "cause_kind", "cause", "local_effect", "system_effect", "response_state", "response_note")
  if (!field %in% editable) refuse("A failure mode has no ", field, " to change.")
  blank <- is.null(value) || !nzchar(trimws(value))
  if (field == "subject") {
    parts <- strsplit(value, "|", fixed = TRUE)[[1]]
    if (length(parts) != 2L) refuse("Choose what fails.")
    return(write_entity(con, "failure_mode", failure_id, list(subject_kind = parts[1], subject_id = parts[2], field_name = NA_character_), author))
  }
  if (field == "response_state" && identical(value, "accepted")) {
    note <- read_entity(con, "failure_mode", failure_id)$response_note
    if (is.null(note) || is.na(note) || !nzchar(trimws(note))) refuse("Say why it's accepted first, in the note under Response.")
  }
  if (field == "response_note" && blank && identical(read_entity(con, "failure_mode", failure_id)$response_state, "accepted")) {
    refuse("An accepted failure needs its reason. Change the response first to clear it.")
  }
  write_entity(con, "failure_mode", failure_id, stats::setNames(list(if (blank) NA_character_ else trimws(value)), field), author)
}

link_failure <- function(con, failure_id, link_type, target, author = "engineer") {
  if (!link_type %in% names(FAILURE_LINK_TARGETS)) refuse("Unknown failure link: ", link_type, ".")
  parts <- strsplit(target, "|", fixed = TRUE)[[1]]
  if (length(parts) != 2L) refuse("Choose what it links to.")
  if (!parts[1] %in% FAILURE_LINK_TARGETS[[link_type]]) {
    refuse(if (link_type == "detected_by") "Detected by" else "Handled by", " takes a ",
           paste(FAILURE_TARGET_LABELS[FAILURE_LINK_TARGETS[[link_type]]], collapse = ", "), ".")
  }
  write_link(con, fields = list(link_type = link_type, source_kind = "failure_mode", source_id = failure_id,
                                target_kind = parts[1], target_id = parts[2]), author = author)
}

waive_failure_check <- function(con, rule, subject_kind, subject_id, rationale, author = "engineer") {
  if (!rule %in% WAIVABLE_RULES) refuse("Only the missing-case checks can be set aside.")
  if (is.null(rationale) || !nzchar(trimws(rationale))) refuse("Say why this check doesn't apply.")
  write_entity(con, "failure_waiver", paste(rule, subject_kind, subject_id, sep = "|"), list(rationale = trimws(rationale)), author)
}

failure_waivers <- function(con) DBI::dbGetQuery(con, "SELECT rule, subject_kind, subject_id, rationale FROM failure_waiver ORDER BY rule, subject_id")

# The six checks (doc §3). Each finding names its rule, what it's about, and for the
# missing-case rules a suggested failure mode (`suggest_*`).
failure_findings <- function(con) {
  modes <- DBI::dbGetQuery(con, "SELECT id, subject_kind, subject_id, field_name, cause_kind, response_state FROM failure_mode")
  links <- DBI::dbGetQuery(con, "SELECT source_id, link_type, target_kind, target_id FROM link WHERE source_kind = 'failure_mode'")
  waivers <- failure_waivers(con)
  waived <- function(rule, kind, id) paste(rule, kind, id) %in% paste(waivers$rule, waivers$subject_kind, waivers$subject_id)
  covered <- function(causes, interface_id, port_id = NA_character_, field = NULL) {
    hit <- modes$cause_kind %in% causes &
      ((modes$subject_kind == "interface" & modes$subject_id == interface_id) | (modes$subject_kind == "port" & modes$subject_id %in% port_id))
    if (!is.null(field)) hit <- hit & (is.na(modes$field_name) | modes$field_name == field)
    any(hit)
  }
  finding <- function(rule, subject_kind, subject_id, text, failure_id = NA_character_, suggest_subject = NA_character_,
                      suggest_cause = NA_character_, suggest_field = NA_character_) {
    data.frame(rule = rule, subject_kind = subject_kind, subject_id = subject_id, text = text, failure_id = failure_id,
               suggest_subject = suggest_subject, suggest_cause = suggest_cause, suggest_field = suggest_field)
  }
  out <- list()
  subject <- function(i) failure_subject_label(con, modes$subject_kind[i], modes$subject_id[i], modes$field_name[i])

  # 1 and 2: responses.
  for (i in seq_len(nrow(modes))) {
    label <- sprintf("%s %s (%s)", subject(i), tolower(FAILURE_CAUSES[[modes$cause_kind[i]]]), modes$id[i])
    if (modes$response_state[i] %in% c("placeholder", "unhandled")) {
      out[[length(out) + 1L]] <- finding("open_response", "failure_mode", modes$id[i],
                                         sprintf("%s: %s.", label, if (modes$response_state[i] == "unhandled") "nothing handles it" else "the response isn't decided"), modes$id[i])
    }
    handlers <- links$source_id == modes$id[i] & links$link_type == "handled_by"
    if (modes$response_state[i] == "handled" && !any(handlers)) {
      out[[length(out) + 1L]] <- finding("handled_unlinked", "failure_mode", modes$id[i],
                                         sprintf("%s is marked handled, but no transition, requirement or component handles it.", label), modes$id[i])
    }
  }

  connections <- DBI::dbGetQuery(con, "
    SELECT c.id, i.id AS interface_id, i.name AS interface, i.flow_type, tp.id AS target_port, tp.name AS target_port_name,
           se.name AS source, se.domain AS source_domain, se.kind AS source_kind,
           te.id AS target_id, te.name AS target, te.domain AS target_domain, te.kind AS target_kind, te.review_state AS target_review
      FROM connector c JOIN port sp ON sp.id = c.source_port_id JOIN port tp ON tp.id = c.target_port_id
      JOIN element se ON se.id = sp.element_id JOIN element te ON te.id = tp.element_id
      JOIN interface i ON i.id = sp.interface_id WHERE i.layer = 'physical' ORDER BY c.id")
  # 3: software that receives a signal from hardware.
  signal <- connections[connections$flow_type == "data" & connections$target_domain %in% c("sw", "hw_sw") &
                          connections$source_domain %in% c("hw", "hw_sw") & connections$target_kind == "component", , drop = FALSE]
  for (i in seq_len(nrow(signal))) {
    row <- signal[i, ]
    if (covered(c("lost", "stale"), row$interface_id, row$target_port) || waived("missing_signal_loss", "port", row$target_port)) next
    out[[length(out) + 1L]] <- finding("missing_signal_loss", "port", row$target_port,
                                       sprintf("%s receives %s from %s, but nothing records what happens if it's lost or stale.", row$target, row$interface, row$source),
                                       suggest_subject = paste0("port|", row$target_port), suggest_cause = "lost")
  }
  # 4: power feeds, and parts that dissipate power.
  power <- connections[connections$flow_type == "energy" & connections$target_kind == "component" & connections$target_review == "reviewed", , drop = FALSE]
  for (i in seq_len(nrow(power))) {
    row <- power[i, ]
    if (covered(c("lost", "failed", "open_circuit", "short_circuit"), row$interface_id, row$target_port) || waived("missing_power_loss", "port", row$target_port)) next
    out[[length(out) + 1L]] <- finding("missing_power_loss", "port", row$target_port,
                                       sprintf("%s is fed %s by %s, but nothing records what happens if that's lost.", row$target, row$interface, row$source),
                                       suggest_subject = paste0("port|", row$target_port), suggest_cause = "lost")
  }
  hot <- DBI::dbGetQuery(con, "
    SELECT DISTINCT e.id, e.name FROM element e JOIN property_value pv ON pv.owner_kind = 'element' AND pv.owner_id = e.id
      JOIN property_def d ON d.id = pv.property_def_id
     WHERE e.kind = 'component' AND e.domain IN ('hw', 'hw_sw') AND e.review_state = 'reviewed'
       AND d.name = 'power' AND pv.value_state = 'set' AND pv.num_value > 0 ORDER BY e.sort_order, e.name")
  for (i in seq_len(nrow(hot))) {
    has <- any(modes$subject_kind == "element" & modes$subject_id == hot$id[i] & modes$cause_kind == "over_temperature")
    if (has || waived("missing_over_temperature", "element", hot$id[i])) next
    out[[length(out) + 1L]] <- finding("missing_over_temperature", "element", hot$id[i],
                                       sprintf("%s draws power, but nothing records what happens if it overheats.", hot$name[i]),
                                       suggest_subject = paste0("element|", hot$id[i]), suggest_cause = "over_temperature")
  }
  # 5: fields with a set range on an interface something receives.
  fields <- DBI::dbGetQuery(con, "
    SELECT f.id, f.name, i.id AS interface_id, i.name AS interface FROM interface_field f JOIN interface i ON i.id = f.interface_id
     WHERE f.range_state = 'set' AND i.layer = 'physical'
       AND EXISTS (SELECT 1 FROM connector c JOIN port p ON p.id = c.source_port_id WHERE p.interface_id = i.id)
     ORDER BY i.name, f.sort_order, f.name")
  for (i in seq_len(nrow(fields))) {
    row <- fields[i, ]
    if (covered("out_of_range", row$interface_id, field = row$name) || waived("missing_out_of_range", "interface_field", row$id)) next
    out[[length(out) + 1L]] <- finding("missing_out_of_range", "interface_field", row$id,
                                       sprintf("%s.%s has a range, but nothing records what happens when a value falls outside it.", row$interface, row$name),
                                       suggest_subject = paste0("interface|", row$interface_id), suggest_cause = "out_of_range", suggest_field = row$name)
  }
  # 6: handled by a requirement nothing verifies.
  handled_reqs <- links[links$link_type == "handled_by" & links$target_kind == "requirement", , drop = FALSE]
  for (i in seq_len(nrow(handled_reqs))) {
    verified <- DBI::dbGetQuery(con, "SELECT 1 FROM link WHERE link_type = 'verified_by' AND source_kind = 'requirement' AND source_id = ?",
                                params = list(handled_reqs$target_id[i]))
    if (nrow(verified) > 0L) next
    out[[length(out) + 1L]] <- finding("handled_unverified", "failure_mode", handled_reqs$source_id[i],
                                       sprintf("%s is handled by %s, which nothing verifies yet.", handled_reqs$source_id[i], handled_reqs$target_id[i]),
                                       handled_reqs$source_id[i])
  }
  do.call(rbind, c(list(finding(character(0), character(0), character(0), character(0), character(0), character(0), character(0), character(0))), out))
}
