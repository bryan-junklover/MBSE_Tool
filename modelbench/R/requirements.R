# Requirement workflow: committing, drafts, what the Specification shows, and imports.

# A coach counts when the light check runs on every edit and its route can actually
# be used. With none, commit runs on the gate alone (BUILD_PLAN §5: no key, no lockout).
coach_configured <- function(con) {
  route <- DBI::dbGetQuery(con, "SELECT * FROM task_route WHERE task = 'req_light_check'")
  if (nrow(route) == 0L || route$trigger_mode != "every_edit" || is.na(route$provider_id)) return(FALSE)
  provider <- DBI::dbGetQuery(con, "SELECT * FROM provider WHERE id = ? AND enabled = 1", params = list(route$provider_id))
  if (nrow(provider) == 0L || is.na(route$model)) return(FALSE)
  provider$is_local == 1L || provider_has_key(provider$id)
}

# Whether Commit is on, and why not. A review only counts for the exact text it
# assessed: "some assessment exists" is never enough (BUILD_PLAN §5). If the coach
# couldn't run on this text, it falls back to the gate alone, so a stopped server
# never locks anyone out.
commit_state <- function(gate, current_text, configured = FALSE, assessed_text = NA_character_, coach_failed_text = NA_character_) {
  if (!nzchar(trimws(current_text))) return(list(enabled = FALSE, reason = "Write a requirement to commit it."))
  if (!isTRUE(gate$passes_gate)) {
    return(list(enabled = FALSE, reason = paste(gate_blockers(gate), collapse = " ")))
  }
  if (!configured) {
    return(list(enabled = TRUE, reason = "No coach is configured, so this commits on the gate alone and is marked uncoached."))
  }
  if (identical(assessed_text, current_text)) {
    return(list(enabled = TRUE, reason = "The coach's review is current for this exact text. Commit re-runs the gate."))
  }
  if (identical(coach_failed_text, current_text)) {
    return(list(enabled = TRUE, reason = "The coach couldn't run, so this commits on the gate alone and is marked uncoached."))
  }
  list(enabled = FALSE, reason = "Waiting for the coach to review this exact text.")
}

gate_fields <- function(gate) {
  list(text = gate$text, gate_score = as.integer(gate$score), gate_pass = as.integer(gate$passes_gate),
       gate_json = canonical_json(list(ledger = lapply(seq_len(nrow(gate$ledger)), function(i) as.list(gate$ledger[i, ])),
                                       banned = as.list(gate$banned_words_found),
                                       placeholders = as.list(gate$placeholders_found))),
       actor_element_id = gate$actor_element_id)
}

next_sort_order <- function(con, parent_id) {
  parent <- if (is.null(parent_id) || is.na(parent_id)) NA_character_ else parent_id
  DBI::dbGetQuery(con, "SELECT coalesce(max(sort_order), 0) + 1 AS n FROM requirement WHERE parent_id IS ?",
                  params = list(parent))$n
}

save_requirement_text <- function(con, gate, id, fields, author, note) {
  existing <- if (is.null(id)) NULL else read_entity(con, "requirement", id)
  if (is.null(existing)) {
    if (is.null(fields$level)) fields$level <- "system"
    fields$sort_order <- next_sort_order(con, fields$parent_id)
  }
  in_tx(con, {
    written <- write_requirement(con, id, c(gate_fields(gate), fields), author, note)
    write_requirement_terms(con, written$id, gate$terms)
    written
  })
}

# Commits a requirement. Core re-runs the gate whatever the UI thought, and the
# database refuses placeholders on its own (BUILD_PLAN Phase 1).
commit_requirement <- function(con, text, id = NULL, fields = list(), author = "engineer",
                               note = NULL, coach_state = "uncoached", assessed_text = NULL, coach_call_id = NULL) {
  gate <- check_requirement(text, model_terms(con))
  if (!gate$passes_gate) refuse("Can't commit: ", paste(gate_blockers(gate), collapse = " "))
  # "Reviewed" only stands for the exact words the coach saw, whatever the button thought.
  if (identical(coach_state, "reviewed")) {
    if (!identical(stringr::str_squish(assessed_text), gate$text)) refuse("Can't commit: the coach's review is for different text.")
    fields$coach_call_id <- coach_call_id
  }
  save_requirement_text(con, gate, id, c(fields, list(status = "committed", coach_state = coach_state)), author, note)
}

# Drafts may fail the gate and hold placeholders. A committed requirement can't go back to draft.
save_draft <- function(con, text, id = NULL, fields = list(), author = "engineer", note = NULL) {
  if (is.null(text) || !nzchar(trimws(text))) refuse("Write something before saving a draft.")
  if (!is.null(id)) {
    existing <- read_entity(con, "requirement", id)
    if (!is.null(existing) && identical(existing$status, "committed")) {
      refuse("Can't save ", id, " as a draft: it's committed. Commit the new text to revise it.")
    }
  }
  gate <- check_requirement(text, model_terms(con))
  save_requirement_text(con, gate, id, c(fields, list(status = "draft")), author, note)
}

# Inspector edits: only the fields that actually changed make a revision.
# Retires a committed requirement. It leaves the specification, the gate counts and
# Health, but its history and baselines keep it, and Undo brings it back.
deprecate_requirement <- function(con, id, reason, author = "engineer") {
  row <- read_entity(con, "requirement", id)
  if (is.null(row)) refuse("There's no requirement ", id, ".")
  if (row$status != "committed") refuse(id, " isn't committed. A draft can be deleted instead.")
  if (is.null(reason) || !nzchar(trimws(reason))) refuse("Say why ", id, " is being deprecated.")
  write_requirement(con, id, list(status = "deprecated"), author, paste("Deprecated:", trimws(reason)))
}

# Traces between requirements outside the parent tree: one derived from, or refining, another.
link_requirements <- function(con, link_type, source_id, target_id, author = "engineer") {
  if (!link_type %in% c("derives_from", "refines")) refuse("Requirements link as derives from or refines.")
  if (identical(source_id, target_id)) refuse("A requirement can't trace to itself.")
  if (is.null(read_entity(con, "requirement", target_id))) refuse("That requirement isn't in the model.")
  write_link(con, fields = list(link_type = link_type, source_kind = "requirement", source_id = source_id,
                                target_kind = "requirement", target_id = target_id), author = author)
}

requirement_links <- function(con, requirement_id) {
  DBI::dbGetQuery(con, "SELECT l.id, l.link_type, l.target_id, r.short_title FROM link l JOIN requirement r ON r.id = l.target_id
                         WHERE l.source_kind = 'requirement' AND l.source_id = ? AND l.target_kind = 'requirement'
                           AND l.link_type IN ('derives_from', 'refines') ORDER BY l.link_type, l.target_id", params = list(requirement_id))
}

LEVEL_ORDER <- c(stakeholder = 1L, system = 2L, subsystem = 3L)

# Records the coach's review of a committed requirement's exact text, without touching the
# text, so its evidence stays current (review F5).
mark_requirement_coached <- function(con, id, assessed_text, call_id = NULL, author = "engineer") {
  row <- read_entity(con, "requirement", id)
  if (is.null(row)) refuse("There's no requirement ", id, ".")
  if (!identical(stringr::str_squish(row$text), stringr::str_squish(assessed_text))) refuse("The coach reviewed different text, so ", id, " isn't marked coached.")
  write_requirement(con, id, list(coach_state = "reviewed", coach_call_id = if (is.null(call_id)) NA else call_id), author, "Coached from Specification")
}

# Drafts were never part of the specification, so they can simply go.
delete_draft_requirement <- function(con, id, author = "engineer") {
  row <- read_entity(con, "requirement", id)
  if (is.null(row)) refuse("There's no requirement ", id, ".")
  if (row$status != "draft") refuse(id, " is committed, so it's deprecated rather than deleted. That keeps its history.")
  delete_entity(con, "requirement", id, author, "Draft deleted")
}

save_requirement_details <- function(con, id, fields, author = "engineer") {
  allowed <- c("short_title", "parent_id", "level", "req_type", "rationale", "verification_method")
  unknown <- setdiff(names(fields), allowed)
  if (length(unknown) > 0L) stop("Not an inspector field: ", paste(unknown, collapse = ", "), call. = FALSE)
  current <- read_entity(con, "requirement", id)
  blank_to_na <- function(x) if (is.null(x) || (is.character(x) && !nzchar(trimws(x)))) NA else x
  fields <- lapply(fields, blank_to_na)
  changed <- Filter(function(name) {
    was <- current[[name]]
    now <- fields[[name]]
    !(identical(is.na(was), is.na(now)) && (is.na(was) || identical(as.character(was), as.character(now))))
  }, names(fields))
  if (length(changed) == 0L) return(NULL)
  write_requirement(con, id, fields[changed], author, "Details edited")
}

# Depth-first by parent, then sort order: one row per requirement, with its depth.
requirement_outline <- function(con) {
  rows <- DBI::dbGetQuery(con, "
    SELECT id, parent_id, short_title, status, gate_score, gate_pass, sort_order
      FROM requirement WHERE status <> 'deprecated' ORDER BY sort_order, id")
  walk <- new.env()
  walk$order <- integer(0)
  walk$depth <- integer(nrow(rows))
  visit <- function(indexes, depth) {
    for (i in indexes) {
      walk$depth[i] <- depth
      walk$order <- c(walk$order, i)
      visit(which(rows$parent_id %in% rows$id[i]), depth + 1L)
    }
  }
  visit(which(is.na(rows$parent_id) | !rows$parent_id %in% rows$id), 0L)
  rows$depth <- walk$depth
  ordered <- walk$order
  out <- rows[ordered, , drop = FALSE]
  rownames(out) <- NULL
  out
}

requirement_table <- function(con) {
  rows <- DBI::dbGetQuery(con, "
    SELECT r.id, r.text, r.short_title, r.req_type, r.status, r.verification_method, r.parent_id, r.level,
           r.rationale, r.gate_score, r.gate_pass, r.coach_state,
           (SELECT group_concat(e.name, ', ') FROM link l JOIN element e ON e.id = l.target_id
             WHERE l.link_type = 'satisfied_by' AND l.source_kind = 'requirement' AND l.source_id = r.id
               AND l.target_kind = 'element') AS allocated_to
      FROM requirement r WHERE r.status <> 'deprecated' ORDER BY r.id")
  # Scored against the model as it is now, not as it was when saved (review C9).
  terms <- model_terms(con)
  live <- lapply(rows$text, function(text) check_requirement(text, terms))
  rows$gate_score <- vapply(live, function(gate) as.integer(gate$score), 0L)
  rows$gate_pass <- vapply(live, function(gate) as.integer(isTRUE(gate$passes_gate)), 0L)
  rows
}

filter_requirements <- function(rows, search = "", type = "all", low_first = FALSE) {
  if (nzchar(trimws(search))) {
    needle <- tolower(trimws(search))
    rows <- rows[grepl(needle, tolower(rows$id), fixed = TRUE) | grepl(needle, tolower(rows$text), fixed = TRUE), , drop = FALSE]
  }
  if (!identical(type, "all")) rows <- rows[rows$req_type %in% type, , drop = FALSE]
  if (low_first) rows <- rows[order(rows$gate_score, rows$id, na.last = FALSE), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# Findings the gate can state about a saved requirement, in plain words.
requirement_findings <- function(row, dictionary = EMPTY_TERMS, parent_level = NULL) {
  gate <- check_requirement(row$text, dictionary)
  findings <- gate_blockers(gate)
  if (isTRUE(gate$warnings$multiple_requirements) && !isTRUE(gate$warnings$multiple_shall)) {
    findings <- c(findings, "Bundles more than one concept joined by \"and\".")
  }
  if (is.na(row$verification_method)) findings <- c(findings, "No verification method recorded.")
  if (is.na(row$rationale)) findings <- c(findings, "No rationale recorded.")
  if (!is.null(parent_level) && !is.na(parent_level) && !is.na(row$level) && LEVEL_ORDER[[row$level]] < LEVEL_ORDER[[parent_level]]) {
    findings <- c(findings, sprintf("A %s requirement sits under a %s one: a child is usually at its parent's level or below.", row$level, parent_level))
  }
  list(score = as.integer(gate$score), findings = findings)
}

# --- Terms panel actions: each creates its thing with the value as a placeholder ---

term_to_snake <- function(term) gsub(" +", "_", trimws(gsub("[^a-z0-9]+", " ", tolower(term))))

term_to_title <- function(term) {
  words <- strsplit(trimws(term), " +")[[1]]
  paste0(toupper(substr(words, 1L, 1L)), substr(words, 2L, nchar(words)), collapse = " ")
}

add_term_as_property <- function(con, term, element_id, unit = NULL, author = "engineer") {
  name <- term_to_snake(term)
  if (!is.null(unit) && !nzchar(unit)) unit <- NULL
  with_change_set(con, function(change_set) {
    def <- DBI::dbGetQuery(con, "SELECT id, unit FROM property_def WHERE name = ? AND stereotype_id IS NULL", params = list(name))
    def_id <- if (nrow(def) > 0L) {
      def$id[1]
    } else {
      write_property_def(con, fields = list(name = name, label = term_to_title(term),
                                            value_type = "number", unit = unit),
                         author = author, change_set = change_set)$id
    }
    held <- DBI::dbGetQuery(con, "SELECT id FROM property_value WHERE owner_kind = 'element' AND owner_id = ? AND property_def_id = ?",
                            params = list(element_id, def_id))
    if (nrow(held) > 0L) refuse(term_to_title(term), " is already a property of ", element_id, ".")
    write_property_value(con, fields = list(owner_kind = "element", owner_id = element_id, property_def_id = def_id,
                                            value_state = "placeholder", unit = unit),
                         author = author, note = "Added from a requirement term", change_set = change_set)
  })
}

add_term_as_field <- function(con, term, interface_id, value_type = "float32", unit = NULL, author = "engineer") {
  if (!is.null(unit) && !nzchar(unit)) unit <- NULL
  next_order <- DBI::dbGetQuery(con, "SELECT coalesce(max(sort_order), 0) + 1 AS n FROM interface_field WHERE interface_id = ?",
                                params = list(interface_id))$n
  write_interface_field(con, fields = list(interface_id = interface_id, name = term_to_snake(term), value_type = value_type,
                                           unit = unit, range_state = "placeholder", sort_order = next_order),
                        author = author, note = "Added from a requirement term")
}

add_term_as_component <- function(con, term, parent_id = NULL, author = "engineer") {
  write_element(con, fields = list(kind = "component", name = term_to_title(term), parent_id = parent_id),
                author = author, note = "Added from a requirement term")
}

mark_not_model_term <- function(con, term) upsert_glossary(con, normalize_term(term))

# --- Import ---

IMPORT_TEXT_COLUMNS <- c("text", "requirement", "requirement_text", "requirement text", "description", "statement")

# Reads csv, xlsx or txt into rows of (text, raw_json). `name` carries the original
# file name, since an upload arrives under a temporary one.
read_import_file <- function(path, name = basename(path)) {
  extension <- tolower(tools::file_ext(name))
  table <- switch(extension,
                  txt = data.frame(text = readLines(path, warn = FALSE, encoding = "UTF-8"), stringsAsFactors = FALSE),
                  csv = utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
                  xlsx = as.data.frame(readxl::read_excel(path, col_types = "text")),
                  refuse("Can't import a .", extension, " file. Use csv, xlsx or txt."))
  if (nrow(table) == 0L || ncol(table) == 0L) refuse("There are no rows to import in ", name, ".")
  column <- match(IMPORT_TEXT_COLUMNS, tolower(trimws(names(table))))
  column <- column[!is.na(column)]
  text_column <- if (length(column) > 0L) column[1] else which(vapply(table, is.character, TRUE))[1]
  if (is.na(text_column)) refuse("No text column found in ", name, ".")
  texts <- trimws(as.character(table[[text_column]]))
  others <- table[, -text_column, drop = FALSE]
  keep <- !is.na(texts) & nzchar(texts)
  data.frame(
    row_no = which(keep),
    text = texts[keep],
    raw_json = vapply(which(keep), function(i) canonical_json(as.list(others[i, , drop = FALSE])), ""),
    stringsAsFactors = FALSE
  )
}

# Rows passing the gate become drafts; the engineer still commits each one.
# The rest wait in the triage queue, one at a time.
# Columns an export writes, read back on import (review F1). Values that don't fit are left out.
import_fields <- function(raw_json, level) {
  raw <- tryCatch(jsonlite::fromJSON(raw_json, simplifyVector = TRUE), error = function(e) list())
  names(raw) <- gsub("[^a-z]+", "_", tolower(trimws(names(raw))))
  pick <- function(keys, allowed = NULL) {
    for (key in keys) {
      value <- raw[[key]]
      if (!is.null(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))) {
        value <- trimws(as.character(value))
        if (is.null(allowed) || tolower(value) %in% allowed) return(if (is.null(allowed)) value else tolower(value))
      }
    }
    NULL
  }
  fields <- list(level = pick("level", c("stakeholder", "system", "subsystem")),
                 req_type = pick(c("type", "req_type"), c("functional", "performance", "physical", "interface", "nfr", "constraint")),
                 verification_method = pick(c("verify_by", "verification_method", "verification"), c("test", "analysis", "inspection", "demonstration", "simulation")),
                 short_title = pick(c("short_title", "title")),
                 rationale = pick("rationale"))
  if (is.null(fields$level)) fields$level <- level
  Filter(Negate(is.null), fields)
}

# The only thing holding a row back is a number still to decide: that's a fine draft.
only_placeholder_blocks <- function(gate) {
  blockers <- gate_blockers(gate)
  length(blockers) > 0L && all(grepl("placeholder", blockers, ignore.case = TRUE))
}

import_requirements <- function(con, rows, source_name, level = "system", author = "engineer") {
  dictionary <- model_terms(con)
  existing <- stringr::str_squish(DBI::dbGetQuery(con, "SELECT text FROM requirement WHERE status <> 'deprecated'")$text)
  with_change_set(con, function(change_set) {
    added <- character(0)
    queued <- 0L
    skipped <- 0L
    for (i in seq_len(nrow(rows))) {
      # A row already in the specification, as when an export comes back, isn't added again.
      if (stringr::str_squish(rows$text[i]) %in% existing) {
        skipped <- skipped + 1L
        next
      }
      gate <- check_requirement(rows$text[i], dictionary)
      if (gate$passes_gate || only_placeholder_blocks(gate)) {
        fields <- utils::modifyList(list(status = "draft", short_title = NULL), import_fields(rows$raw_json[i], level))
        fields <- fields[intersect(names(fields), c("status", "level", "req_type", "verification_method", "short_title", "rationale"))]
        written <- save_requirement_text(con, gate, NULL, fields, author, paste("Imported from", source_name))
        added <- c(added, written$id)
        existing <- c(existing, stringr::str_squish(rows$text[i]))
      } else {
        insert_triage_row(con, source_name, rows$row_no[i], rows$text[i], rows$raw_json[i],
                          canonical_json(as.list(gate_blockers(gate))))
        queued <- queued + 1L
      }
    }
    list(added = added, queued = queued, skipped = skipped, change_set = change_set)
  })
}

triage_pending <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM import_triage WHERE status = 'pending' ORDER BY id")
}

# Re-gates the edited text. Refused until it passes; then it joins the drafts.
triage_fix <- function(con, triage_id, text, level = "system", author = "engineer") {
  row <- DBI::dbGetQuery(con, "SELECT * FROM import_triage WHERE id = ?", params = list(triage_id))
  if (nrow(row) == 0L) stop("No triage row ", triage_id, call. = FALSE)
  gate <- check_requirement(text, model_terms(con))
  if (!gate$passes_gate && !only_placeholder_blocks(gate)) refuse("Still failing the gate: ", paste(gate_blockers(gate), collapse = " "))
  in_tx(con, {
    written <- save_requirement_text(con, gate, NULL, list(status = "draft", level = level), author,
                                     paste("Imported from", row$source_name, "after a fix"))
    update_triage_row(con, triage_id, "fixed", written$id)
    written
  })
}

triage_skip <- function(con, triage_id) update_triage_row(con, triage_id, "skipped")

# --- Settings and project facts ---

get_setting <- function(con, key, default = NA_character_) {
  value <- DBI::dbGetQuery(con, "SELECT value FROM project_meta WHERE key = ?", params = list(key))$value
  if (length(value) == 1L) value else default
}

# Prefixes that already mean something else can't be reused for another kind of id.
set_id_prefix <- function(con, what, prefix) {
  if (!what %in% names(CONFIGURABLE_PREFIXES)) stop("Not a configurable prefix: ", what, call. = FALSE)
  prefix <- toupper(trimws(prefix))
  if (!grepl("^[A-Z]{1,6}$", prefix)) refuse("A prefix is 1 to 6 letters.")
  reserved <- setdiff(names(ID_FORMATS), CONFIGURABLE_PREFIXES)
  if (prefix %in% reserved) refuse(prefix, " is already used for another kind of id.")
  set_setting(con, paste0("id_prefix_", what), prefix)
}

status_counts <- function(con) {
  list(unreviewed = DBI::dbGetQuery(con, "SELECT count(*) AS n FROM v_unreviewed")$n,
       placeholders = DBI::dbGetQuery(con, "SELECT count(*) AS n FROM v_placeholders")$n)
}

# "Survey UAV · working copy · based on BL-03"
project_context <- function(con) {
  base <- DBI::dbGetQuery(con, "SELECT id FROM baseline WHERE is_safety = 0 ORDER BY created_at DESC, id DESC LIMIT 1")$id
  parts <- c(get_setting(con, "project_name", "Untitled project"), "working copy",
             if (length(base) == 1L) paste("based on", base))
  paste(parts, collapse = " · ")
}
