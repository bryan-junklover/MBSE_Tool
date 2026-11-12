# The AI as a sense check on failure modes (docs/VIEWPOINTS_AND_FAILURES.md §5): it
# suggests failures nobody has recorded. The checks in failure.R find gaps in what was
# recorded; this looks for what nobody thought of. Suggestions land in the review queue,
# and the AI never says how the system responds or that anything is handled.

# NULL becomes the default; anything else is kept.
value_or <- function(x, default) if (is.null(x)) default else x

FAILURE_SUGGEST_SCHEMA <- s_object(items = s_array(s_object(
  subject = s_string(), field = s_string(), cause_kind = s_enum(names(FAILURE_CAUSES)),
  title = s_string(), cause = s_string(), local_effect = s_string(), system_effect = s_string(),
  confidence = s_enum(c("low", "medium", "high"))
)))

# Components, outside equipment and physical interfaces as numbered handles. The map stays
# here and is never sent (BUILD_PLAN §2).
failure_candidates <- function(con) {
  root <- default_structure_root(con)
  elements <- DBI::dbGetQuery(con, "SELECT 'element' AS kind, id, name,
                                            CASE WHEN kind = 'external' THEN 'outside the system'
                                                 ELSE coalesce(domain, 'domain not set') END AS detail
                                       FROM element WHERE kind IN ('component', 'external') ORDER BY kind, sort_order, name")
  elements <- elements[!elements$id %in% root, , drop = FALSE]
  interfaces <- DBI::dbGetQuery(con, "SELECT 'interface' AS kind, i.id, i.name,
                                              i.flow_type || coalesce(': ' || (SELECT group_concat(f.name, ', ') FROM interface_field f WHERE f.interface_id = i.id), '') AS detail
                                         FROM interface i WHERE i.layer = 'physical' ORDER BY i.name")
  rows <- rbind(elements, interfaces)
  rows$n <- if (nrow(rows) > 0L) sprintf("C%d", seq_len(nrow(rows))) else character(0)
  parents <- DBI::dbGetQuery(con, "SELECT id, parent_id FROM element")
  parent <- parents$parent_id[match(rows$id, parents$id)]
  inside <- rows$kind == "element" & !is.na(parent) & parent %in% rows$id
  rows$detail[inside] <- paste0(rows$detail[inside], ", part of ", rows$n[match(parent[inside], rows$id)])
  rows
}

failure_suggest_prompt <- function(con, focus = NULL) {
  candidates <- failure_candidates(con)
  handle <- function(kind, id) candidates$n[match(paste(kind, id), paste(candidates$kind, candidates$id))]
  connections <- DBI::dbGetQuery(con, "
    SELECT DISTINCT se.id AS source, te.id AS target, i.id AS interface FROM connector c
      JOIN port sp ON sp.id = c.source_port_id JOIN port tp ON tp.id = c.target_port_id
      JOIN element se ON se.id = sp.element_id JOIN element te ON te.id = tp.element_id
      JOIN interface i ON i.id = sp.interface_id WHERE i.layer = 'physical'")
  hosting <- DBI::dbGetQuery(con, "SELECT source_id, target_id FROM link WHERE link_type = 'hosted_on'")
  modes <- DBI::dbGetQuery(con, "SELECT f.subject_kind, f.subject_id, f.field_name, f.cause_kind, f.cause, p.element_id, p.interface_id
                                   FROM failure_mode f LEFT JOIN port p ON f.subject_kind = 'port' AND p.id = f.subject_id")
  recorded <- vapply(seq_len(nrow(modes)), function(i) {
    row <- modes[i, ]
    about <- switch(row$subject_kind,
                    element = handle("element", row$subject_id),
                    interface = paste0(handle("interface", row$subject_id), if (!is.na(row$field_name)) paste0(".", row$field_name) else ""),
                    port = sprintf("%s input from %s", handle("element", row$element_id), handle("interface", row$interface_id)),
                    "a state machine state")
    sprintf("- %s: %s%s", about, row$cause_kind, if (!is.na(row$cause)) paste0(" (", row$cause, ")") else "")
  }, "")
  lines <- if (nrow(candidates) > 0L) sprintf("%s %s (%s)", candidates$n, candidates$name, candidates$detail) else "(none yet)"
  scope <- get_setting(con, "scope_text", "")
  list(
    task = "failure_suggest", prompt_builder = "failure_suggest_prompt", prompt_version = "failure-suggest v1",
    system = paste(c(
      "You help a systems engineer find failure modes the model doesn't record yet. You suggest; the engineer decides.",
      COACH_POLICY,
      "Suggest failures nobody has recorded: don't repeat the recorded ones, or the same cause on the same part.",
      "Think about hardware (power, connectors, heat, wear, damage) and software (lost, stale, wrong or out-of-range data).",
      "Never say how the system responds, whether a failure is handled, or how likely or severe it is. Those are the engineer's.",
      "subject is the handle of the part or interface that fails, such as C2. Use field only for one field of an interface, else an empty string.",
      "title is a question about the system and ends with a question mark. cause, local_effect and system_effect are short plain sentences.",
      "Give at most six items, the most consequential first."
    ), collapse = "\n"),
    message = paste(c(
      paste("System scope:", if (nzchar(scope)) scope else "(not written yet)"),
      "Model content:", lines,
      "Connections (sender to receiver over interface):",
      if (nrow(connections) > 0L) {
        sprintf("- %s to %s over %s", handle("element", connections$source), handle("element", connections$target), handle("interface", connections$interface))
      } else {
        "(none)"
      },
      "Software and the hardware it runs on:",
      if (nrow(hosting) > 0L) sprintf("- %s runs on %s", handle("element", hosting$source_id), handle("element", hosting$target_id)) else "(none recorded)",
      "Failure modes already recorded:", if (length(recorded) > 0L) recorded else "(none yet)",
      if (!is.null(focus) && nzchar(focus)) paste("Look especially at:", focus)
    ), collapse = "\n"),
    schema = FAILURE_SUGGEST_SCHEMA, cache_text = NULL,
    candidates_json = canonical_json(lapply(seq_len(nrow(candidates)), function(i) as.list(candidates[i, c("n", "kind", "id", "name")]))),
    candidates = candidates
  )
}

# Maps handles back, drops what can't be placed or is already recorded, scrubs invented
# numbers, and stores each suggestion as a proposal. Returns the new proposal ids.
store_failure_answer <- function(con, prompt, result) {
  candidates <- prompt$candidates
  recorded <- DBI::dbGetQuery(con, "SELECT subject_kind, subject_id, cause_kind FROM failure_mode")
  open <- DBI::dbGetQuery(con, "SELECT target_kind, target_id, payload_json FROM ai_proposal WHERE proposal_kind = 'sketch_failure' AND status = 'open'")
  open_causes <- vapply(open$payload_json, function(json) value_or(jsonlite::fromJSON(json, simplifyVector = FALSE)$cause_kind, ""), "", USE.NAMES = FALSE)
  taken <- c(paste(recorded$subject_kind, recorded$subject_id, recorded$cause_kind), paste(open$target_kind, open$target_id, open_causes))
  ids <- character(0)
  for (item in result$value$items) {
    at <- match(trimws(item$subject), candidates$n)
    if (is.na(at)) {
      log_ai_drop(con, list(call = result$call_id, dropped = "failure subject", handle = item$subject))
      next
    }
    subject <- candidates[at, ]
    key <- paste(subject$kind, subject$id, item$cause_kind)
    if (key %in% taken) {
      log_ai_drop(con, list(call = result$call_id, dropped = "failure already recorded", handle = item$subject, cause = item$cause_kind))
      next
    }
    taken <- c(taken, key)
    field <- trimws(item$field)
    if (nzchar(field) && (subject$kind != "interface" || !field %in% interface_fields(con, subject$id)$name)) {
      log_ai_drop(con, list(call = result$call_id, dropped = "failure field", handle = item$subject, field = field))
      field <- ""
    }
    scrubbed <- scrub_tree(list(title = item$title, cause = item$cause, local_effect = item$local_effect, system_effect = item$system_effect), candidates$name)
    text <- scrubbed$value
    payload <- list(subject_kind = subject$kind, subject_id = subject$id, field_name = if (nzchar(field)) field else NULL,
                    subject_label = failure_subject_label(con, subject$kind, subject$id, if (nzchar(field)) field else NA_character_),
                    cause_kind = item$cause_kind, cause = text$cause, local_effect = text$local_effect, system_effect = text$system_effect)
    if (scrubbed$count > 0L) payload$`_invented` <- scrubbed$count
    if (!grepl("\\?\\s*$", text$title)) payload$`_warning` <- "The title isn't phrased as a question."
    ids <- c(ids, insert_proposal(con, list(
      task = "failure_suggest", proposal_kind = "sketch_failure", source = "ai", target_kind = subject$kind, target_id = subject$id,
      title = text$title, body = text$cause, payload_json = canonical_json(payload), confidence = item$confidence, llm_call_id = result$call_id
    )))
  }
  ids
}

run_failure_suggest_async <- function(con, focus = NULL) {
  safely_async(function() {
    prompt <- failure_suggest_prompt(con, focus)
    promises::then(run_ai_async(con, prompt), function(result) {
      if (DBI::dbIsValid(con) && isTRUE(result$ok)) result$proposal_ids <- store_failure_answer(con, prompt, result)
      result
    })
  })
}

run_failure_suggest <- function(con, focus = NULL) {
  prompt <- failure_suggest_prompt(con, focus)
  result <- run_ai(con, prompt)
  if (isTRUE(result$ok)) result$proposal_ids <- store_failure_answer(con, prompt, result)
  result
}

# Keeping a suggestion makes it an unreviewed AI failure mode with its response not
# decided. Reviewing it, with a note, is a separate step, as for every AI draft.
keep_failure_sketch <- function(con, proposal, note, author = "engineer") {
  payload <- jsonlite::fromJSON(proposal$payload_json, simplifyVector = FALSE)
  if (!nzchar(trimws(note))) refuse("Adding a suggestion needs a note: why this failure matters, in your own words.")
  kind <- value_or(payload$subject_kind, "")
  if (!kind %in% FAILURE_SUBJECT_KINDS || !polymorphic_exists(con, kind, value_or(payload$subject_id, ""))) {
    refuse("What this suggestion is about is no longer in the model.")
  }
  with_change_set(con, function(change_set) {
    written <- write_entity(con, "failure_mode", NULL, list(
      subject_kind = payload$subject_kind, subject_id = payload$subject_id, field_name = value_or(payload$field_name, NA_character_),
      cause_kind = payload$cause_kind, cause = value_or(payload$cause, NA_character_), local_effect = value_or(payload$local_effect, NA_character_),
      system_effect = value_or(payload$system_effect, NA_character_), response_state = "placeholder", origin = "ai", review_state = "unreviewed"
    ), author, "Added from an AI suggestion", change_set)
    update_proposal(con, proposal$id, list(status = "kept", engineer_note = trimws(note), created_entity_kind = "failure_mode", created_entity_id = written$id))
    written
  })
}
