# The AI features as async tasks. Each captures project and inputs when it starts, so a
# result always belongs to what was asked (BUILD_PLAN §2: snapshot at invoke).

# A synchronous throw becomes a resolved failure, so the session can't crash.
safely_async <- function(action) {
  promises::catch(
    tryCatch(promises::as.promise(action()), error = function(e) promises::promise_reject(e)),
    function(e) ai_result(FALSE, error = conditionMessage(e), kind = "other")
  )
}

coach_inputs <- function(con, draft, task, parent_id = NULL) {
  committed <- DBI::dbGetQuery(con, "SELECT text FROM requirement WHERE status = 'committed'")$text
  scope <- get_setting(con, "scope_text", "")
  parent <- if (is.null(parent_id) || !nzchar(parent_id)) NULL else read_entity(con, "requirement", parent_id)$text
  related <- if (task == "req_deep_check") related_requirements(draft, committed, 40L) else related_requirements(draft, committed, 12L)
  req_coach_prompt(draft, task, related, scope, parent)
}

decorate_coach_result <- function(con, draft, result) {
  result$text <- stringr::str_squish(draft)
  if (isTRUE(result$ok)) result$answer <- process_coach_answer(con, draft, result$value)
  result
}

# A cached review for this exact text, or NULL.
coach_from_cache <- function(con, draft, task = "req_light_check", parent_id = NULL) {
  hit <- ai_cache_lookup(con, coach_inputs(con, draft, task, parent_id))
  if (is.null(hit)) return(NULL)
  decorate_coach_result(con, draft, hit)
}

run_coach_async <- function(con, draft, task = "req_light_check", parent_id = NULL) {
  safely_async(function() {
    prompt <- coach_inputs(con, draft, task, parent_id)
    promises::then(run_ai_async(con, prompt), function(result) {
      if (!DBI::dbIsValid(con)) return(result)
      decorate_coach_result(con, draft, result)
    })
  })
}

run_coach <- function(con, draft, task = "req_light_check", parent_id = NULL) {
  decorate_coach_result(con, draft, run_ai(con, coach_inputs(con, draft, task, parent_id)))
}

# Gap and scope questions: stored as proposals. A title that isn't a question is asked
# for once more, then stored with a warning (handoff §5.3).
run_gaps_async <- function(con, task = "gap_inference", focus = NULL) {
  safely_async(function() {
    prompt <- gap_prompt(con, task, focus)
    store <- function(result) {
      if (!DBI::dbIsValid(con)) return(result)
      if (isTRUE(result$ok)) result$proposal_ids <- store_gap_answer(con, prompt, result)
      result
    }
    promises::then(run_ai_async(con, prompt), function(first) {
      if (!isTRUE(first$ok) || !gap_answer_needs_retry(first$value)) return(store(first))
      promises::then(run_ai_async(con, prompt), function(second) store(if (isTRUE(second$ok)) second else first))
    })
  })
}

run_gaps <- function(con, task = "gap_inference", focus = NULL) {
  prompt <- gap_prompt(con, task, focus)
  result <- run_ai(con, prompt)
  if (isTRUE(result$ok) && gap_answer_needs_retry(result$value)) {
    second <- run_ai(con, prompt)
    if (isTRUE(second$ok)) result <- second
  }
  if (isTRUE(result$ok)) result$proposal_ids <- store_gap_answer(con, prompt, result)
  result
}

# Trace and allocation questions, over numbered candidates ranked locally.
run_link_coach_async <- function(con, target_kind, target_id) {
  safely_async(function() {
    prompt <- link_coach_prompt(con, target_kind, target_id)
    promises::then(run_ai_async(con, prompt, prompt$target), function(result) {
      if (DBI::dbIsValid(con) && isTRUE(result$ok)) result$proposal_ids <- store_link_answer(con, prompt, result)
      result
    })
  })
}

run_link_coach <- function(con, target_kind, target_id) {
  prompt <- link_coach_prompt(con, target_kind, target_id)
  result <- run_ai(con, prompt, prompt$target)
  if (isTRUE(result$ok)) result$proposal_ids <- store_link_answer(con, prompt, result)
  result
}

# --- The review queue ---

proposal_list <- function(con, status = "open", sources = c("ai", "structural")) {
  marks <- paste(rep("?", length(sources)), collapse = ", ")
  rows <- DBI::dbGetQuery(con, sprintf("
    SELECT p.*, c.provider_id, c.model, c.prompt_version FROM ai_proposal p LEFT JOIN llm_call c ON c.id = p.llm_call_id
     WHERE p.status = ? AND p.source IN (%s) ORDER BY p.source = 'structural' DESC, p.id", marks), params = c(list(status), as.list(sources)))
  rows
}

dismiss_proposal <- function(con, id, reason) {
  if (!nzchar(trimws(reason))) refuse("Dismissing needs a reason.")
  update_proposal(con, id, list(status = "dismissed", dismiss_reason = trimws(reason)))
}

# Creates a sketch as an unreviewed draft, with its value as a placeholder.
keep_sketch <- function(con, id, note, author = "engineer") {
  if (!nzchar(trimws(note))) refuse("Adding a sketch needs a note: why the system needs this, in your own words.")
  proposal <- DBI::dbGetQuery(con, "SELECT * FROM ai_proposal WHERE id = ?", params = list(id))
  if (nrow(proposal) == 0L) refuse("That proposal no longer exists.")
  if (proposal$status != "open") refuse("That proposal has already been handled.")
  payload <- jsonlite::fromJSON(proposal$payload_json, simplifyVector = FALSE)
  kind <- switch(proposal$proposal_kind, sketch_element = "component", sketch_function = "function", refuse("That isn't a sketch of a model element."))
  if (!nzchar(trimws(if (is.null(payload$name)) "" else payload$name))) refuse("This sketch has no name, so it can't be added.")
  stereotype <- DBI::dbGetQuery(con, "SELECT id FROM stereotype WHERE name = ?", params = list(if (is.null(payload$stereotype)) "" else payload$stereotype))$id
  with_change_set(con, function(change_set) {
    written <- write_element(con, fields = list(
      kind = kind, name = payload$name, parent_id = if (kind == "component") default_structure_root(con) else NA_character_,
      stereotype_id = if (length(stereotype) == 1L) stereotype else NA_character_, origin = "ai", review_state = "unreviewed"
    ), author = author, note = "Added from an AI sketch", change_set = change_set)
    for (port in payload$ports) {
      interface <- DBI::dbGetQuery(con, "SELECT id FROM interface WHERE name = ?", params = list(port$interface))$id
      write_port(con, fields = list(element_id = written$id, name = port$name, direction = port$direction,
                                    interface_id = if (length(interface) == 1L) interface else NA_character_), author = author, change_set = change_set)
    }
    if (kind == "component") {
      mass <- DBI::dbGetQuery(con, "SELECT id, unit FROM property_def WHERE name = 'mass' AND stereotype_id IS NULL")
      if (nrow(mass) == 1L) {
        write_property_value(con, fields = list(owner_kind = "element", owner_id = written$id, property_def_id = mass$id,
                                                value_state = "placeholder", unit = mass$unit), author = author, change_set = change_set)
      }
    }
    update_proposal(con, id, list(status = "kept", engineer_note = trimws(note), created_entity_kind = "element", created_entity_id = written$id))
    written
  })
}

# "Start from this in Write": returns the draft text. The commit stays the engineer's.
start_from_sketch <- function(con, id) {
  proposal <- DBI::dbGetQuery(con, "SELECT payload_json FROM ai_proposal WHERE id = ?", params = list(id))
  if (nrow(proposal) == 0L) refuse("That proposal no longer exists.")
  draft <- jsonlite::fromJSON(proposal$payload_json, simplifyVector = FALSE)$draft
  if (is.null(draft) || !nzchar(draft)) refuse("This proposal has no starting draft.")
  update_proposal(con, id, list(status = "kept", engineer_note = "Started a draft from this in Write"))
  draft
}

# Structural gaps come from code. A dismissed one stays dismissed (BUILD_PLAN §5), and
# one that no longer applies is closed.
refresh_structural_gaps <- function(con) {
  ports <- DBI::dbGetQuery(con, "SELECT p.port_id AS id, e.name AS element, p.name FROM v_unconnected_ports p JOIN element e ON e.id = p.element_id")
  root <- default_structure_root(con)
  components <- DBI::dbGetQuery(con, "SELECT e.id, e.name FROM element e WHERE e.kind = 'component' AND e.review_state = 'reviewed' AND NOT EXISTS (
    SELECT 1 FROM link l WHERE l.link_type = 'satisfied_by' AND l.target_id = e.id)")
  components <- components[!components$id %in% root, , drop = FALSE]
  wanted <- rbind(
    data.frame(kind = rep("port", nrow(ports)), id = ports$id, title = sprintf("%s port %s has no connector", ports$element, ports$name), stringsAsFactors = FALSE),
    data.frame(kind = rep("element", nrow(components)), id = components$id, title = sprintf("%s has no linked requirements", components$name), stringsAsFactors = FALSE)
  )
  known <- DBI::dbGetQuery(con, "SELECT id, target_kind, target_id, status FROM ai_proposal WHERE source = 'structural'")
  for (i in seq_len(nrow(wanted))) {
    seen <- known$target_kind == wanted$kind[i] & known$target_id == wanted$id[i] & known$status %in% c("open", "dismissed", "kept")
    if (!any(seen)) {
      insert_proposal(con, list(task = "gap_inference", proposal_kind = "gap", source = "structural", target_kind = wanted$kind[i],
                                target_id = wanted$id[i], title = wanted$title[i], payload_json = "{}"))
    }
  }
  still <- paste(wanted$kind, wanted$id)
  stale <- known[known$status == "open" & !paste(known$target_kind, known$target_id) %in% still, , drop = FALSE]
  for (id in stale$id) update_proposal(con, id, list(status = "superseded"))
  invisible(NULL)
}

routes_table <- function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM task_route ORDER BY task")
}

set_redact_terms <- function(con, text) {
  terms <- trimws(strsplit(text, "\n", fixed = TRUE)[[1]])
  set_setting(con, "redact_terms", canonical_json(as.list(terms[nzchar(terms)])))
}
