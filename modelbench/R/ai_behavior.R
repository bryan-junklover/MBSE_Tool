# AI over behaviour: an unreviewed sketch from a description, and questions about a state
# machine. The model picks from numbered handles and never names an id or a number.

SKETCH_SCHEMA <- s_object(
  steps = s_array(s_object(kind = s_enum(c("message", "note")), from = s_string(), to = s_string(), interface = s_string(), label = s_string(), has_guard = s_enum(c("yes", "no")), has_timing = s_enum(c("yes", "no")))),
  nodes = s_array(s_object(key = s_string(), kind = s_enum(c("initial", "final", "action", "decision")), label = s_string(), performer = s_string())),
  edges = s_array(s_object(from = s_string(), to = s_string(), has_guard = s_enum(c("yes", "no")))),
  missing = s_array(s_object(title = s_string(), body = s_string()))
)
ANALYSIS_SCHEMA <- s_object(questions = s_array(s_object(title = s_string(), body = s_string(), confidence = s_enum(c("low", "medium", "high")))))

sketch_candidates <- function(con) {
  elements <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE kind IN ('component', 'external') AND review_state = 'reviewed' ORDER BY id")
  interfaces <- DBI::dbGetQuery(con, "SELECT id, name FROM interface ORDER BY id")
  elements$n <- if (nrow(elements) > 0L) sprintf("E%d", seq_len(nrow(elements))) else character(0)
  interfaces$n <- if (nrow(interfaces) > 0L) sprintf("IF%d", seq_len(nrow(interfaces))) else character(0)
  list(elements = elements, interfaces = interfaces)
}

behavior_sketch_prompt <- function(con, description, view = "sequence") {
  candidates <- sketch_candidates(con)
  shape <- if (view == "activity") {
    "Fill nodes and edges and leave steps empty. Each node has a short key that edges use, and performer is an element handle or empty."
  } else {
    "Fill steps and leave nodes and edges empty. from and to are element handles; interface is an interface handle or empty. A note's to is empty."
  }
  list(
    task = "behavior_sketch", prompt_builder = "behavior_sketch_prompt", prompt_version = "behavior-sketch v1",
    system = paste("You sketch a behaviour scenario for an engineer to review.", PLAIN_TEXT_RULE, shape,
                   "Use only the listed elements and interfaces, by handle. Don't state numbers, timings or guard conditions: set has_guard or has_timing and the engineer fills them in.",
                   "If the description needs an element or interface that isn't listed, don't invent it: add a question to missing, phrased as a question.", sep = "\n"),
    message = paste(c(
      paste("Description:", description),
      "Elements:", if (nrow(candidates$elements) > 0L) sprintf("%s %s", candidates$elements$n, candidates$elements$name) else "(none)",
      "Interfaces:", if (nrow(candidates$interfaces) > 0L) sprintf("%s %s", candidates$interfaces$n, candidates$interfaces$name) else "(none)"
    ), collapse = "\n"),
    schema = SKETCH_SCHEMA, cache_text = NULL, candidates = candidates, view = view, description = description,
    candidates_json = canonical_json(c(lapply(seq_len(nrow(candidates$elements)), function(i) list(n = candidates$elements$n[i], kind = "element", id = candidates$elements$id[i])),
                                       lapply(seq_len(nrow(candidates$interfaces)), function(i) list(n = candidates$interfaces$n[i], kind = "interface", id = candidates$interfaces$id[i]))))
  )
}

# Turns an answer into an unreviewed scenario. Anything it can't resolve is dropped and logged.
store_sketch_answer <- function(con, prompt, result, name) {
  candidates <- prompt$candidates
  allowed <- c(candidates$elements$name, candidates$interfaces$name)
  clean <- function(text) scrub_numbers(text, allowed)$text
  element_of <- function(handle) candidates$elements$id[match(trimws(handle), candidates$elements$n)]
  interface_of <- function(handle) candidates$interfaces$id[match(trimws(handle), candidates$interfaces$n)]
  drop <- function(what, handle) log_ai_drop(con, list(call = result$call_id, dropped = what, handle = handle))
  value <- result$value
  activity <- identical(prompt$view, "activity")

  with_change_set(con, function(change_set) {
    ai_fields <- list(origin = "ai", review_state = "unreviewed")
    write <- function(writer, fields) writer(con, fields = c(fields, ai_fields), author = "ai", note = "AI sketch", change_set = change_set)
    scenario <- write(write_scenario, list(name = name, description = prompt$description, has_sequence = as.integer(!activity), has_activity = as.integer(activity)))$id

    if (activity) {
      ids <- list()
      for (node in value$nodes) {
        performer <- element_of(node$performer)
        if (nzchar(trimws(node$performer)) && is.na(performer)) drop("performer", node$performer)
        written <- write(write_act_node, list(scenario_id = scenario, node_kind = node$kind, label = if (node$kind %in% c("initial", "final")) NA_character_ else clean(node$label), performer_element_id = performer))
        ids[[node$key]] <- written$id
      }
      for (edge in value$edges) {
        if (is.null(ids[[edge$from]]) || is.null(ids[[edge$to]])) {
          drop("edge", paste(edge$from, edge$to))
          next
        }
        write_act_edge(con, fields = list(scenario_id = scenario, from_node_id = ids[[edge$from]], to_node_id = ids[[edge$to]], guard = if (identical(edge$has_guard, "yes")) "[guard]" else NA_character_),
                       author = "ai", note = "AI sketch", change_set = change_set)
      }
    } else {
      lifelines <- new.env()
      lifeline <- function(element) {
        if (is.null(lifelines[[element]])) {
          lifelines[[element]] <- write_seq_lifeline(con, fields = list(scenario_id = scenario, element_id = element, sort_order = length(ls(lifelines)) + 1L), author = "ai", note = "AI sketch", change_set = change_set)$id
        }
        lifelines[[element]]
      }
      seq_no <- 0L
      for (step in value$steps) {
        from <- element_of(step$from)
        to <- if (step$kind == "note") from else element_of(step$to)
        if (is.na(from) || is.na(to)) {
          drop("step", paste(step$from, step$to))
          next
        }
        interface <- interface_of(step$interface)
        if (nzchar(trimws(step$interface)) && is.na(interface)) drop("interface", step$interface)
        seq_no <- seq_no + 1L
        write(write_seq_step, list(
          scenario_id = scenario, seq = seq_no, step_kind = if (step$kind == "note") "note" else if (from == to) "self" else "message",
          from_lifeline_id = lifeline(from), to_lifeline_id = lifeline(to), interface_id = interface, label = clean(step$label),
          guard = if (identical(step$has_guard, "yes")) "[guard]" else NA_character_, timing_state = if (identical(step$has_timing, "yes")) "placeholder" else "none"
        ))
      }
    }
    questions <- vapply(value$missing, function(item) {
      insert_proposal(con, list(task = prompt$task, proposal_kind = "question", source = "ai", target_kind = "scenario", target_id = scenario, title = clean(item$title), body = clean(item$body),
                                payload_json = "{}", llm_call_id = result$call_id))
    }, "")
    list(id = scenario, change_set = change_set, questions = questions)
  })
}

run_behavior_sketch_async <- function(con, description, name, view = "sequence") {
  safely_async(function() {
    prompt <- behavior_sketch_prompt(con, description, view)
    promises::then(run_ai_async(con, prompt), function(result) {
      if (DBI::dbIsValid(con) && isTRUE(result$ok)) result$sketch <- store_sketch_answer(con, prompt, result, name)
      result
    })
  })
}

behavior_analysis_prompt <- function(con, sm_id) {
  machine <- load_machine(con, sm_id)
  unhandled <- unhandled_pairs(con, sm_id)
  transitions <- machine$transitions
  lines <- sprintf("%s → %s on %s", state_name(machine, transitions$from_state_id), state_name(machine, transitions$to_state_id), machine$events$name[match(transitions$event_id, machine$events$id)])
  list(
    task = "behavior_analysis", prompt_builder = "behavior_analysis_prompt", prompt_version = "behavior-analysis v1",
    system = paste("You ask an engineer questions about a state machine's behaviour. Code has already listed the unhandled pairs; don't repeat them.", PLAIN_TEXT_RULE,
                   "Each title is one question, ending in a question mark. Don't state numbers. Don't propose a fix.", sep = "\n"),
    message = paste(c(
      paste("State machine:", machine$name),
      "States:", machine$states$name,
      "Events:", machine$events$name,
      "Transitions:", lines,
      "Already listed by code:", if (nrow(unhandled) > 0L) sprintf("%s × %s", unhandled$event_name, unhandled$state_name) else "(none)"
    ), collapse = "\n"),
    schema = ANALYSIS_SCHEMA, cache_text = paste(sm_id, model_hash(con), sep = "\n"), candidates_json = NULL, sm_id = sm_id, allowed = c(machine$states$name, machine$events$name)
  )
}

store_analysis_answer <- function(con, prompt, result) {
  vapply(result$value$questions, function(item) {
    insert_proposal(con, list(task = prompt$task, proposal_kind = "question", source = "ai", target_kind = "state_machine", target_id = prompt$sm_id,
                              title = scrub_numbers(item$title, prompt$allowed)$text, body = scrub_numbers(item$body, prompt$allowed)$text,
                              payload_json = "{}", confidence = item$confidence, llm_call_id = result$call_id))
  }, "")
}

run_behavior_analysis_async <- function(con, sm_id) {
  safely_async(function() {
    prompt <- behavior_analysis_prompt(con, sm_id)
    promises::then(run_ai_async(con, prompt), function(result) {
      if (DBI::dbIsValid(con) && isTRUE(result$ok)) result$proposal_ids <- store_analysis_answer(con, prompt, result)
      result
    })
  })
}
