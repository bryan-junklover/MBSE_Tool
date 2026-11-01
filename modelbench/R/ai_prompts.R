# One prompt builder per feature. Each returns what's sent, so the call and the "See the
# prompt" dialog can't drift. The authoring rules come from the gate's constants.

COACH_VERDICT <- s_enum(c("pass", "question", "none"))

COACH_SCHEMA <- s_object(
  criteria = s_object(singular = COACH_VERDICT, verifiable = COACH_VERDICT, design_independent = COACH_VERDICT,
                      conflicts = COACH_VERDICT, fits_scope = COACH_VERDICT),
  questions = s_array(s_object(text = s_string(), context = s_string())),
  example_rewrite = s_string(),
  conflicts = s_array(s_object(quoted_text = s_string(), explanation = s_string()))
)

GAP_SCHEMA <- s_object(items = s_array(s_object(
  kind = s_enum(c("question", "sketch_element", "sketch_function", "sketch_requirement")),
  title = s_string(), body = s_string(), target = s_string(), confidence = s_enum(c("low", "medium", "high")),
  name = s_string(), stereotype = s_string(), draft = s_string(),
  ports = s_array(s_object(name = s_string(), direction = s_enum(c("in", "out", "inout")), interface = s_string()))
)))

authoring_rules_block <- function() {
  paste(c(
    "A good requirement:",
    '- has exactly one "shall", with a named system or component before it, never a person or a pronoun;',
    sprintf("- is a complete sentence of at least %d words;", MIN_WORD_COUNT),
    "- states a measurable bound: a number with a unit, or a bounded phrase such as \"only in\" or \"limited to\";",
    "- states one concept, in the active voice;",
    sprintf("- avoids these words and phrases: %s.", paste(BANNED_WORDS, collapse = ", "))
  ), collapse = "\n")
}

COACH_POLICY <- paste(
  "You coach an engineer; you don't do their work. Ask questions about the system instead of giving instructions.",
  "Never supply a number the engineer didn't write: where a value is needed, write [value].",
  PLAIN_TEXT_RULE
)

req_coach_prompt <- function(draft, task = "req_light_check", related = character(0), scope = "", parent = NULL) {
  deep <- task == "req_deep_check"
  list_block <- function(label, values) {
    if (length(values) == 0L) return(character(0))
    c(label, paste0("- ", values))
  }
  message <- paste(c(
    if (nzchar(scope)) paste("System scope:", scope),
    if (!is.null(parent)) paste("Parent requirement:", parent),
    list_block(sprintf("Committed requirements to check against (%d, chosen as the most related):", length(related)), related),
    paste("Draft requirement:", draft)
  ), collapse = "\n")
  list(
    task = task, prompt_builder = "req_coach_prompt", prompt_version = if (deep) "req-coach-deep v1" else "req-coach v1",
    system = paste(c(
      "You review one draft requirement for a systems engineer.", COACH_POLICY, authoring_rules_block(),
      "Rate each criterion pass, question or none. Use none for conflicts when you found none.",
      if (deep) "Ask up to three questions, the most consequential first." else "Ask at most one question, and only if it matters.",
      "Give one example rewrite of the draft, or an empty string if it needs none. It's only an example.",
      "Report a conflict only by quoting the other requirement's text exactly."
    ), collapse = "\n"),
    message = message, schema = COACH_SCHEMA, cache_text = paste(task, draft, sep = "\n"),
    candidates_json = NULL
  )
}

# Model content as numbered handles. The map stays here and is never sent (BUILD_PLAN §2).
gap_candidates <- function(con) {
  elements <- DBI::dbGetQuery(con, "SELECT 'element' AS kind, id, name, kind AS detail FROM element ORDER BY kind, sort_order, name")
  interfaces <- DBI::dbGetQuery(con, "SELECT 'interface' AS kind, id, name, flow_type AS detail FROM interface WHERE layer = 'physical' ORDER BY name")
  rows <- rbind(elements, interfaces)
  rows$n <- if (nrow(rows) > 0L) sprintf("C%d", seq_len(nrow(rows))) else character(0)
  rows
}

gap_prompt <- function(con, task = "gap_inference", focus = NULL) {
  candidates <- gap_candidates(con)
  committed <- DBI::dbGetQuery(con, "SELECT text FROM requirement WHERE status = 'committed' ORDER BY id")$text
  scope <- get_setting(con, "scope_text", "")
  sketching <- task == "gap_inference"
  lines <- if (nrow(candidates) > 0L) sprintf("%s %s (%s)", candidates$n, candidates$name, candidates$detail) else "(none yet)"
  list(
    task = task, prompt_builder = "gap_prompt", prompt_version = if (sketching) "gaps v1" else "scope-gaps v1",
    system = paste(c(
      "You look for what a system model is missing.", COACH_POLICY,
      "Every title must be a question about the system and end with a question mark.",
      "Refer to model content only by its handle, such as C2. Put a handle in target, or leave it empty.",
      "Give at most five items. Fill unused fields with an empty string or an empty list.",
      "kind question: a question, with context in body.",
      "kind sketch_requirement: a question plus a starting draft in draft, using [value] for every number.",
      authoring_rules_block(),
      if (sketching) paste("kind sketch_element or sketch_function: a question plus a proposed name, stereotype and ports.",
                           "A sketch is structure only: no values. Each port's interface must be a handle.")
    ), collapse = "\n"),
    message = paste(c(
      paste("System scope:", if (nzchar(scope)) scope else "(not written yet)"),
      "Model content:", lines,
      "Committed requirements:", if (length(committed) > 0L) paste0("- ", committed) else "(none yet)",
      if (!is.null(focus)) paste("Look especially at:", focus)
    ), collapse = "\n"),
    schema = GAP_SCHEMA, cache_text = NULL,
    candidates_json = canonical_json(lapply(seq_len(nrow(candidates)), function(i) as.list(candidates[i, c("n", "kind", "id", "name")]))),
    candidates = candidates
  )
}

# Up to `limit` committed requirements most related to a draft: rare shared words count most.
related_requirements <- function(draft, committed, limit = 40L) {
  if (length(committed) == 0L) return(character(0))
  words_of <- function(text) unique(singular_word(setdiff(term_tokens(text), c(TERM_DETERMINERS, TERM_STOP_WORDS, ",", ".", ";", ":", "(", ")"))))
  draft_words <- words_of(draft)
  documents <- lapply(committed, words_of)
  frequency <- table(unlist(documents))
  score <- vapply(documents, function(words) {
    shared <- intersect(words, draft_words)
    if (length(shared) == 0L) 0 else sum(1 / as.numeric(frequency[shared]))
  }, 0)
  committed[utils::head(order(-score), limit)]
}

LINK_SCHEMA <- s_object(items = s_array(s_object(
  question = s_string(),
  candidates = s_array(s_object(handle = s_string(), why = s_string()))
)))

# Scores components against a text: rare shared words count most, then shared interfaces,
# the same parent, and components already holding related functions (handoff §5.3).
rank_candidates <- function(con, target_text, target_kind, target_id = NULL, k = 10L) {
  root <- default_structure_root(con)
  elements <- DBI::dbGetQuery(con, "SELECT id, name, description, parent_id FROM element WHERE kind = 'component' ORDER BY sort_order, name")
  elements <- elements[!elements$id %in% root, , drop = FALSE]
  if (nrow(elements) == 0L) return(data.frame(n = character(0), kind = character(0), id = character(0), name = character(0), summary = character(0), score = numeric(0)))
  texts <- DBI::dbGetQuery(con, "SELECT l.target_id, r.text FROM link l JOIN requirement r ON r.id = l.source_id WHERE l.link_type = 'satisfied_by'")
  ports <- DBI::dbGetQuery(con, "SELECT p.element_id, p.direction, i.name, i.id AS interface_id FROM port p JOIN interface i ON i.id = p.interface_id")
  words_of <- function(text) unique(singular_word(setdiff(term_tokens(text), c(TERM_DETERMINERS, TERM_STOP_WORDS, "shall", ",", ".", ";", ":", "(", ")"))))
  documents <- lapply(seq_len(nrow(elements)), function(i) {
    row <- elements[i, ]
    words_of(paste(c(row$name, row$description[!is.na(row$description)], texts$text[texts$target_id == row$id], ports$name[ports$element_id == row$id]), collapse = " "))
  })
  frequency <- table(unlist(documents))
  target_words <- words_of(target_text)
  score <- vapply(documents, function(words) {
    shared <- intersect(words, target_words)
    if (length(shared) == 0L) 0 else sum(log(1 + nrow(elements) / as.numeric(frequency[shared])))
  }, 0)
  linked <- character(0)
  if (!is.null(target_id)) {
    linked <- DBI::dbGetQuery(con, "SELECT target_id FROM link WHERE source_id = ? AND link_type IN ('satisfied_by', 'allocated_to')", params = list(target_id))$target_id
    if (target_kind == "element") {
      neighbours <- DBI::dbGetQuery(con, "
        SELECT DISTINCT l.target_id FROM connector c JOIN port s ON s.id = c.source_port_id JOIN port t ON t.id = c.target_port_id
          JOIN link l ON l.link_type = 'allocated_to' AND l.source_id = CASE WHEN s.element_id = ? THEN t.element_id ELSE s.element_id END
         WHERE s.element_id = ? OR t.element_id = ?", params = list(target_id, target_id, target_id))$target_id
      score <- score + 1 * (elements$id %in% neighbours)
    } else {
      siblings <- DBI::dbGetQuery(con, "SELECT l.target_id, e.parent_id FROM link l JOIN requirement r ON r.id = l.source_id
                                         JOIN element e ON e.id = l.target_id
                                         WHERE l.link_type = 'satisfied_by' AND r.parent_id = (SELECT parent_id FROM requirement WHERE id = ?)", params = list(target_id))
      score <- score + 0.5 * (elements$id %in% siblings$target_id) + 0.25 * (elements$parent_id %in% siblings$parent_id)
    }
  }
  keep <- !elements$id %in% linked
  elements <- elements[keep, , drop = FALSE]
  score <- score[keep]
  chosen <- utils::head(order(-score, elements$name), k)
  elements <- elements[chosen, , drop = FALSE]
  summaries <- vapply(elements$id, function(id) {
    mine <- ports[ports$element_id == id, , drop = FALSE]
    holds <- unique(mine$name[mine$direction != "in"])
    takes <- unique(mine$name[mine$direction == "in"])
    paste(c(if (length(holds) > 0L) paste("provides", paste(holds, collapse = ", ")), if (length(takes) > 0L) paste("takes", paste(takes, collapse = ", "))), collapse = "; ")
  }, "", USE.NAMES = FALSE)
  data.frame(n = if (nrow(elements) > 0L) sprintf("C%d", seq_len(nrow(elements))) else character(0), kind = rep("element", nrow(elements)), id = elements$id,
             name = elements$name, summary = summaries, score = score[chosen], stringsAsFactors = FALSE)
}

link_coach_prompt <- function(con, target_kind, target_id, k = 10L) {
  allocating <- target_kind == "element"
  row <- read_entity(con, target_kind, target_id)
  text <- if (allocating) row$name else row$text
  candidates <- rank_candidates(con, text, target_kind, target_id, k)
  scope <- get_setting(con, "scope_text", "")
  lines <- if (nrow(candidates) > 0L) sprintf("%s %s%s", candidates$n, candidates$name, ifelse(nzchar(candidates$summary), paste(" —", candidates$summary), "")) else "(none)"
  list(
    task = if (allocating) "allocation_coach" else "trace_coach",
    prompt_builder = "link_coach_prompt", prompt_version = if (allocating) "allocation-coach v1" else "trace-coach v1",
    system = paste(c(
      sprintf("You help an engineer decide which component should own a %s. The engineer chooses.", if (allocating) "function" else "requirement"),
      COACH_POLICY,
      "Ask up to two questions. Each must end with a question mark, and argue the candidates worth considering.",
      "Refer to components only by their handle, such as C2. Don't make up handles.",
      "If no candidate fits, ask what the model is missing and list no candidates."
    ), collapse = "\n"),
    message = paste(c(
      if (nzchar(scope)) paste("System scope:", scope),
      paste(if (allocating) "Function:" else "Requirement:", text),
      "Candidates:", lines
    ), collapse = "\n"),
    schema = LINK_SCHEMA, cache_text = NULL,
    candidates_json = canonical_json(lapply(seq_len(nrow(candidates)), function(i) as.list(candidates[i, c("n", "kind", "id", "name")]))),
    candidates = candidates, target = list(kind = target_kind, id = target_id)
  )
}
