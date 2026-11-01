# Validators: what comes back from a model is checked before anyone sees it. Numbers
# stay the engineer's, and handles must resolve.

# Replaces every token holding a digit with [value], leaving listed names alone.
# Returns the text and how many it replaced.
scrub_numbers <- function(text, allowed = character(0)) {
  if (length(text) != 1L || is.na(text) || !nzchar(text)) return(list(text = text, count = 0L))
  tokens <- strsplit(text, "(?<=\\s)|(?=\\s)", perl = TRUE)[[1]]
  allowed_tokens <- unique(unlist(strsplit(allowed, "\\s+")))
  bare <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", tokens)
  numeric_token <- grepl("[0-9]", tokens) & !bare %in% allowed_tokens & !grepl("^\\[[^]]*\\]$", bare)
  tokens[numeric_token] <- "[value]"
  list(text = paste(tokens, collapse = ""), count = sum(numeric_token))
}

scrub_tree <- function(value, allowed) {
  state <- new.env()
  state$count <- 0L
  walk <- function(x) {
    if (is.character(x) && length(x) == 1L) {
      out <- scrub_numbers(x, allowed)
      state$count <- state$count + out$count
      return(out$text)
    }
    if (is.list(x)) return(lapply(x, walk))
    x
  }
  list(value = walk(value), count = state$count)
}

# The word diff between a draft and an example: rows of (word, op) with op keep/del/ins.
word_diff <- function(old, new) {
  a <- strsplit(trimws(old), "\\s+")[[1]]
  b <- strsplit(trimws(new), "\\s+")[[1]]
  lcs <- matrix(0L, length(a) + 1L, length(b) + 1L)
  for (i in rev(seq_along(a))) {
    for (j in rev(seq_along(b))) {
      lcs[i, j] <- if (a[i] == b[j]) lcs[i + 1L, j + 1L] + 1L else max(lcs[i + 1L, j], lcs[i, j + 1L])
    }
  }
  words <- character(0)
  ops <- character(0)
  i <- 1L
  j <- 1L
  while (i <= length(a) || j <= length(b)) {
    if (i <= length(a) && j <= length(b) && a[i] == b[j]) {
      words <- c(words, a[i])
      ops <- c(ops, "keep")
      i <- i + 1L
      j <- j + 1L
    } else if (j <= length(b) && (i > length(a) || lcs[i, j + 1L] >= lcs[i + 1L, j])) {
      words <- c(words, b[j])
      ops <- c(ops, "ins")
      j <- j + 1L
    } else {
      words <- c(words, a[i])
      ops <- c(ops, "del")
      i <- i + 1L
    }
  }
  data.frame(word = words, op = ops, stringsAsFactors = FALSE)
}

# An example rewrite: numbers not in the draft become [value], then it's re-gated. A
# placeholder is expected in an example, so only other failures hide it. It's never
# repaired automatically (handoff §5.2).
check_example_rewrite <- function(draft, example, dictionary = EMPTY_TERMS) {
  if (!nzchar(trimws(example))) return(list(status = "none", invented = 0L))
  draft_numbers <- regmatches(draft, gregexpr("[0-9][0-9.]*", draft))[[1]]
  tokens <- strsplit(example, "(?<=\\s)|(?=\\s)", perl = TRUE)[[1]]
  invented <- 0L
  for (i in seq_along(tokens)) {
    found <- regmatches(tokens[i], gregexpr("[0-9][0-9.]*", tokens[i]))[[1]]
    if (length(found) > 0L && !all(sub("\\.$", "", found) %in% sub("\\.$", "", draft_numbers))) {
      ending <- regmatches(tokens[i], regexpr("[[:punct:]]+$", tokens[i]))
      tokens[i] <- paste0("[value]", if (length(ending) == 1L) ending else "")
      invented <- invented + 1L
    }
  }
  text <- paste(tokens, collapse = "")
  gate <- check_requirement(text, dictionary)
  reasons <- gate_blockers(gate)
  reasons <- reasons[!grepl("Contains a placeholder", reasons, fixed = TRUE)]
  if (length(reasons) > 0L) return(list(status = "failed", reasons = reasons, invented = invented))
  list(status = "ok", text = text, diff = word_diff(draft, text), invented = invented)
}

# What the Write panel shows for a coach answer.
process_coach_answer <- function(con, draft, value) {
  committed <- DBI::dbGetQuery(con, "SELECT id, text FROM requirement WHERE status = 'committed'")
  conflicts <- lapply(value$conflicts, function(conflict) {
    match_at <- match(trimws(conflict$quoted_text), trimws(committed$text))
    list(quoted_text = conflict$quoted_text, explanation = conflict$explanation,
         requirement_id = if (is.na(match_at)) NA_character_ else committed$id[match_at])
  })
  list(criteria = value$criteria, questions = value$questions, conflicts = conflicts,
       example = check_example_rewrite(draft, value$example_rewrite, model_terms(con)))
}

# Only a verbatim conflict is recorded; a paraphrase is shown and left at that.
record_conflicts <- function(con, requirement_id, answer, call_id) {
  for (conflict in answer$conflicts) {
    if (!is.na(conflict$requirement_id) && conflict$requirement_id != requirement_id) {
      insert_conflict(con, requirement_id, conflict$requirement_id, conflict$explanation, call_id)
    }
  }
  invisible(NULL)
}

needs_question_mark <- function(item) {
  item$kind %in% c("question", "candidate_link") && !grepl("\\?\\s*$", item$title)
}

gap_answer_needs_retry <- function(value) any(vapply(value$items, needs_question_mark, TRUE))

# Maps handles back, scrubs numbers, and stores each item as a proposal. Handles that
# don't resolve are dropped and logged. Returns the new proposal ids.
store_gap_answer <- function(con, prompt, result) {
  candidates <- prompt$candidates
  names_allowed <- candidates$name
  resolve <- function(handle) {
    at <- match(trimws(handle), candidates$n)
    if (is.na(at)) NULL else candidates[at, ]
  }
  dictionary <- model_terms(con)
  ids <- character(0)
  for (item in result$value$items) {
    target <- NULL
    if (nzchar(trimws(item$target))) {
      target <- resolve(item$target)
      if (is.null(target)) log_ai_drop(con, list(call = result$call_id, dropped = "target", handle = item$target))
    }
    invented <- 0L
    payload <- list()
    if (item$kind %in% c("sketch_element", "sketch_function")) {
      ports <- list()
      for (port in item$ports) {
        interface <- resolve(port$interface)
        if (is.null(interface) || interface$kind != "interface") {
          log_ai_drop(con, list(call = result$call_id, dropped = "port", handle = port$interface))
          next
        }
        ports[[length(ports) + 1L]] <- list(name = port$name, direction = port$direction, interface = interface$name)
      }
      scrubbed <- scrub_tree(list(name = item$name, stereotype = item$stereotype, ports = ports), names_allowed)
      payload <- scrubbed$value
      invented <- invented + scrubbed$count
    }
    if (item$kind == "sketch_requirement") {
      # The starting draft is gate-checked when it's opened in Write, and holds no invented numbers.
      scrubbed <- scrub_numbers(item$draft, names_allowed)
      invented <- invented + scrubbed$count
      gate <- check_requirement(scrubbed$text, dictionary)
      payload <- list(draft = scrubbed$text, gate = as.list(gate_blockers(gate)))
    }
    if (invented > 0L) payload$`_invented` <- invented
    if (needs_question_mark(item)) payload$`_warning` <- "The title isn't phrased as a question."
    ids <- c(ids, insert_proposal(con, list(
      task = prompt$task, proposal_kind = item$kind, source = "ai", target_kind = if (!is.null(target)) target$kind,
      target_id = if (!is.null(target)) target$id, title = item$title, body = item$body,
      payload_json = if (length(payload) == 0L) "{}" else canonical_json(payload), confidence = item$confidence, llm_call_id = result$call_id
    )))
  }
  ids
}

# Numbered candidates chosen from: maps a handle back and creates the link. The
# rationale is the engineer's own, and the database refuses it blank.
link_from_candidate <- function(con, proposal_id, handle, link_type, source_kind, source_id, rationale, author = "engineer") {
  proposal <- DBI::dbGetQuery(con, "SELECT * FROM ai_proposal WHERE id = ?", params = list(proposal_id))
  if (nrow(proposal) == 0L) refuse("That proposal no longer exists.")
  if (!nzchar(trimws(rationale))) refuse("A link chosen from candidates needs a note in your own words.")
  candidates <- jsonlite::fromJSON(proposal$candidates_json)
  chosen <- candidates[candidates$n == handle, , drop = FALSE]
  if (nrow(chosen) != 1L) refuse("That candidate isn't on the list.")
  in_tx(con, {
    written <- write_link(con, fields = list(link_type = link_type, source_kind = source_kind, source_id = source_id,
                                             target_kind = chosen$kind, target_id = chosen$id, rationale = trimws(rationale),
                                             origin = "ai_candidate", ai_proposal_id = proposal_id), author = author)
    update_proposal(con, proposal_id, list(status = "kept", engineer_note = trimws(rationale), created_entity_kind = "link", created_entity_id = written$id))
    written
  })
}
