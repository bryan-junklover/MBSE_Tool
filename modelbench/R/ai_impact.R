# AI over the impact set: significance over I1…In, and a narrative. Every number comes
# from code and reaches the model as a quoted fact (BUILD_PLAN §2).

IMPACT_SCHEMA <- s_object(ratings = s_array(s_object(n = s_string(), significance = s_enum(c("High", "Medium", "Low")), reason = s_string())))
NARRATIVE_SCHEMA <- s_object(technical = s_string(), stakeholder = s_string(), decision = s_string())

# The numbers a narrative may use, from roll-ups and analyses.
impact_facts <- function(con, baseline_id = NULL) {
  budgets <- budget_status(con)
  analyses <- analysis_status(con)
  lines <- c(
    if (nrow(budgets) > 0L) sprintf("%s: %s (%s)", budgets$label, budgets$text, ifelse(budgets$pass, "within limit", "over limit")),
    if (nrow(analyses) > 0L) sprintf("%s: %s (%s)", term_to_title(analyses$analysis), analyses$text, ifelse(analyses$pass, "meets limit", "misses limit"))
  )
  list(lines = lines, failing = any(!budgets$pass) || any(!analyses$pass))
}

card_summary <- function(card) {
  paste(vapply(card$entities, function(entity) sprintf("%s: %s", entity$title, entity$change_text), ""), collapse = "; ")
}

impact_rating_prompt <- function(con, card, items) {
  list(
    task = "impact_narrative", prompt_builder = "impact_rating_prompt", prompt_version = "impact-rating v1",
    system = paste("You rate how much a change matters to each item a code traversal found. You don't add or remove items.", PLAIN_TEXT_RULE,
                   "Rate every item High, Medium or Low, by its handle, with a short reason. Don't state numbers in reasons.", sep = "\n"),
    message = paste(c(
      paste("Change:", card_summary(card)),
      "Facts from code:", impact_facts(con)$lines,
      "Items:", if (nrow(items) > 0L) sprintf("I%d %s (%d hops; %s)", seq_len(nrow(items)), items$label, items$hops, items$path) else "(none)"
    ), collapse = "\n"),
    schema = IMPACT_SCHEMA, cache_text = paste(card$change_set, model_hash(con), sep = "\n"), candidates_json = NULL
  )
}

# Ratings by row of the impact table. A handle that doesn't resolve is dropped and logged.
apply_ratings <- function(con, items, result) {
  items$significance <- rep(NA_character_, nrow(items))
  items$reason <- rep(NA_character_, nrow(items))
  if (!isTRUE(result$ok)) return(items)
  allowed <- items$label
  for (rating in result$value$ratings) {
    at <- suppressWarnings(as.integer(sub("^I", "", trimws(rating$n))))
    if (is.na(at) || at < 1L || at > nrow(items)) {
      log_ai_drop(con, list(call = result$call_id, dropped = "impact item", handle = rating$n))
      next
    }
    items$significance[at] <- rating$significance
    items$reason[at] <- scrub_numbers(rating$reason, allowed)$text
  }
  items
}

impact_narrative_prompt <- function(con, card, items) {
  facts <- impact_facts(con)
  list(
    task = "impact_narrative", prompt_builder = "impact_narrative_prompt", prompt_version = "impact-narrative v1",
    system = paste(
      "You explain a change to a system model, twice: once for engineers, once for stakeholders.", PLAIN_TEXT_RULE,
      "Use only the numbers given as facts. Never work out a new one.",
      "technical: name the requirements and tests affected, and give the numbers.",
      "stakeholder: plain words only, with no ids, interface names or stereotypes.",
      "decision: if a budget or requirement fails, the choice a stakeholder faces, in plain words. Otherwise an empty string.", sep = "\n"),
    message = paste(c(
      paste("Change:", card_summary(card)),
      "Facts from code:", facts$lines,
      "What it reaches:", if (nrow(items) > 0L) sprintf("- %s (%s)", items$label, items$path) else "(nothing)"
    ), collapse = "\n"),
    schema = NARRATIVE_SCHEMA, cache_text = paste("narrative", card$change_set, model_hash(con), sep = "\n"), candidates_json = NULL
  )
}

# Ids and interface names in plain-word text become plain names, or go.
plain_words <- function(con, text) {
  requirements <- DBI::dbGetQuery(con, "SELECT id, coalesce(short_title, 'a requirement') AS name FROM requirement")
  elements <- DBI::dbGetQuery(con, "SELECT id, name FROM element")
  cases <- DBI::dbGetQuery(con, "SELECT id, name FROM test_case")
  interfaces <- DBI::dbGetQuery(con, "SELECT id, name FROM interface")
  map <- rbind(requirements, elements, cases, interfaces)
  map <- map[order(-nchar(map$id)), , drop = FALSE]
  for (i in seq_len(nrow(map))) {
    text <- gsub(paste0("(?<![A-Za-z0-9-])", gsub("([.+*?^$()\\[\\]{}|\\\\-])", "\\\\\\1", map$id[i], perl = TRUE), "(?![A-Za-z0-9-])"), map$name[i], text, perl = TRUE)
  }
  for (name in interfaces$name) text <- gsub(paste0("\\b", name, "\\b"), "a connection", text)
  gsub("«[^»]*»\\s*", "", text)
}

allowed_fact_numbers <- function(con) {
  lines <- impact_facts(con)$lines
  found <- unlist(regmatches(lines, gregexpr("[0-9][0-9.]*", lines)))
  rounded <- suppressWarnings(as.numeric(found))
  unique(c(found, as.character(round(rounded[!is.na(rounded)]))))
}

# Numbers not given as facts go, and the stakeholder text ends on the decision when
# something fails (handoff §5.9).
process_narrative <- function(con, card, value) {
  allowed <- c(allowed_fact_numbers(con), unlist(regmatches(card_summary(card), gregexpr("[0-9][0-9.]*", card_summary(card)))))
  ids <- unlist(lapply(c("requirement", "element", "test_case", "interface"), function(kind) DBI::dbGetQuery(con, sprintf("SELECT id FROM %s", kind))$id))
  technical <- replace_unknown_numbers(value$technical, allowed, ids)
  stakeholder <- replace_unknown_numbers(plain_words(con, value$stakeholder), allowed)
  decision <- replace_unknown_numbers(plain_words(con, value$decision), allowed)
  failing <- impact_facts(con)$failing
  list(technical = technical$text, stakeholder = stakeholder$text,
       decision = if (failing && nzchar(trimws(decision$text))) decision$text else NA_character_,
       invented = technical$count + stakeholder$count + decision$count)
}

run_impact_ai_async <- function(con, card, items) {
  safely_async(function() {
    rating <- impact_rating_prompt(con, card, items)
    narrative <- impact_narrative_prompt(con, card, items)
    cached_rating <- ai_cache_lookup(con, rating)
    cached_narrative <- ai_cache_lookup(con, narrative)
    first <- if (!is.null(cached_rating)) promises::promise_resolve(cached_rating) else run_ai_async(con, rating)
    promises::then(first, function(rated) {
      second <- if (!is.null(cached_narrative)) promises::promise_resolve(cached_narrative) else run_ai_async(con, narrative)
      promises::then(second, function(told) {
        if (!DBI::dbIsValid(con)) return(rated)
        list(ok = isTRUE(rated$ok) || isTRUE(told$ok), error = if (!isTRUE(rated$ok)) rated$error else told$error, change_set = card$change_set,
             items = apply_ratings(con, items, rated),
             narrative = if (isTRUE(told$ok)) process_narrative(con, card, told$value),
             provider_id = told$provider_id, model = told$model, prompt_version = told$prompt_version)
      })
    })
  })
}

# What's already known for this card and this model, without any call.
impact_ai_from_cache <- function(con, card, items) {
  rated <- ai_cache_lookup(con, impact_rating_prompt(con, card, items))
  told <- ai_cache_lookup(con, impact_narrative_prompt(con, card, items))
  if (is.null(rated) && is.null(told)) return(NULL)
  list(ok = TRUE, change_set = card$change_set,
       items = if (is.null(rated)) NULL else apply_ratings(con, items, rated),
       narrative = if (is.null(told)) NULL else process_narrative(con, card, told$value),
       provider_id = if (is.null(told)) NA_character_ else told$provider_id, model = if (is.null(told)) NA_character_ else told$model)
}

# --- Stakeholder view ---

PLAIN_MEASURES <- c(mass = "Weight", power = "Power use in flight", endurance = "Flight time")

stakeholder_meters <- function(con) {
  budgets <- DBI::dbGetQuery(con, "SELECT b.id, d.name, b.comparison FROM budget b JOIN property_def d ON d.id = b.property_def_id ORDER BY b.id")
  status <- budget_status(con)
  rows <- lapply(seq_len(nrow(status)), function(i) {
    name <- budgets$name[match(status$id[i], budgets$id)]
    list(label = if (name %in% names(PLAIN_MEASURES)) unname(PLAIN_MEASURES[name]) else status$label[i],
         value_text = sprintf("%s of %s %s", format_amount(status$value[i], status$limit[i]), format_amount(status$limit[i]), status$unit[i]),
         fraction = min(1, status$value[i] / status$limit[i]), pass = status$pass[i], line = if (status$pass[i]) "Within limit" else "Over the limit. A decision is needed.")
  })
  analyses <- analysis_status(con)
  c(rows, lapply(seq_len(nrow(analyses)), function(i) {
    row <- analyses[i, ]
    known <- row$status == "ok"
    list(label = if (row$analysis %in% names(PLAIN_MEASURES)) unname(PLAIN_MEASURES[row$analysis]) else term_to_title(row$analysis),
         value_text = if (known) sprintf("%.0f %s (need %.0f)", row$value, row$unit, row$limit) else "Not known yet",
         fraction = if (known) min(1, row$value / row$limit) else 0, pass = row$pass,
         line = if (!known) "Waiting on a value." else if (row$pass) "On target" else "Below the target. A decision is needed.")
  }))
}

# Open items in plain words, from code. Nothing here needs a model.
stakeholder_open_questions <- function(con) {
  ports <- DBI::dbGetQuery(con, "SELECT DISTINCT e.name FROM v_unconnected_ports p JOIN element e ON e.id = p.element_id")$name
  unassigned <- count_of(con, "SELECT count(*) AS n FROM v_unallocated_requirements")
  drafts <- count_of(con, "SELECT count(*) AS n FROM v_unreviewed")
  missing <- count_of(con, "SELECT count(*) AS n FROM v_placeholders")
  stale <- nrow(stale_evidence(con))
  plural <- function(n, one, many) if (n == 1L) one else many
  c(sprintf("%s isn't fully connected to the rest of the system yet.", ports),
    if (unassigned > 0L) sprintf("%d %s assigned to any part of the design.", unassigned, plural(unassigned, "requirement isn't", "requirements aren't")),
    if (missing > 0L) sprintf("%d %s still to be decided.", missing, plural(missing, "value is", "values are")),
    if (drafts > 0L) sprintf("%d suggested %s still waiting for an engineer's review.", drafts, plural(drafts, "item is", "items are")),
    if (stale > 0L) sprintf("%d %s to be repeated because the design has changed.", stale, plural(stale, "test result needs", "test results need")))
}

stakeholder_overview <- function(con) {
  root <- default_structure_root(con)
  parts <- if (is.na(root)) {
    DBI::dbGetQuery(con, "SELECT id, name, plain_summary, plain_summary_origin, status FROM element WHERE kind = 'component' AND parent_id IS NULL AND review_state = 'reviewed' ORDER BY sort_order, name")
  } else {
    DBI::dbGetQuery(con, "SELECT id, name, plain_summary, plain_summary_origin, status FROM element WHERE kind = 'component' AND parent_id = ? AND review_state = 'reviewed' ORDER BY sort_order, name", params = list(root))
  }
  summary <- get_setting(con, "stakeholder_summary", "")
  list(project = get_setting(con, "project_name", "This system"),
       paragraph = if (nzchar(summary)) summary else get_setting(con, "scope_text", ""),
       paragraph_is_ai = nzchar(summary), parts = parts, meters = stakeholder_meters(con), questions = stakeholder_open_questions(con))
}

STAKEHOLDER_SCHEMA <- s_object(paragraph = s_string(), summaries = s_array(s_object(handle = s_string(), summary = s_string())))

stakeholder_prompt <- function(con) {
  overview <- stakeholder_overview(con)
  parts <- overview$parts
  parts$n <- if (nrow(parts) > 0L) sprintf("C%d", seq_len(nrow(parts))) else character(0)
  list(
    task = "stakeholder_summary", prompt_builder = "stakeholder_prompt", prompt_version = "stakeholder v1",
    system = paste("You write for people who aren't engineers.", PLAIN_TEXT_RULE,
                   "Use plain words, with no ids, interface names or stereotypes, and no numbers.",
                   "paragraph: the system in one short paragraph, ending on what's still open.",
                   "summaries: one plain sentence per part, by handle, only for parts with no summary yet.", sep = "\n"),
    message = paste(c(
      paste("System:", overview$project), paste("Scope:", get_setting(con, "scope_text", "")),
      "Parts:", if (nrow(parts) > 0L) sprintf("%s %s: %s", parts$n, parts$name, ifelse(is.na(parts$plain_summary), "(no summary yet)", parts$plain_summary)) else "(none)",
      "Open items:", overview$questions
    ), collapse = "\n"),
    schema = STAKEHOLDER_SCHEMA, cache_text = NULL, candidates_json = canonical_json(lapply(seq_len(nrow(parts)), function(i) list(n = parts$n[i], kind = "element", id = parts$id[i], name = parts$name[i]))),
    candidates = parts
  )
}

store_stakeholder_answer <- function(con, prompt, result, author = "engineer") {
  allowed <- prompt$candidates$name
  set_setting(con, "stakeholder_summary", scrub_numbers(plain_words(con, result$value$paragraph), allowed)$text)
  with_change_set(con, function(change_set) {
    for (item in result$value$summaries) {
      at <- match(trimws(item$handle), prompt$candidates$n)
      if (is.na(at)) {
        log_ai_drop(con, list(call = result$call_id, dropped = "summary", handle = item$handle))
        next
      }
      if (!is.na(prompt$candidates$plain_summary[at])) next
      write_element(con, prompt$candidates$id[at], list(plain_summary = scrub_numbers(plain_words(con, item$summary), allowed)$text, plain_summary_origin = "ai"),
                    author, "Plain summary from AI", change_set)
    }
    change_set
  })
}

run_stakeholder_async <- function(con) {
  safely_async(function() {
    prompt <- stakeholder_prompt(con)
    promises::then(run_ai_async(con, prompt), function(result) {
      if (DBI::dbIsValid(con) && isTRUE(result$ok)) result$change_set <- store_stakeholder_answer(con, prompt, result)
      result
    })
  })
}
