# Term extraction and resolution: which words in a requirement name something in
# the model. Matching ignores case, articles and plurals.

TERM_DETERMINERS <- c("the", "a", "an", "its", "their", "each", "every", "any", "all", "both", "this", "that")

# Words that end a noun phrase: prepositions, conjunctions, and the verbs that
# commonly follow a subject in a condition clause.
TERM_STOP_WORDS <- c(
  "of", "to", "in", "on", "at", "by", "for", "with", "within", "from", "into", "onto", "over", "under",
  "below", "above", "after", "before", "between", "through", "during", "using", "via", "per", "against",
  "and", "or", "but", "not", "no", "than", "that", "which", "who", "when", "while", "if", "where", "upon",
  "then", "unless", "except", "shall", "should", "will", "must", "may", "can", "is", "are", "be", "was",
  "were", "been", "has", "have", "had", "falls", "rises", "drops", "exceeds", "reaches", "enters", "leaves",
  "opens", "closes", "fails", "stops", "starts", "lost", "detected", "defined", "at", "least", "most"
)

# "Sensors" and "sensor" compare equal; "status", "gnss" and "analysis" keep their s.
singular_word <- function(word) {
  ifelse(grepl("ies$", word) & nchar(word) > 4L, sub("ies$", "y", word),
         ifelse(grepl("[^sui]s$", word) & nchar(word) > 3L, sub("s$", "", word), word))
}

term_tokens <- function(text) {
  tokens <- regmatches(tolower(text), gregexpr("[a-z0-9]+|[,.;:()]", tolower(text)))[[1]]
  tokens
}

normalize_term <- function(text) {
  vapply(text, function(one) {
    tokens <- term_tokens(one)
    tokens <- tokens[grepl("^[a-z0-9]", tokens)]
    while (length(tokens) > 1L && tokens[1] %in% TERM_DETERMINERS) tokens <- tokens[-1]
    paste(singular_word(tokens), collapse = " ")
  }, "", USE.NAMES = FALSE)
}

# What a draft's words can resolve to: element names, glossary aliases, property
# names (resolved to the element holding them) and terms marked "not a model term".
model_terms <- function(con) {
  elements <- DBI::dbGetQuery(con, "
    SELECT e.name AS term, e.id AS element_id, e.name AS element_name, s.name AS stereotype, 'element' AS source
      FROM element e LEFT JOIN stereotype s ON s.id = e.stereotype_id")
  aliases <- DBI::dbGetQuery(con, "
    SELECT g.term AS term, e.id AS element_id, e.name AS element_name, s.name AS stereotype, 'alias' AS source
      FROM glossary g JOIN element e ON e.id = g.element_id LEFT JOIN stereotype s ON s.id = e.stereotype_id")
  properties <- DBI::dbGetQuery(con, "
    SELECT d.label AS term, e.id AS element_id, e.name AS element_name, s.name AS stereotype, 'property' AS source
      FROM property_value v JOIN property_def d ON d.id = v.property_def_id
      JOIN element e ON v.owner_kind = 'element' AND e.id = v.owner_id
      LEFT JOIN stereotype s ON s.id = e.stereotype_id")
  # An interface field ("heading accuracy") resolves to the element that sends it.
  fields <- DBI::dbGetQuery(con, "
    SELECT replace(f.name, '_', ' ') AS term, e.id AS element_id, e.name AS element_name, s.name AS stereotype, 'field' AS source
      FROM interface_field f JOIN port p ON p.interface_id = f.interface_id AND p.direction IN ('out', 'inout')
      JOIN element e ON e.id = p.element_id LEFT JOIN stereotype s ON s.id = e.stereotype_id
     ORDER BY f.name, e.sort_order")
  fields <- fields[!duplicated(fields$term), , drop = FALSE]
  not_model <- DBI::dbGetQuery(con, "
    SELECT term, NULL AS element_id, NULL AS element_name, NULL AS stereotype, 'not_model' AS source
      FROM glossary WHERE element_id IS NULL")
  # A property several elements hold (mass, power) doesn't say which element is meant.
  properties <- properties[!properties$term %in% properties$term[duplicated(properties$term)], , drop = FALSE]
  terms <- rbind(elements, aliases, properties, fields, not_model)
  terms$norm <- normalize_term(terms$term)
  # An element name beats an alias, which beats a property.
  terms[!duplicated(terms$norm), , drop = FALSE]
}

EMPTY_TERMS <- data.frame(term = character(0), element_id = character(0), element_name = character(0),
                          stereotype = character(0), source = character(0), norm = character(0),
                          stringsAsFactors = FALSE)

# Marks the longest dictionary matches. Returns one row per mention, by token position.
find_mentions <- function(tokens, dictionary) {
  words <- ifelse(grepl("^[a-z0-9]", tokens), singular_word(tokens), "")
  used <- rep(FALSE, length(tokens))
  longest <- if (nrow(dictionary) == 0L) 0L else max(lengths(strsplit(dictionary$norm, " ", fixed = TRUE)))
  mentions <- list()
  for (size in rev(seq_len(longest))) {
    for (start in seq_len(max(0L, length(tokens) - size + 1L))) {
      span <- start:(start + size - 1L)
      if (any(used[span]) || any(words[span] == "")) next
      hit <- match(paste(words[span], collapse = " "), dictionary$norm)
      if (is.na(hit)) next
      used[span] <- TRUE
      mentions[[length(mentions) + 1L]] <- list(start = start, end = start + size - 1L, row = hit)
    }
  }
  list(mentions = mentions, used = used)
}

# Noun phrases the model doesn't know: words after a determiner, up to a stop word.
# A phrase that runs into a known term only modifies it, so it isn't a term of its own.
find_unknown_phrases <- function(tokens, used) {
  phrases <- list()
  i <- 1L
  while (i <= length(tokens)) {
    if (!tokens[i] %in% TERM_DETERMINERS || used[i]) {
      i <- i + 1L
      next
    }
    j <- i + 1L
    while (j <= length(tokens) && j - i <= 4L && grepl("^[a-z]", tokens[j]) &&
           !tokens[j] %in% TERM_STOP_WORDS && !used[j]) {
      j <- j + 1L
    }
    runs_into_known <- j <= length(tokens) && used[j]
    if (j > i + 1L && !runs_into_known) {
      phrases[[length(phrases) + 1L]] <- list(start = i + 1L, end = j - 1L)
    }
    i <- max(j, i + 1L)
  }
  phrases
}

QUANTITY_LOOKAHEAD <- "^(of (at least |at most |no more than |no less than |not less than |up to )?[0-9]|at least [0-9]|no more than [0-9]|no less than [0-9])"

term_role <- function(tokens, start, end, actor_norm) {
  phrase <- paste(singular_word(tokens[start:end]), collapse = " ")
  if (nzchar(actor_norm) && grepl(paste0("(^| )", phrase, "( |$)"), actor_norm)) return("actor")
  after <- paste(utils::tail(tokens, length(tokens) - end), collapse = " ")
  if (grepl(QUANTITY_LOOKAHEAD, after)) "quantity" else "object"
}

# One row per term: term, role, resolution (model / not_model / unresolved) and the
# element it resolved to. The actor comes first, then the order of appearance.
extract_terms <- function(text, actor, dictionary = EMPTY_TERMS) {
  empty <- data.frame(term = character(0), role = character(0), resolution = character(0),
                      element_id = character(0), element_name = character(0), stereotype = character(0),
                      source = character(0), position = integer(0), stringsAsFactors = FALSE)
  tokens <- term_tokens(text)
  if (length(tokens) == 0L) return(empty)
  actor_norm <- if (is.na(actor)) "" else normalize_term(actor)
  found <- find_mentions(tokens, dictionary)

  rows <- lapply(found$mentions, function(m) {
    entry <- dictionary[m$row, ]
    data.frame(term = paste(tokens[m$start:m$end], collapse = " "),
               role = term_role(tokens, m$start, m$end, actor_norm),
               resolution = if (identical(entry$source, "not_model")) "not_model" else "model",
               element_id = entry$element_id, element_name = entry$element_name, stereotype = entry$stereotype,
               source = entry$source, position = m$start, stringsAsFactors = FALSE)
  })
  unknown <- lapply(find_unknown_phrases(tokens, found$used), function(p) {
    data.frame(term = paste(tokens[p$start:p$end], collapse = " "),
               role = term_role(tokens, p$start, p$end, actor_norm),
               resolution = "unresolved", element_id = NA_character_, element_name = NA_character_,
               stereotype = NA_character_, source = NA_character_, position = p$start, stringsAsFactors = FALSE)
  })
  terms <- do.call(rbind, c(list(empty), rows, unknown))

  # An actor with no determiner ("GNSS receiver shall") isn't caught above.
  if (nzchar(actor_norm) && !any(terms$role == "actor")) {
    bare <- normalize_term(actor)
    terms <- rbind(terms, data.frame(term = bare, role = "actor", resolution = "unresolved",
                                     element_id = NA_character_, element_name = NA_character_,
                                     stereotype = NA_character_, source = NA_character_, position = 0L,
                                     stringsAsFactors = FALSE))
  }
  terms <- terms[order(terms$role != "actor", terms$position), , drop = FALSE]
  terms <- terms[!duplicated(normalize_term(terms$term)), , drop = FALSE]
  rownames(terms) <- NULL
  terms
}

# The whole gate: the structural checks plus term resolution against the model.
check_requirement <- function(text, dictionary = EMPTY_TERMS) {
  structural <- check_requirement_logic(text)
  terms <- extract_terms(structural$text, structural$extracted_actor, dictionary)
  actor_row <- terms[terms$role == "actor", , drop = FALSE]
  has_named_actor <- nrow(actor_row) > 0L && !isTRUE(structural$warnings$pronoun_actor)
  actor_resolved <- if (nrow(dictionary) > 0L && has_named_actor) {
    actor_row$resolution[1] != "unresolved"
  } else {
    NA
  }
  result <- check_requirement_logic(text, actor_resolved)
  result$terms <- terms
  result$actor_element_id <- if (nrow(actor_row) > 0L) actor_row$element_id[1] else NA_character_
  result
}

# Which actions a term offers, by the role it plays (handoff §5.1).
term_actions <- function(role) {
  switch(role,
         actor = c("component", "not_model"),
         quantity = c("property", "not_model"),
         c("property", "field", "not_model"))
}
