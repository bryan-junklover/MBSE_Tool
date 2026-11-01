# The requirement gate, ported from Requirements Coach (R/logic_checks.R, 79a11d9).
# Deterministic, cheap sanity checks applied before any LLM call. Term resolution
# (R/terms.R) replaces the Coach's glossary check.

BANNED_WORDS <- c(
  "minimize", "maximize", "optimize", "rapid", "robust",
  "user-friendly", "easy", "fast", "slow", "flexible",
  "adequate", "appropriate", "seamless", "efficient",
  "should", "will", "quick", "quickly",
  # "Safely"/"safe" name an outcome without a testable definition of it -
  # same category as "robust"/"adequate" above. A genuine safety
  # requirement needs a concrete bound instead (e.g. "shall not exceed 40C
  # surface temperature") rather than asserting the word "safe" itself.
  "safe", "safely",
  # Escape clauses: grammatically complete, but each one gives the
  # requirement a discretionary out that guts its bindingness ("as
  # appropriate" leaves "appropriate" to nobody-knows-who's judgment) -
  # equally damaging to verifiability as the vague qualifiers above, so
  # they get the same hard-blocking treatment rather than a soft penalty.
  # Multi-word phrases work fine here: the existing banned_regex
  # alternation matches a literal space just like any other character.
  "as appropriate", "as applicable", "as required", "as needed",
  "as necessary", "if practical", "if practicable", "if possible",
  "where feasible", "when appropriate", "to the extent possible",
  "to the extent practicable", "insofar as possible"
)

# Established safety-engineering terms that contain a banned word but name a
# specific, verifiable condition rather than asserting a vague quality -
# "enter a safe state within 100 ms" is testable in a way "be safe" never
# is. These are masked out (same-length placeholder, so character positions
# stay valid for highlighting) before the banned-word scan, so a bare
# "safe"/"safely" elsewhere in the same sentence is still caught.
BANNED_WORD_EXCEPTIONS_REGEX <- "\\bfail-?safe\\b|\\bsafe (state|mode|position|condition|shutdown|stop)s?\\b"

mask_banned_word_exceptions <- function(req_lower) {
  stringr::str_replace_all(req_lower, BANNED_WORD_EXCEPTIONS_REGEX, function(m) strrep("_", nchar(m)))
}

ACTOR_WORDS <- c("user", "operator", "human", "person", "customer")

PRONOUN_ACTORS <- c("it", "they", "this", "that", "these", "those", "he", "she", "we", "you")

# A human-role word immediately followed by one of these names a system
# component ("the user interface", "the operator console", "the
# human-machine interface"), not a person - so it isn't penalized as a
# human actor.
ACTOR_COMPONENT_NOUNS <- c(
  "interface", "console", "database", "account", "portal", "app", "application",
  "display", "terminal", "panel", "station", "record", "profile", "screen",
  "device", "dashboard", "service", "machine", "controls", "input", "data"
)

HUMAN_ACTOR_REGEX <- stringr::str_c(
  "\\b(", stringr::str_c(ACTOR_WORDS, collapse = "|"), ")\\b",
  "(?![\\s-]+(", stringr::str_c(ACTOR_COMPONENT_NOUNS, collapse = "|"), ")\\b)"
)

# A constraint keyword alone (e.g. "within") isn't a real, verifiable
# constraint until it's attached to an actual number - and requiring the
# number is what stops the gate from passing mid-sentence, before the user
# has typed the value (e.g. "...within" before "500 ms" is added).
# Deliberately omits ambiguous short words that commonly follow a bare
# count in prose ("in", "a", "k") - "the first 10 in the list" must not
# read as a bounded requirement.
CONSTRAINT_UNIT_WORDS <- c(
  # time
  "ns", "us", "µs", "ms", "milliseconds", "s", "sec", "secs", "second", "seconds",
  "min", "mins", "minute", "minutes", "hr", "hrs", "hour", "hours",
  "day", "days", "week", "weeks", "month", "months", "year", "years",
  # length
  "nm", "um", "µm", "mm", "cm", "m", "km", "meter", "meters", "metre", "metres",
  "inch", "inches", "ft", "foot", "feet", "mi", "mile", "miles",
  # mass / volume / force / pressure
  "mg", "g", "kg", "lb", "lbs", "oz", "ml", "l", "liter", "liters", "litre", "litres",
  "n", "kn", "newton", "newtons", "pa", "kpa", "mpa", "bar", "psi",
  # electrical / power / frequency
  "v", "mv", "kv", "volt", "volts", "ma", "amp", "amps", "w", "mw", "kw", "watt", "watts",
  "hz", "khz", "mhz", "ghz", "rpm",
  # data
  "kb", "mb", "gb", "tb", "bps", "kbps", "mbps", "gbps",
  # other
  "db", "mph", "lux", "lm", "cycle", "cycles",
  "c", "f", "degree", "degrees", "kelvin"
)

# Units written with a symbol rather than a word: "45 °C", "6.5\"", "10%", "±2".
CONSTRAINT_SYMBOL_REGEX <- "\\d+(\\.\\d+)?\\s?°\\s?[cfk]?|\\d+(\\.\\d+)?\\s?\"|\\d+(\\.\\d+)?\\s?%|±\\s?\\d"

# Amps get their own rule: "2 a" is only read as a unit at the end of a
# clause ("...no more than 2 A."), since a bare "a" after a number is far
# more often an article in prose ("5 a day").
CONSTRAINT_UNIT_REGEX <- stringr::str_c(
  "\\d+(\\.\\d+)?\\s?(", stringr::str_c(CONSTRAINT_UNIT_WORDS, collapse = "|"), ")\\b",
  "|\\d+(\\.\\d+)?\\s?a(?=\\s*[.,;]|\\s*$)"
)

# Shorter, form-friendly unit list for the EARS builder's dropdown - every
# entry is recognized by CONSTRAINT_UNIT_REGEX/CONSTRAINT_SYMBOL_REGEX.
EARS_UNIT_CHOICES <- c("ms", "s", "min", "hours", "days", "mm", "cm", "m", "kg", "g",
                       "N", "kPa", "V", "mA", "W", "Hz", "rpm", "dB", "°C", "cycles")

# Locates the actor phrase: the 1-3 words immediately preceding the first
# "shall", without crossing a clause boundary. Two shapes are handled:
#   "When X happens, the trap shall ..."   -> actor after the last comma
#   "The trap, when armed, shall ..."      -> comma right before "shall"
#                                             means a parenthetical, so the
#                                             actor is before the first comma
# Returns list(start, end, text) against req_lower, or NULL when there's
# nothing before "shall" (or no "shall" at all). Shared by
# check_requirement_logic() and compute_highlight_spans(), so the scored,
# stored, and highlighted actor are always the same text.
locate_actor <- function(req_lower) {
  shall_loc <- stringr::str_locate(req_lower, "\\bshall\\b")
  if (is.na(shall_loc[1, 1]) || shall_loc[1, 1] <= 1) return(NULL)
  prefix <- substr(req_lower, 1, shall_loc[1, 1] - 1)

  seg_start <- 1L
  seg_end <- nchar(prefix)
  commas <- stringr::str_locate_all(prefix, ",")[[1]][, 1]
  if (length(commas) > 0) {
    last_comma <- max(commas)
    if (nzchar(stringr::str_trim(substr(prefix, last_comma + 1, nchar(prefix))))) {
      seg_start <- last_comma + 1L
    } else {
      seg_end <- commas[1] - 1L
    }
  }
  segment <- substr(prefix, seg_start, seg_end)

  m <- stringr::str_locate(segment, "[^\\s,]+(?:\\s+[^\\s,]+){0,2}(?=\\s*$)")
  if (is.na(m[1, 1])) return(NULL)
  start <- m[1, 1]

  # A leading condition without its closing comma ("when external power is
  # lost the trap shall"): three words back would reach into the condition
  # ("lost the trap"). When a condition keyword comes earlier in the
  # segment, the actor starts at the last determiner in the window ("the
  # trap"), or is a trailing pronoun ("...is armed it shall"); a leading
  # "then" ("if jammed then the latch shall") is never part of it.
  words <- stringr::str_locate_all(substr(segment, start, m[1, 2]), "\\S+")[[1]]
  if (nrow(words) > 1) {
    word_text <- vapply(seq_len(nrow(words)), function(i) {
      substr(segment, start + words[i, 1] - 1L, start + words[i, 2] - 1L)
    }, character(1))
    cut <- NA_integer_
    det <- which(word_text %in% ACTOR_DETERMINERS)
    det <- det[det > 1]
    if (length(det) > 0) cut <- max(det)
    else if (word_text[length(word_text)] %in% PRONOUN_ACTORS) cut <- length(word_text)
    else if (word_text[1] == "then") cut <- 2L
    if (!is.na(cut)) {
      new_start <- start + words[cut, 1] - 1L
      before <- substr(segment, 1, new_start - 1L)
      if (stringr::str_detect(before, CONDITION_REGEX)) start <- new_start
    }
  }

  # "If the door jams, then the latch shall": the comma already ended the
  # condition, so "then" opens the segment and isn't part of the actor.
  if (stringr::str_detect(substr(segment, start, m[1, 2]), "^then\\s+\\S")) {
    start <- start + stringr::str_locate(substr(segment, start, m[1, 2]), "^then\\s+")[1, 2]
  }

  list(
    start = seg_start + start - 1L,
    end = seg_start + m[1, 2] - 1L,
    text = substr(segment, start, m[1, 2])
  )
}

# Words that open the noun phrase naming an actor - used to find where the
# actor starts when a condition runs into it without a comma.
ACTOR_DETERMINERS <- c("the", "a", "an", "each", "every", "all", "any", "both",
                       "this", "that", "these", "those", "its", "their", "our")

# Not every genuine constraint is numeric - some bound the operating
# envelope categorically instead (e.g. "only in indoor locations", "except
# during maintenance mode"). These phrases are just as testable as a number
# ("does it work indoors?" is a real test) even without a threshold
# attached, so they count toward the same ledger row as a numeric
# constraint rather than needing a whole separate criterion.
CATEGORICAL_CONSTRAINT_REGEX <- "\\bonly (in|at|for|when|within|under|during)\\b|\\bexcept(ing)?\\b|\\bexcluding\\b|\\bunless\\b|\\blimited to\\b|\\brestricted to\\b"

# Condition-clause triggers: the keyword-based forms (when/if/while/during/
# upon/where) are the common case, but "in the presence/absence of X" means
# the same thing ("when X is present/absent") without using any of those
# keywords - a common phrasing in safety-critical requirements (e.g. "...in
# the presence of pets and children"). Shared by check_requirement_logic()
# and compute_highlight_spans() so the two can never drift apart - see
# compute_highlight_spans()'s comment for why that matters.
CONDITION_REGEX <- "\\b(when|if|while|during|upon|where)\\b|\\bin the (presence|absence) of\\b"

# "shall be <past participle>" usually means the actor performing the
# action went unstated (e.g. "the display shall be updated" - by whom?).
# Unlike the banned words above this is a soft signal, not a hard block -
# a well-bounded passive requirement isn't necessarily unverifiable, just
# less clear about who's responsible. "limited"/"restricted" are excluded
# since "shall be limited to X" is the categorical-constraint phrasing this
# app already encourages (see CATEGORICAL_CONSTRAINT_REGEX above) - flagging
# it here would contradict that guidance for the exact same phrase.
PASSIVE_VOICE_REGEX <- "\\bshall be (?!limited\\b|restricted\\b)(\\w+ed\\b|sent\\b|given\\b|done\\b|made\\b|shown\\b|taken\\b|held\\b|set\\b|kept\\b|built\\b|known\\b|seen\\b|found\\b|read\\b|written\\b)"

# Function words that virtually never legitimately end a finished
# requirement sentence. If the draft currently ends on one of these, the
# user is still mid-clause (e.g. "...within", "...battery power and") -
# a strong, cheap signal to hold off on an LLM call regardless of score.
DANGLING_END_WORDS <- c(
  "a", "an", "the", "to", "of", "in", "on", "at", "by", "for", "with",
  "and", "or", "but", "nor", "than", "that", "which", "who", "whom", "whose",
  "shall", "should", "will", "would", "must", "may", "might", "can", "could",
  "within", "less", "greater", "least", "most", "maximum", "minimum",
  "no", "not", "is", "are", "be", "been", "being", "has", "have", "had",
  "this", "these", "those", "its", "their", "if", "when", "while", "during",
  "upon", "where", "as", "so", "because", "after", "before",
  "from", "into", "onto", "about", "over", "under", "through", "between",
  "without", "against", "per", "via", "each", "every", "any", "some", "such"
)

MIN_WORD_COUNT <- 6
GATE_SCORE_THRESHOLD <- 50

# Placeholders stand in for a decision nobody has made yet: bracketed or
# angle-bracketed notes ("[value]", "<limit>", "[confirm this value]"),
# TBD / TBC / TBR / TBA, "XX". A hard block: rigor first - a value the
# AI suggested or the author deferred must be replaced by one an engineer
# can justify before it's committed (docs/design/coaching_alignment.md).
PLACEHOLDER_REGEX <- "\\[[^\\]]*\\]|<[^>]*>|\\b(tbd|tbc|tbr|tba)\\b|\\bx{2,}\\b|confirm this value"

# actor_resolved says whether term resolution found the actor in the model. NA
# (the default) leaves that check inert: an empty model has nothing to match.
check_requirement_logic <- function(text, actor_resolved = NA) {
  req_clean <- stringr::str_squish(text)
  req_lower <- stringr::str_to_lower(req_clean)

  banned_regex <- stringr::str_c("\\b(", stringr::str_c(BANNED_WORDS, collapse = "|"), ")\\b")
  found_banned <- stringr::str_extract_all(mask_banned_word_exceptions(req_lower), banned_regex)[[1]] |> unique()
  found_placeholders <- stringr::str_extract_all(req_clean, stringr::regex(PLACEHOLDER_REGEX, ignore_case = TRUE))[[1]] |> unique()

  has_shall     <- stringr::str_detect(req_lower, "\\bshall\\b")
  has_condition <- stringr::str_detect(req_lower, CONDITION_REGEX)
  has_passive_voice <- stringr::str_detect(req_lower, PASSIVE_VOICE_REGEX)

  # A placeholder where the number belongs ("within [value] ms") is
  # structurally a constraint awaiting its value: it scores as one, and the
  # placeholder itself is what holds the draft at the gate.
  req_for_constraint <- stringr::str_replace_all(req_lower, stringr::regex(PLACEHOLDER_REGEX, ignore_case = TRUE), "1")
  has_constraint <- stringr::str_detect(req_for_constraint, CONSTRAINT_UNIT_REGEX) ||
    stringr::str_detect(req_for_constraint, CONSTRAINT_SYMBOL_REGEX) ||
    stringr::str_detect(req_for_constraint, CATEGORICAL_CONSTRAINT_REGEX)

  # Actor extraction (see locate_actor()). NA when "shall" opens the
  # sentence with nothing before it - e.g. text graduated from the EARS
  # builder and then hand-edited down to just "Shall ..." in the free text
  # box. That's not "not user-centric" (there's no actor to judge either
  # way) - it's a distinct, equally-blocking problem, so has_actor is
  # tracked and gated on separately below rather than letting a missing
  # actor silently read as a passing check.
  actor_loc <- locate_actor(req_lower)
  extracted_actor <- if (is.null(actor_loc)) NA_character_ else actor_loc$text
  has_actor <- !is.na(extracted_actor)
  is_user_centric <- has_actor && isTRUE(stringr::str_detect(extracted_actor, HUMAN_ACTOR_REGEX))

  # A pronoun actor ("...is armed, it shall") leaves the responsible
  # element to be inferred from context - fine in prose, ambiguous in a
  # requirement read on its own. Soft penalty, like passive voice.
  actor_is_pronoun <- has_actor && stringr::str_extract(extracted_actor, "[^\\s]+$") %in% PRONOUN_ACTORS

  shall_count <- stringr::str_count(req_lower, "\\bshall\\b")
  and_count   <- stringr::str_count(req_lower, "\\band\\b")
  # More than one "shall" is always two requirements in one sentence - a
  # hard block (see passes_gate below). Several "and"s with a single
  # "shall" may just be a list of objects, so that stays a soft penalty.
  has_multiple_shall <- shall_count > 1
  is_multiple <- has_multiple_shall || (and_count > 1 && shall_count == 1)

  word_count   <- stringr::str_count(req_clean, "\\S+")
  is_too_short <- word_count > 0 && word_count < MIN_WORD_COUNT

  trailing_stripped  <- stringr::str_replace(req_lower, "[[:punct:][:space:]]+$", "")
  last_word          <- stringr::str_extract(trailing_stripped, "[a-z']+$")
  looks_incomplete   <- !is.na(last_word) && last_word %in% DANGLING_END_WORDS

  # Soft: an actor the model doesn't know is a question, not a hard block.
  resolution_active <- !is.na(actor_resolved) && has_actor

  # Explicit points ledger so the score is auditable in the UI rather than
  # a black box. The three structural criteria sum to 100 so a fully
  # compliant requirement can reach 100; "shall" alone (40) is deliberately
  # below the gate threshold so a bare "the system shall" can't trigger the
  # LLM coach on its own. "Sufficient Detail" is a soft nudge, not a hard
  # block - short-but-complete requirements shouldn't be penalized to zero.
  # "Actor in the model" only appears when term resolution is active, same
  # spirit as the other soft rows - never a hard gate requirement.
  ledger <- data.frame(
    Item = c("Mandatory 'shall'", "Condition Clause", "Constraint or Bound", "System-Centric Actor",
             "Named Actor", "Singular Concept", "Sufficient Detail", "Active Voice"),
    Status = c(has_shall, has_condition, has_constraint, has_actor && !is_user_centric,
               !actor_is_pronoun, !is_multiple, !is_too_short, !has_passive_voice),
    Impact = c(if (has_shall) 40 else 0, if (has_condition) 20 else 0, if (has_constraint) 40 else 0,
               if (!has_actor || is_user_centric) -15 else 0, if (actor_is_pronoun) -10 else 0,
               if (is_multiple) -15 else 0, if (is_too_short) -10 else 0, if (has_passive_voice) -10 else 0),
    stringsAsFactors = FALSE
  )
  if (resolution_active) {
    ledger <- rbind(ledger, data.frame(Item = "Actor in the model", Status = isTRUE(actor_resolved),
                                       Impact = if (isTRUE(actor_resolved)) 0 else -10, stringsAsFactors = FALSE))
  }

  banned_penalty <- length(found_banned) * -10
  total_score <- max(0, min(100, sum(ledger$Impact) + banned_penalty))

  list(
    text = req_clean,
    score = total_score,
    ledger = ledger,
    banned_penalty = banned_penalty,
    passes_gate = (
      has_shall &&
        !has_multiple_shall &&
        length(found_banned) == 0 &&
        length(found_placeholders) == 0 &&
        has_actor &&
        !is_user_centric &&
        !looks_incomplete &&
        total_score >= GATE_SCORE_THRESHOLD
    ),
    extracted_actor = extracted_actor,
    banned_words_found = found_banned,
    placeholders_found = found_placeholders,
    warnings = list(
      missing_shall = !has_shall,
      missing_actor = !has_actor,
      user_as_actor = is_user_centric,
      pronoun_actor = actor_is_pronoun,
      multiple_shall = has_multiple_shall,
      multiple_requirements = is_multiple,
      missing_constraint = !has_constraint,
      too_short = is_too_short,
      looks_incomplete = looks_incomplete,
      actor_not_in_model = resolution_active && !isTRUE(actor_resolved),
      passive_voice_detected = has_passive_voice
    )
  )
}

# Fraction of a pair's longer length taken by their Levenshtein distance,
# so two long-but-nearly-identical sentences score highly even though their
# raw edit distance is larger than two short ones with the same fraction of
# difference. A pure structural/lexical signal - it knows nothing about
# meaning, so it complements rather than replaces the coach's own semantic
# conflict_check.
NEAR_DUPLICATE_SIMILARITY_THRESHOLD <- 0.80

# Cheap, local, deterministic near-duplicate check against already-
# committed requirement text - runs on every keystroke right alongside the
# syntax gate, no LLM call involved. Purely advisory (never affects score or
# passes_gate): a structural resemblance is worth a glance, not proof of an
# actual conflict, which is why this is a separate function the caller
# surfaces as its own soft alert rather than folding into the ledger.
find_near_duplicates <- function(text, existing_texts, threshold = NEAR_DUPLICATE_SIMILARITY_THRESHOLD) {
  text_norm <- stringr::str_squish(stringr::str_to_lower(text))
  if (!nzchar(text_norm) || length(existing_texts) == 0) return(character(0))

  existing_norm <- stringr::str_squish(stringr::str_to_lower(existing_texts))
  # An exact match is the same requirement re-evaluated (e.g. retyping an
  # already-committed draft), not a duplicate of itself.
  candidates <- existing_texts[existing_norm != text_norm]
  if (length(candidates) == 0) return(character(0))
  candidates_norm <- stringr::str_squish(stringr::str_to_lower(candidates))

  dist <- utils::adist(text_norm, candidates_norm)[1, ]
  max_len <- pmax(nchar(text_norm), nchar(candidates_norm), 1)
  similarity <- 1 - (dist / max_len)

  candidates[similarity >= threshold]
}

# Plain-language bullet list of exactly what's keeping a draft from
# passing the gate, built from the same ledger/warnings/banned-words data
# check_requirement_logic() already computed - not a re-derivation. Used
# to ground the "Help Me Fix This" LLM prompt in the draft's actual,
# specific problems rather than a generic restatement of the whole
# rulebook (that's what authoring_rules_prompt_block() in R/llm_coach.R is
# for, and gets sent alongside this).
#
# Each unmet ledger row is described in plain language (rather than its
# terse table label) since this text is read by an LLM as well as people.
GATE_ISSUE_DESCRIPTIONS <- c(
  "Mandatory 'shall'"    = "Missing the word 'shall' for the binding action.",
  "Condition Clause"     = "No triggering condition (when / if / while / during / upon / where). Optional, but add one if it fits naturally.",
  "Constraint or Bound"  = "No concrete constraint: needs a number with a unit (e.g. 500 ms, 2 kg, 10%) or a bounded phrase ('only in ...', 'limited to ...', 'except ...').",
  "System-Centric Actor" = "The actor before 'shall' is missing or is a person (user, operator...); the system or a named component must be the actor.",
  "Named Actor"          = "The actor is a pronoun ('it', 'they', 'this'...); name the system or component instead.",
  "Singular Concept"     = "Bundles more than one action or condition; state exactly one.",
  "Sufficient Detail"    = sprintf("Too short - write a complete sentence of at least %d words.", MIN_WORD_COUNT),
  "Active Voice"         = "Passive voice ('shall be ...ed') hides who performs the action; make the actor do it.",
  "Actor in the model"   = "The actor doesn't match a model element or a glossary alias."
)

describe_gate_issues <- function(logic_result) {
  issues <- character(0)

  unmet <- logic_result$ledger[!logic_result$ledger$Status, , drop = FALSE]
  if (nrow(unmet) > 0) {
    described <- ifelse(unmet$Item %in% names(GATE_ISSUE_DESCRIPTIONS),
                        GATE_ISSUE_DESCRIPTIONS[unmet$Item],
                        paste0(unmet$Item, " (not satisfied)"))
    issues <- c(issues, paste0("- ", described))
  }
  if (length(logic_result$banned_words_found) > 0) {
    issues <- c(issues, sprintf(
      "- Contains banned/vague word(s): %s",
      paste(logic_result$banned_words_found, collapse = ", ")
    ))
  }
  if (isTRUE(logic_result$warnings$looks_incomplete)) {
    issues <- c(issues, "- The sentence looks unfinished (trails off mid-clause).")
  }
  if (isTRUE(logic_result$warnings$multiple_shall)) {
    issues <- c(issues, "- Uses \"shall\" more than once - it states more than one requirement and must be split.")
  }
  if (length(logic_result$placeholders_found) > 0) {
    issues <- c(issues, sprintf(
      "- Contains a placeholder (%s): the author has to decide that value; don't invent one.",
      paste(logic_result$placeholders_found, collapse = ", ")
    ))
  }

  if (length(issues) == 0) {
    return("(No specific structural issues detected - the draft may just be scoring marginally low overall.)")
  }
  paste(issues, collapse = "\n")
}

# Plain-language list of the hard blocks currently stopping a draft from
# passing the gate, in the order a user would most usefully fix them -
# character(0) when the draft passes (or is empty). Mirrors passes_gate in
# check_requirement_logic() exactly; the score threshold is listed last
# since the other fixes usually raise the score anyway. Drives the gate
# status banner above the Live Analysis table, so a "90/100 but locked"
# draft says exactly why instead of leaving it to the points table.
gate_blockers <- function(logic_result) {
  if (!nzchar(logic_result$text) || isTRUE(logic_result$passes_gate)) return(character(0))
  w <- logic_result$warnings
  out <- character(0)
  if (isTRUE(w$missing_shall)) out <- c(out, "Missing the word \"shall\".")
  if (isTRUE(w$multiple_shall)) out <- c(out, "Uses \"shall\" more than once - split it into separate requirements.")
  if (length(logic_result$banned_words_found) > 0) {
    out <- c(out, sprintf("Contains banned wording: %s.",
                          paste0("\"", logic_result$banned_words_found, "\"", collapse = ", ")))
  }
  if (isTRUE(w$missing_actor)) out <- c(out, "No actor before \"shall\" - name the system or component responsible.")
  if (isTRUE(w$user_as_actor)) out <- c(out, sprintf("The actor (\"%s\") is a person - make the system the actor.", logic_result$extracted_actor))
  if (isTRUE(w$looks_incomplete)) out <- c(out, "The sentence looks unfinished.")
  if (length(logic_result$placeholders_found) > 0) {
    out <- c(out, sprintf("Contains a placeholder (%s) - replace it with a value you can justify.",
                          paste0("\"", logic_result$placeholders_found, "\"", collapse = ", ")))
  }
  if (logic_result$score < GATE_SCORE_THRESHOLD) {
    out <- c(out, sprintf("Score is %d/100 - at least %d is needed.", as.integer(logic_result$score), as.integer(GATE_SCORE_THRESHOLD)))
  }
  out
}

# Locates every semantic span worth highlighting, reusing the exact regexes
# check_requirement_logic() scores against so the highlight can never show
# something different from what was actually evaluated. Positions are
# found against the lowercased text but apply unchanged to the
# original-case text, since case-folding doesn't shift character positions
# for the text this app deals with.
compute_highlight_spans <- function(req_clean) {
  req_lower <- stringr::str_to_lower(req_clean)

  collect <- function(locations, category) {
    if (is.null(locations) || nrow(locations) == 0) return(list())
    locations <- locations[!is.na(locations[, 1]), , drop = FALSE]
    if (nrow(locations) == 0) return(list())
    lapply(seq_len(nrow(locations)), function(i) {
      list(start = locations[i, 1], end = locations[i, 2], category = category)
    })
  }

  spans <- list()
  spans <- c(spans, collect(stringr::str_locate_all(req_lower, "\\bshall\\b")[[1]], "shall"))

  # Same locate_actor() the scoring uses, so the highlighted actor is
  # exactly the extracted (and stored) one.
  actor_loc <- locate_actor(req_lower)
  if (!is.null(actor_loc)) {
    spans <- c(spans, list(list(start = actor_loc$start, end = actor_loc$end, category = "actor")))
  }

  spans <- c(spans, collect(stringr::str_locate_all(req_lower, CONDITION_REGEX)[[1]], "condition"))

  spans <- c(spans, collect(stringr::str_locate_all(req_lower, CONSTRAINT_UNIT_REGEX)[[1]], "constraint"))
  spans <- c(spans, collect(stringr::str_locate_all(req_lower, CONSTRAINT_SYMBOL_REGEX)[[1]], "constraint"))
  # More generous than the scoring regex's "±\\s?\\d" (single digit) -
  # purely cosmetic here, so highlighting the full number reads better.
  spans <- c(spans, collect(stringr::str_locate_all(req_lower, "±\\s?\\d+")[[1]], "constraint"))
  spans <- c(spans, collect(stringr::str_locate_all(req_lower, CATEGORICAL_CONSTRAINT_REGEX)[[1]], "constraint"))

  spans
}
