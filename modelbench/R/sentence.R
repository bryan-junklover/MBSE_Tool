# Splits a requirement into the parts the Write screen underlines.

CONSTRAINT_LEAD_IN <- paste0("(within|at least|at most|no more than|no less than|not less than|not more than|",
                             "not exceed|more than|less than|greater than|up to|under|over|at|for|between)\\s+$")

# Same length as the input, so positions stay valid: "within [value] ms" reads as a number.
mask_placeholders <- function(text) {
  stringr::str_replace_all(text, stringr::regex(PLACEHOLDER_REGEX, ignore_case = TRUE),
                           function(m) strrep("1", nchar(m)))
}

# Where the constraint sits in the text after "shall", or NULL.
locate_constraint <- function(lower, from) {
  masked <- mask_placeholders(lower)
  tail_text <- substr(masked, from, nchar(masked))
  hits <- rbind(
    stringr::str_locate(tail_text, CONSTRAINT_UNIT_REGEX),
    stringr::str_locate(tail_text, CONSTRAINT_SYMBOL_REGEX)
  )
  hits <- hits[!is.na(hits[, 1]), , drop = FALSE]
  if (nrow(hits) > 0L) {
    hit <- hits[which.min(hits[, 1]), ]
    start <- hit[["start"]]
    before <- substr(tail_text, 1L, start - 1L)
    lead <- stringr::str_locate(before, CONSTRAINT_LEAD_IN)
    if (!is.na(lead[1, 1])) start <- lead[1, 1]
    return(c(from + start - 1L, from + hit[["end"]] - 1L))
  }
  categorical <- stringr::str_locate(tail_text, CATEGORICAL_CONSTRAINT_REGEX)
  if (is.na(categorical[1, 1])) return(NULL)
  end <- nchar(sub("[.;,\\s]+$", "", tail_text, perl = TRUE))
  c(from + categorical[1, 1] - 1L, from + end - 1L)
}

# Data frame of (text, part) whose text pastes back to the squished input. Parts:
# condition, actor, shall, response, constraint, plain.
parse_sentence <- function(text) {
  clean <- stringr::str_squish(text)
  lower <- stringr::str_to_lower(clean)
  segments <- function(starts, ends, parts) {
    keep <- ends >= starts
    data.frame(text = substring(clean, starts[keep], ends[keep]), part = parts[keep], stringsAsFactors = FALSE)
  }
  if (!nzchar(clean)) return(data.frame(text = character(0), part = character(0), stringsAsFactors = FALSE))
  shall <- stringr::str_locate(lower, "\\bshall\\b")
  actor <- locate_actor(lower)
  if (is.na(shall[1, 1]) || is.null(actor)) return(segments(1L, nchar(clean), "plain"))

  spans <- new.env()
  spans$starts <- integer(0)
  spans$ends <- integer(0)
  spans$parts <- character(0)
  push <- function(start, end, part) {
    spans$starts <- c(spans$starts, start)
    spans$ends <- c(spans$ends, end)
    spans$parts <- c(spans$parts, part)
  }

  prefix <- substr(lower, 1L, actor$start - 1L)
  condition <- stringr::str_locate(prefix, CONDITION_REGEX)
  cursor <- 1L
  if (!is.na(condition[1, 1])) {
    condition_end <- nchar(sub("[,\\s]+$", "", prefix, perl = TRUE))
    push(cursor, condition[1, 1] - 1L, "plain")
    push(condition[1, 1], condition_end, "condition")
    cursor <- condition_end + 1L
  }
  push(cursor, actor$start - 1L, "plain")
  push(actor$start, actor$end, "actor")
  push(actor$end + 1L, shall[1, 1] - 1L, "plain")
  push(shall[1, 1], shall[1, 2], "shall")

  response_start <- shall[1, 2] + 2L
  push(shall[1, 2] + 1L, shall[1, 2] + 1L, "plain")
  constraint <- locate_constraint(lower, response_start)
  body_end <- nchar(sub("[.;,\\s]+$", "", clean, perl = TRUE))
  if (is.null(constraint)) {
    push(response_start, body_end, "response")
    push(body_end + 1L, nchar(clean), "plain")
  } else {
    response_end <- constraint[1] - 1L
    while (response_end >= response_start && substr(clean, response_end, response_end) == " ") response_end <- response_end - 1L
    push(response_start, response_end, "response")
    push(response_end + 1L, constraint[1] - 1L, "plain")
    push(constraint[1], constraint[2], "constraint")
    push(constraint[2] + 1L, nchar(clean), "plain")
  }
  segments(spans$starts, spans$ends, spans$parts)
}

EARS_PATTERNS <- c("Ubiquitous" = "ubiquitous", "Event-driven" = "event", "State-driven" = "state",
                   "Unwanted behaviour" = "unwanted", "Optional feature" = "optional")

# Assembles an EARS sentence. A blank value is written as [value], never a guess.
build_ears <- function(pattern, actor, response, trigger = "", bound = "within", value = "", unit = "") {
  blank <- function(x, label) if (nzchar(trimws(x))) trimws(x) else label
  opening <- switch(pattern,
                    ubiquitous = "",
                    event = sprintf("When %s, ", blank(trigger, "[trigger]")),
                    state = sprintf("While %s, ", blank(trigger, "[state]")),
                    unwanted = sprintf("If %s, then ", blank(trigger, "[condition]")),
                    optional = sprintf("Where %s, ", blank(trigger, "[feature]")),
                    stop("Unknown EARS pattern: ", pattern, call. = FALSE))
  subject <- paste0("the ", tolower(blank(actor, "[actor]")))
  constraint <- if (nzchar(unit) || nzchar(trimws(value))) {
    paste0(" ", paste(c(bound, blank(value, "[value]"), if (nzchar(unit)) unit), collapse = " "))
  } else {
    ""
  }
  sentence <- sprintf("%s%s shall %s%s.", opening, subject, blank(response, "[response]"), constraint)
  paste0(toupper(substr(sentence, 1L, 1L)), substr(sentence, 2L, nchar(sentence)))
}
