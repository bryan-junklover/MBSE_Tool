gate <- function(text, actor_resolved = NA) check_requirement_logic(text, actor_resolved)
str_to_lower <- stringr::str_to_lower

test_that("a complete requirement passes the gate with a full score", {
  r <- gate("When external power is lost, the system shall transition to battery power within 500 ms.")
  expect_true(r$passes_gate)
  expect_equal(r$score, 100)
  expect_equal(r$extracted_actor, "the system")
})

test_that("hard blocks stop the gate regardless of score", {
  expect_false(gate("The system will transition to battery within 500 ms.")$passes_gate)          # no shall
  expect_false(gate("When power is lost, the system shall quickly switch within 500 ms.")$passes_gate)  # banned
  expect_false(gate("Shall transition to battery power within 500 ms.")$passes_gate)              # no actor
  expect_false(gate("When power is lost, the operator shall switch to battery within 5 min.")$passes_gate)  # human actor
  expect_false(gate("When power is lost, the system shall transition to battery within")$passes_gate)  # trails off
})

test_that("escape clauses are hard-blocking banned phrases", {
  r <- gate("The system shall log faults within 5 ms as appropriate.")
  expect_false(r$passes_gate)
  expect_true("as appropriate" %in% r$banned_words_found)
})

test_that("passive voice is a soft penalty, but 'shall be limited to' is not passive", {
  passive <- gate("When armed, the display shall be updated within 200 ms.")
  expect_true(passive$warnings$passive_voice_detected)
  limited <- gate("The system shall be limited to indoor residential locations.")
  expect_false(limited$warnings$passive_voice_detected)
})

test_that("the actor-in-model check is inert without resolution and a soft penalty with it", {
  text <- "When armed, the FCC shall close the door within 2 s."
  expect_false(gate(text)$warnings$actor_not_in_model)
  expect_equal(gate(text)$score, 100)
  expect_false(gate(text, TRUE)$warnings$actor_not_in_model)
  expect_equal(gate(text, TRUE)$score, 100)
  miss <- gate(text, FALSE)
  expect_true(miss$warnings$actor_not_in_model)
  expect_equal(miss$score, 90)
  expect_true(miss$passes_gate)
})

test_that("'then' after a comma isn't part of the actor", {
  expect_equal(gate("If the door jams, then the latch shall release within 2 s.")$extracted_actor, "the latch")
})

# --- B2: actor extraction must not bleed across a clause boundary ------------

test_that("actor stops at the comma ending a leading condition clause", {
  expect_equal(gate("When a mouse enters the chamber, the trap shall close the door within 2 s.")$extracted_actor, "the trap")
  expect_equal(gate("When humidity exceeds 90%, the trap shall seal the bait within 5 s.")$extracted_actor, "the trap")
  expect_equal(gate("If power is lost, the system shall retain the log for 30 days.")$extracted_actor, "the system")
})

test_that("a leading condition without its comma doesn't leak into the actor", {
  expect_equal(gate("when external power is lost the trap shall switch on a red light")$extracted_actor, "the trap")
  expect_equal(gate("When external power is lost the system shall switch to battery within 5 ms.")$extracted_actor, "the system")
  expect_equal(gate("While the trap is armed each sensor shall report within 1 s.")$extracted_actor, "each sensor")
  expect_equal(gate("If the door jams then the latch shall release within 2 s.")$extracted_actor, "the latch")
  expect_equal(gate("When armed it shall close the door within 2 s.")$extracted_actor, "it")
  expect_true(gate("When armed it shall close the door within 2 s.")$warnings$pronoun_actor)
  # A human actor is still caught when the comma is missing.
  expect_true(gate("When power is lost the operator shall switch to battery within 5 min.")$warnings$user_as_actor)
  # The highlight follows the same actor.
  text <- "When external power is lost the trap shall switch on a red light within 1 s."
  actor <- Filter(function(sp) sp$category == "actor", compute_highlight_spans(text))[[1]]
  expect_equal(substr(str_to_lower(text), actor$start, actor$end), "the trap")
})

test_that("without a condition, a determiner inside the window doesn't move the actor", {
  # No condition keyword, so nothing is trimmed and the 3-word window
  # stands. "in the kitchen" is a known limitation (a prepositional phrase
  # after the actor), left alone deliberately: trimming at the determiner
  # would give "the kitchen", which is no better.
  expect_equal(gate("The trap door shall support a load of at least 2 kg.")$extracted_actor, "the trap door")
  expect_equal(gate("The display in the kitchen shall show the time within 1 s.")$extracted_actor, "in the kitchen")
})

test_that("a parenthetical clause between actor and shall keeps the real actor", {
  r <- gate("The trap, when armed, shall close the door within 2 s.")
  expect_equal(r$extracted_actor, "the trap")
  expect_true(r$passes_gate)
})

test_that("a sentence opening with shall still reports a missing actor", {
  r <- gate("Shall close the door within 2 s.")
  expect_true(is.na(r$extracted_actor))
  expect_true(r$warnings$missing_actor)
})

test_that("a human role after a condition clause is still caught", {
  r <- gate("When the trap closes, the user shall release the mouse within 24 hours.")
  expect_equal(r$extracted_actor, "the user")
  expect_true(r$warnings$user_as_actor)
})

# --- G1: 'safe state' is standard safety wording, not a vague qualifier ------

test_that("'safe state' and 'fail-safe' are not banned, bare 'safe'/'safely' still are", {
  expect_true(gate("When a fault is detected, the controller shall enter a safe state within 100 ms.")$passes_gate)
  expect_true(gate("When power is lost, the latch shall move to its fail-safe position within 50 ms.")$passes_gate)
  expect_false(gate("The trap shall be safe.")$passes_gate)
  expect_false(gate("When armed, the trap shall operate safely within 2 s.")$passes_gate)
})

test_that("a bare 'safe' elsewhere in a sentence with 'safe state' is still flagged", {
  r <- gate("When a fault occurs, the controller shall enter a safe state and remain safe within 100 ms.")
  expect_true("safe" %in% r$banned_words_found)
})

# --- G2: human-role words naming a system component --------------------------

test_that("component names containing a human-role word are system actors", {
  for (actor in c("the user interface", "the operator console", "the customer database",
                  "the human-machine interface")) {
    r <- gate(sprintf("When the lid opens, %s shall display the status within 2 s.", actor))
    expect_false(r$warnings$user_as_actor, info = actor)
    expect_true(r$passes_gate, info = actor)
  }
  expect_true(gate("When the lid opens, the operator shall log the event within 2 s.")$warnings$user_as_actor)
})

# --- G3/G4: constraint units ---------------------------------------------------

test_that("common engineering units count as constraints", {
  bounded <- c(
    "The heater surface shall not exceed 40C.",
    "The heater surface shall not exceed 45 °C.",
    "The battery shall operate at 20 degrees C.",
    "The trap shall close the door within 5 s.",
    "The enclosure shall withstand a drop of 1.5 m onto concrete.",
    "The housing shall measure no more than 6.5\" in length.",
    "The housing shall measure no more than 160 mm in length.",
    "The battery shall power the trap for 12 months.",
    "The system shall retain the event log for 30 days.",
    "The motor shall deliver 15 N of closing force.",
    "The motor shall draw no more than 500 mA.",
    "The seal shall withstand 20 kPa of pressure.",
    "The fan shall spin at no more than 1200 rpm.",
    "The latch shall survive 10000 cycles."
  )
  for (t in bounded) expect_false(gate(t)$warnings$missing_constraint, info = t)
})

test_that("numbers without a unit are still not constraints", {
  expect_true(gate("The system shall keep the first 10 in the list.")$warnings$missing_constraint)
  expect_true(gate("The trap shall close the door within moments.")$warnings$missing_constraint)
})

# --- Sample regression (handoff: 10 pass / 9 fail) ---------------------------

test_that("the 19-item sample set keeps its expected pass/fail split", {
  texts <- readLines(test_path("fixtures", "sample_requirements.txt"), warn = FALSE)
  texts <- texts[nzchar(trimws(texts))]
  expect_length(texts, 19)
  res <- lapply(texts, gate)
  passes <- vapply(res, function(r) r$passes_gate, logical(1))
  expect_equal(which(passes), 1:10)
  # None of the extracted actors should carry a clause fragment.
  actors <- vapply(res, function(r) if (is.na(r$extracted_actor)) "" else r$extracted_actor, character(1))
  expect_false(any(grepl(",", actors)))
})

# --- Highlighter ----------------------------------------------------------------

test_that("the highlighted actor matches the extracted actor", {
  text <- "When humidity exceeds 90%, the trap shall seal the bait within 5 s."
  spans <- compute_highlight_spans(text)
  actor <- Filter(function(s) s$category == "actor", spans)[[1]]
  expect_equal(substr(str_to_lower(text), actor$start, actor$end), gate(text)$extracted_actor)
})

# --- Near duplicates / issue descriptions ------------------------------------

test_that("near-duplicate detection ignores exact matches and flags close variants", {
  existing <- c("The trap shall close the door within 2 seconds.", "The trap shall weigh under 2 kg.")
  expect_equal(find_near_duplicates("The trap shall close the door within 3 seconds.", existing), existing[1])
  expect_length(find_near_duplicates("The trap shall close the door within 2 seconds.", existing), 0)
  expect_length(find_near_duplicates("", existing), 0)
})

test_that("describe_gate_issues names the specific problems", {
  out <- describe_gate_issues(gate("The trap shall be robust and"))
  expect_match(out, "robust")
  expect_match(out, "unfinished")
  expect_match(describe_gate_issues(gate("When power is lost, the system shall switch within 5 ms.")),
               "No specific structural issues")
})

# --- G5: pronoun actors are a soft penalty ---------------------------------------

test_that("a pronoun actor costs 10 points but doesn't block the gate", {
  r <- gate("When the trap is armed, it shall close the door within 500 ms.")
  expect_equal(r$extracted_actor, "it")
  expect_true(r$warnings$pronoun_actor)
  expect_equal(r$score, 90)
  expect_true(r$passes_gate)
  named <- gate("When the trap is armed, the trap shall close the door within 500 ms.")
  expect_false(named$warnings$pronoun_actor)
  expect_equal(named$score, 100)
})

# --- G6: two "shall"s are a hard block ---------------------------------------------

test_that("more than one 'shall' hard-blocks the gate however high the score", {
  r <- gate("When triggered, the door shall close and the latch shall lock within 50 ms.")
  expect_true(r$warnings$multiple_shall)
  expect_gte(r$score, GATE_SCORE_THRESHOLD)
  expect_false(r$passes_gate)
  expect_match(describe_gate_issues(r), "more than once")
})

test_that("several 'and's with a single 'shall' stay a soft penalty", {
  r <- gate("When armed, the trap shall log the time, date and temperature and humidity within 5 s.")
  expect_false(r$warnings$multiple_shall)
  expect_true(r$warnings$multiple_requirements)
  expect_true(r$passes_gate)
})

# --- gate_blockers ------------------------------------------------------------------

test_that("gate_blockers lists exactly the hard blocks, and nothing for passing or empty drafts", {
  expect_length(gate_blockers(gate("When armed, the trap shall close the door within 2 s.")), 0)
  expect_length(gate_blockers(gate("")), 0)
  b <- gate_blockers(gate("When a fault occurs, the operator shall quickly reset the trap within 2 s."))
  expect_true(any(grepl("\"quickly\"", b, fixed = TRUE)))
  expect_true(any(grepl("is a person", b, fixed = TRUE)))
  expect_true(any(grepl("more than once", gate_blockers(gate("The door shall close and the latch shall lock within 5 s.")))))
  expect_true(any(grepl("Score is 40/100", gate_blockers(gate("The system shall maintain power levels.")), fixed = TRUE)))
})

# --- Placeholders (docs/design/coaching_alignment.md) --------------------------------

test_that("placeholders hard-block the gate, however high the score", {
  for (t in c("When the lid opens, the trap shall flash an LED within 1 s [confirm this value].",
              "When the lid opens, the trap shall flash an LED within [value] s.",
              "When the lid opens, the trap shall flash an LED within TBD s.",
              "When the lid opens, the trap shall flash an LED within tbc s.",
              "When the lid opens, the trap shall flash an LED within XX s.",
              "When the lid opens, the trap shall log <event> within 2 s.")) {
    r <- check_requirement_logic(t)
    expect_false(r$passes_gate, info = t)
    expect_true(length(r$placeholders_found) > 0, info = t)
    expect_match(paste(gate_blockers(r), collapse = " "), "replace it with a value you can justify", info = t)
    expect_match(describe_gate_issues(r), "don't invent one", info = t)
  }
  expect_equal(check_requirement_logic("When the lid opens, the trap shall flash an LED within 1 s [confirm this value].")$score, 100)
})

test_that("a placeholder where the number belongs still counts as the constraint's structure", {
  r <- check_requirement_logic("When the lid opens, the trap shall flash an LED within [value] s.")
  expect_equal(r$score, 100)
  expect_equal(gate_blockers(r), "Contains a placeholder (\"[value]\") - replace it with a value you can justify.")
})

test_that("ordinary words that merely contain the letters aren't placeholders", {
  r <- check_requirement_logic("When the lid opens, the trap shall flash the max LED within 1 s.")
  expect_length(r$placeholders_found, 0)
  expect_true(r$passes_gate)
})
