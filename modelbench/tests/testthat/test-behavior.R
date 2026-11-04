SCOPE <- list(variables = c(a = "number", b = "number", p = "bool", q = "bool"), property = function(element, property) if (element == "Battery" && property == "cap") data.frame(value_state = "set", num_value = 5000))
VARS <- list(a = 2, b = 3, p = TRUE, q = FALSE)

battery_card <- function(con) Filter(function(card) card$entities[[1]]$id == "V-0013", compare_baselines(con, "BL-03"))[[1]]

test_that("A-001 passes on the sample and is recorded with the model hash", {
  con <- local_built_sample()
  outcome <- run_assertion(con, "A-001")
  expect_identical(outcome$result, "pass")
  expect_identical(outcome$message, "entered Return to home 3.0 s after gnss_lost")
  expect_identical(outcome$current, "Return to home")
  expect_equal(outcome$clock, 3)
  run <- DBI::dbGetQuery(con, "SELECT result, model_hash FROM sm_run WHERE assertion_id = 'A-001'")
  expect_identical(run$result, "pass")
  expect_identical(run$model_hash, model_hash(con))
  expect_true(assertion_status(con, "SM-001")$current)
})

test_that("v_sm_unhandled returns exactly the 3 sample pairs", {
  con <- local_built_sample()
  pairs <- unhandled_pairs(con, "SM-001")
  expect_setequal(paste(pairs$event_name, pairs$state_name), c("battery_low Hold position", "link_lost Return to home", "link_lost Land"))
})

test_that("an ignore needs a note and clears the pair, and a transition does too", {
  con <- local_built_sample()
  expect_error(ignore_pair(con, "S-LND", "E-06", "  "), "note")
  ignore_pair(con, "S-LND", "E-06", "Already landing")
  save_transition(con, fields = list(sm_id = "SM-001", from_state_id = "S-HLD", to_state_id = "S-RTH", event_id = "E-05"), note = "x")
  expect_identical(unhandled_pairs(con, "SM-001")$event_name, "link_lost")
})

test_that("the simulator follows handoff 5.8", {
  con <- local_built_sample()
  m <- load_machine(con, "SM-001")
  sim <- sim_start(m)
  expect_identical(sim$current, "S-DIS")
  sim <- sim_fire(m, sim, "link_lost")
  expect_identical(sim$trace[[1]]$note, "link_lost ignored in Disarmed")
  expect_identical(sim$trace[[1]]$level, "normal")
  for (event in c("arm", "takeoff", "alt_reached", "link_lost")) sim <- sim_fire(m, sim, event)
  expect_identical(sim$current, "S-HLD")
  expect_false(sim$variables$link_ok)
  # In scope, unhandled: the log line is amber.
  sim <- sim_fire(m, sim, "battery_low")
  expect_identical(sim$trace[[length(sim$trace)]]$level, "gap")
  # The placeholder hold stops the run.
  stopped <- sim_fire(m, sim, "hold_timeout")
  expect_identical(stopped$status, "error")
  expect_identical(stopped$error, "hold_timeout needs a value")
  # A stopped run stays stopped.
  expect_identical(sim_fire(m, stopped, "link_restored")$current, "S-HLD")
})

test_that("two true transitions at the same priority stop the run", {
  con <- local_built_sample()
  save_transition(con, fields = list(sm_id = "SM-001", from_state_id = "S-DIS", to_state_id = "S-LND", event_id = "E-01", guard_expr = "soc > 1"), note = "x")
  m <- load_machine(con, "SM-001")
  expect_match(sim_fire(m, sim_start(m), "arm")$error, "equally ranked")
  # Priority breaks the tie.
  twin <- DBI::dbGetQuery(con, "SELECT id FROM sm_transition WHERE guard_expr = 'soc > 1'")$id
  save_transition(con, twin, list(priority = 5L), note = "x")
  m <- load_machine(con, "SM-001")
  expect_identical(sim_fire(m, sim_start(m), "arm")$current, "S-LND")
})

test_that("assertions report fail and error", {
  con <- local_built_sample()
  m <- load_machine(con, "SM-001")
  script <- list(list(start = "S-MIS"), list(event = "gnss_lost", set = list(gnss_valid = FALSE), hold_s = 3))
  expect_identical(check_assertion(m, script, list(state = "S-LND"))$result, "fail")
  late <- check_assertion(m, script, list(state = "S-RTH", within_s = 2))
  expect_identical(late$result, "fail")
  expect_match(late$message, "over the 2.0 s limit")
  script[[2]]$hold_s <- 5
  expect_match(check_assertion(m, script, list(state = "S-RTH"))$message, "says hold 5 s")
  expect_identical(check_assertion(m, list(list(start = "S-MIS"), list(advance_s = 5)), list(vars = list(soc = 78)))$result, "pass")
  expect_identical(check_assertion(m, list(list(start = "S-MIS")), list(vars = list(soc = 1)))$result, "fail")
  expect_identical(check_assertion(m, list(list(nothing = 1)), list(state = "S-MIS"))$result, "error")
})

test_that("a recorded run goes out of date when the model changes", {
  con <- local_built_sample()
  run_assertion(con, "A-001")
  save_state(con, "S-LND", list(name = "Land now"), note = "x")
  expect_false(assertion_status(con, "SM-001")$current)
})

test_that("the language parses, types and evaluates", {
  expect_true(eval_guard("a + b * 2 == 8 and not q", VARS))
  expect_true(eval_guard("(a + b) * 2 == 10", VARS))
  expect_true(eval_guard("a < b or q", VARS))
  expect_false(eval_guard("not (p and a <= b)", VARS))
  expect_true(eval_guard("", VARS))
  expect_true(eval_guard(NA_character_, VARS))
  out <- apply_effect("a := a + 1; b := a * 2; q := true", VARS)
  expect_equal(c(out$a, out$b), c(3, 6))
  expect_true(out$q)
  expect_true(eval_guard("prop(Battery.cap) > 100", VARS, SCOPE$property))
  for (good in c("a == 1", "p", "not p", "prop(Battery.cap) > a", "(p)")) expect_no_error(check_guard(good, SCOPE))
  for (bad in c("a", "a + 1", "p + 1", "a and p", "a == p", "nope == 1", "prop(Nope.cap) > 1", "a ==", "(a == 1", "a == 1 2", "1.2.3 > 1", "a $ b", "system('x')", "prop(Battery)")) {
    expect_error(check_guard(bad, SCOPE), class = "modelbench_refused", label = bad)
  }
  for (bad in c("a", "a := p", "p := 1", "nope := 1", "a := 1;", "a = 1")) expect_error(check_effect(bad, SCOPE), class = "modelbench_refused", label = bad)
  expect_error(eval_guard("a / 0 > 1", VARS), class = "modelbench_sim_error")
})

test_that("a model property with no value stops the run instead of guessing", {
  blank <- function(element, property) data.frame(value_state = "placeholder", num_value = NA_real_)
  expect_error(eval_guard("prop(Battery.cap) > 1", VARS, blank), "needs a value")
})

test_that("expressions never reach eval or parse", {
  source_text <- paste(sub("#.*$", "", readLines(test_path("..", "..", "R", "sm_expr.R"))), collapse = "\n")
  expect_false(grepl("\\b(eval|parse|str2lang|str2expression)\\(", gsub("(eval_guard|parse_guard|parse_effect)\\(", "", source_text)))
  tripwire <- tempfile()
  expect_error(check_guard(sprintf("file.create('%s')", tripwire), SCOPE), class = "modelbench_refused")
  expect_false(file.exists(tripwire))
})

# Random well-typed expressions from the grammar.
gen_number <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.3) return(sample(c("a", "b", "1", "2.5", "prop(Battery.cap)"), 1L))
  switch(sample(3L, 1L),
         sprintf("%s %s %s", gen_number(depth - 1L), sample(c("+", "-", "*"), 1L), gen_number(depth - 1L)),
         sprintf("(%s)", gen_number(depth - 1L)),
         sprintf("%s / %s", gen_number(depth - 1L), sample(c("2", "4"), 1L)))
}
gen_bool <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.2) return(sample(c("p", "q", "true", "false"), 1L))
  switch(sample(4L, 1L),
         sprintf("%s %s %s", gen_number(depth - 1L), sample(c("==", "!=", "<", "<=", ">", ">="), 1L), gen_number(depth - 1L)),
         sprintf("%s %s %s", gen_bool(depth - 1L), sample(c("and", "or"), 1L), gen_bool(depth - 1L)),
         sprintf("not %s", gen_bool(depth - 1L)),
         sprintf("(%s)", gen_bool(depth - 1L)))
}

test_that("property: well-typed guards pass the gate, evaluate to one logical, and repeat exactly", {
  withr::local_seed(42)
  for (i in seq_len(300)) {
    text <- gen_bool(5L)
    expect_no_error(check_guard(text, SCOPE))
    first <- expr_value(parse_guard(text), VARS, SCOPE$property)
    expect_true(is.logical(first) && length(first) == 1L && !is.na(first), label = text)
    expect_identical(expr_value(parse_guard(text), VARS, SCOPE$property), first, label = text)
  }
})

test_that("property: an unknown identifier anywhere is refused", {
  withr::local_seed(43)
  for (i in seq_len(200)) {
    text <- gen_bool(4L)
    spoiled <- sprintf("%s and unknown_%d == 1", text, i)
    expect_error(check_guard(spoiled, SCOPE), "isn't a variable", label = spoiled)
  }
})

test_that("property: junk always terminates in a refusal or a result, never a crash", {
  withr::local_seed(44)
  pieces <- c("a", "1", "(", ")", "and", "or", "not", "==", ":=", ";", "+", "*", "/", "prop(", "true", "$", ".", "\"", "\\", " ")
  for (i in seq_len(400)) {
    text <- paste(sample(pieces, sample(12L, 1L), replace = TRUE), collapse = " ")
    result <- tryCatch({
      check_guard(text, SCOPE)
      "ok"
    }, modelbench_refused = function(e) "refused")
    expect_true(result %in% c("ok", "refused"), label = text)
  }
})

test_that("a very deep or long expression is refused rather than overflowing", {
  expect_error(check_guard(paste0(strrep("(", 500), "p", strrep(")", 500)), SCOPE), "too deeply", class = "modelbench_refused")
  expect_error(check_guard(paste(c(rep("not", 200), "p"), collapse = " "), SCOPE), "too deeply")
  expect_error(check_guard(strrep("a + ", 1000), SCOPE), "too long")
})

test_that("editing refuses bad guards, effects and certain ties", {
  con <- local_built_sample()
  expect_error(save_transition(con, "T-05", list(guard_expr = "nope == 1"), note = "x"), "isn't a variable")
  expect_error(save_transition(con, "T-05", list(guard_expr = "soc"), note = "x"), "true or false")
  expect_error(save_transition(con, "T-05", list(effect_expr = "soc := true"), note = "x"), "holds a number")
  expect_error(save_state(con, "S-MIS", list(entry_effect = "nope := 1"), note = "x"), "isn't a variable")
  expect_error(save_transition(con, fields = list(sm_id = "SM-001", from_state_id = "S-DIS", to_state_id = "S-LND", event_id = "E-01"), note = "x"), "same event, guard and priority")
  expect_error(add_assertion(con, "SM-001", NA_character_, "x", "not json", "{}"), "valid JSON")
  written <- add_assertion(con, "SM-001", "SYS-012", "Hold", '[{"start":"S-HLD"}]', '{"state":"S-HLD"}')
  expect_identical(run_assertion(con, written$id)$result, "pass")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM link WHERE target_kind = 'sm_assertion' AND target_id = ?", params = list(written$id))$n, 1L)
})

test_that("behavioural impact for the battery change lists 4 scenarios", {
  con <- local_built_sample()
  impact <- behavior_impact(con, battery_card(con))
  expect_setequal(vapply(impact, function(one) one$id, ""), c("SC-001", "SC-002", "SC-003", "SC-004"))
  activity <- Filter(function(one) one$id == "SC-003", impact)[[1]]
  expect_setequal(activity$changed_nodes, c("N-4", "N-5"))
  expect_identical(activity$hops, 1L)
  expect_identical(activity$inputs[[1]]$before, "5000 mAh")
  expect_identical(activity$inputs[[1]]$after, "4200 mAh")
  expect_identical(Filter(function(one) one$id == "SC-004", impact)[[1]]$changed_steps, "ST-19")
  diagram <- activity_diagram(con, "SC-003", impact_callouts(activity$inputs, "after"))
  expect_identical(diagram$nodes$id[diagram$nodes$gap], c("N-4", "N-5"))
  expect_match(diagram$nodes$gap_note[diagram$nodes$id == "N-4"], "4200 mAh")
})

test_that("a field change highlights steps using that interface", {
  con <- local_built_sample()
  card <- Filter(function(card) card$entities[[1]]$id == "FLD-0005", compare_baselines(con, "BL-03"))[[1]]
  impact <- behavior_impact(con, card)
  expect_setequal(Filter(function(one) one$id == "SC-002", impact)[[1]]$changed_steps, c("ST-01", "ST-06"))
})

test_that("consistency counts messages and offers the missing field", {
  con <- local_built_sample()
  check <- scenario_consistency(con, "SC-002")
  expect_identical(c(check$mapped, check$total), c(6L, 8L))
  expect_length(check$problems, 2L)
  expect_identical(check$problems[[1]]$field_name, "abort")
  add_field_for_step(con, "IF-004", "abort")
  expect_identical(scenario_consistency(con, "SC-002")$mapped, 8L)
})

test_that("steps can be added, edited and reordered through the write path", {
  con <- local_built_sample()
  reorder_steps(con, "SC-004", c("ST-19", "ST-17", "ST-18"))
  expect_identical(scenario_steps(con, "SC-004")$id, c("ST-19", "ST-17", "ST-18"))
  expect_identical(scenario_steps(con, "SC-004")$seq, 1:3)
  expect_error(reorder_steps(con, "SC-004", c("ST-19", "ST-17")), "doesn't match")
  written <- add_step(con, "SC-004", "fragment")
  steps <- scenario_steps(con, "SC-004")
  expect_identical(steps$step_kind[4:5], c("fragment_open", "fragment_close"))
  add_step(con, "SC-004", "note")
  expect_identical(nrow(scenario_steps(con, "SC-004")), 6L)
  update_step_field(con, "ST-17", "timing", "[value]")
  expect_identical(step_label(scenario_steps(con, "SC-004")[2, ]), "link lost ([value] s)")
  update_step_field(con, "ST-17", "timing", "4")
  expect_identical(step_label(scenario_steps(con, "SC-004")[2, ]), "link lost (4 s)")
  update_step_field(con, "ST-17", "timing", "")
  expect_identical(step_label(scenario_steps(con, "SC-004")[2, ]), "link lost")
  expect_error(update_step_field(con, "ST-17", "timing", "soon"), "number of seconds")
  expect_error(update_step_field(con, "ST-17", "label", " "), "needs a label")
  expect_error(add_step(con, new_scenario(con, "Empty")$id, "message"), "lifeline")
})

test_that("activity edits reach what impact reads", {
  con <- local_built_sample()
  node <- add_act_node(con, "SC-003", "action", "Check reserve")$id
  set_node_reads(con, node, list(list(owner_kind = "element", owner_id = "C-003", property = "battery_capacity")))
  expect_true(node %in% Filter(function(one) one$id == "SC-003", behavior_impact(con, battery_card(con)))[[1]]$changed_nodes)
  expect_error(add_act_edge(con, "SC-003", node, "nope"), "in this activity")
  expect_no_error(add_act_edge(con, "SC-003", "N-1", node))
  set_node_reads(con, node, list())
  expect_true(is.na(read_entity(con, "act_node", node)$reads_json))
})

sketch_result <- function(value) list(ok = TRUE, value = value, call_id = NA_integer_)

test_that("a sketch is unreviewed throughout, with placeholders, and invents nothing", {
  con <- local_built_sample()
  prompt <- behavior_sketch_prompt(con, "Battery runs out")
  e <- prompt$candidates$elements
  handle <- function(id) e$n[e$id == id]
  iface <- prompt$candidates$interfaces$n[1]
  before <- count_of(con, "SELECT count(*) AS n FROM element")
  stored <- store_sketch_answer(con, prompt, sketch_result(list(
    steps = list(
      list(kind = "message", from = handle("C-003"), to = handle("C-004"), interface = iface, label = "soc below 20 percent", has_guard = "yes", has_timing = "yes"),
      list(kind = "note", from = handle("C-004"), to = "", interface = "", label = "decide", has_guard = "no", has_timing = "no"),
      list(kind = "message", from = "E999", to = handle("C-004"), interface = "", label = "dropped", has_guard = "no", has_timing = "no")),
    nodes = list(), edges = list(),
    missing = list(list(title = "What cools the battery?", body = "No thermal element exists.")))), "Sketch")
  scenario <- read_entity(con, "scenario", stored$id)
  expect_identical(c(scenario$origin, scenario$review_state), c("ai", "unreviewed"))
  steps <- scenario_steps(con, stored$id)
  expect_identical(nrow(steps), 2L)
  expect_true(all(steps$origin == "ai" & steps$review_state == "unreviewed"))
  expect_identical(steps$timing_state[1], "placeholder")
  expect_identical(steps$guard[1], "[guard]")
  expect_match(steps$label[1], "[value]", fixed = TRUE)
  expect_identical(count_of(con, "SELECT count(*) AS n FROM element"), before)
  question <- DBI::dbGetQuery(con, "SELECT proposal_kind, title FROM ai_proposal WHERE id = ?", params = list(stored$questions[[1]]))
  expect_identical(question$proposal_kind, "question")
  # The whole sketch blocks a baseline until reviewed.
  expect_gt(nrow(baseline_blockers(con)), 8L)
  expect_false(grepl("C-003", prompt$message, fixed = TRUE))
})

test_that("an activity sketch makes unreviewed nodes and drops bad edges", {
  con <- local_built_sample()
  prompt <- behavior_sketch_prompt(con, "Land", "activity")
  stored <- store_sketch_answer(con, prompt, sketch_result(list(
    steps = list(), missing = list(),
    nodes = list(list(key = "a", kind = "initial", label = "", performer = ""), list(key = "b", kind = "action", label = "Land", performer = prompt$candidates$elements$n[1]), list(key = "c", kind = "final", label = "", performer = "")),
    edges = list(list(from = "a", to = "b", has_guard = "no"), list(from = "b", to = "c", has_guard = "yes"), list(from = "b", to = "zzz", has_guard = "no")))), "Land sketch")
  nodes <- activity_nodes(con, stored$id)
  expect_identical(nrow(nodes), 3L)
  expect_true(all(nodes$review_state == "unreviewed"))
  edges <- activity_edges(con, stored$id)
  expect_identical(nrow(edges), 2L)
  expect_identical(sum(edges$guard %in% "[guard]"), 1L)
})

test_that("behaviour questions become proposals on the state machine, free of numbers", {
  con <- local_built_sample()
  prompt <- behavior_analysis_prompt(con, "SM-001")
  expect_match(prompt$message, "link_lost × Land", fixed = TRUE)
  ids <- store_analysis_answer(con, prompt, sketch_result(list(questions = list(list(title = "What if the link drops for 30 s?", body = "", confidence = "medium")))))
  row <- DBI::dbGetQuery(con, "SELECT target_kind, target_id, title FROM ai_proposal WHERE id = ?", params = list(ids[[1]]))
  expect_identical(c(row$target_kind, row$target_id), c("state_machine", "SM-001"))
  expect_match(row$title, "[value]", fixed = TRUE)
})
