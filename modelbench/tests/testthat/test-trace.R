test_that("TC-007 is stale after the battery change, and says what moved", {
  con <- local_built_sample()
  stale <- stale_evidence(con)
  expect_identical(stale$test_case_id, "TC-007")
  expect_match(stale$text, "Battery capacity was 5000 mAh, now 4200 mAh", fixed = TRUE)
  expect_match(stale$text, "Mass was 1.47 kg, now 1.36 kg", fixed = TRUE)
  evidence <- requirement_evidence(con, "SYS-001")
  expect_identical(evidence$result, "pass")
  expect_match(evidence$stale, "4200 mAh", fixed = TRUE)
})

test_that("re-recording makes evidence current, and changes reach it", {
  con <- local_built_sample()
  write_verification_record(con, "SYS-001", "TC-007", "fail", "engineer", note = "31 min measured")
  expect_identical(nrow(stale_evidence(con)), 0L)
  write_property_value(con, "V-0018", list(num_value = 0.85))
  expect_match(stale_evidence(con)$text, "Usable fraction was 0.9, now 0.85", fixed = TRUE)
  # A newly linked element joins the context.
  write_verification_record(con, "SYS-001", "TC-007", "fail", "engineer")
  link_requirement(con, "SYS-001", "C-004")
  expect_match(stale_evidence(con)$text, "added", fixed = TRUE)
})

test_that("a change elsewhere doesn't touch the evidence", {
  con <- local_built_sample()
  write_verification_record(con, "SYS-001", "TC-007", "fail", "engineer")
  write_property_value(con, "V-0001", list(num_value = 0.08))
  write_element(con, "C-002", list(description = "Laptop app"))
  # V-0001 is a mass, which the power roll-up doesn't read; C-002 isn't linked.
  expect_identical(nrow(stale_evidence(con)), 0L)
})

test_that("state machine content joins the context of an assertion's requirement", {
  con <- local_built_sample()
  ids <- vapply(verification_context(con, "SYS-012"), function(entry) entry$id, "")
  expect_true(all(c("SM-001", "S-RTH", "T-05", "E-04", "VAR-1") %in% ids))
  write_test_case(con, "TC-X", list(name = "Sim", method = "simulation"))
  write_verification_record(con, "SYS-012", "TC-X", "pass", "engineer")
  expect_identical(nrow(stale_evidence(con)[stale_evidence(con)$requirement_id == "SYS-012", ]), 0L)
  write_sm_transition(con, "T-05", list(hold_s = 4))
  expect_match(stale_evidence(con)[stale_evidence(con)$requirement_id == "SYS-012", ]$text, "T-05", fixed = TRUE)
})

test_that("Health counts stale evidence: 8 blockers on the sample", {
  health <- model_health(local_built_sample())
  expect_identical(nrow(health$blockers$placeholders) + nrow(health$blockers$unreviewed) + nrow(health$blockers$stale), 8L)
  expect_identical(health$functional$flows, 9L)
  expect_identical(health$functional$flows_realised, 9L)
})

test_that("the realisation check passes on the sample's functional flows", {
  con <- local_built_sample()
  check <- realisation_check(con)
  expect_identical(nrow(check), 9L)
  expect_true(all(check$realised))
  expect_identical(check$carried_by[check$id == "K-0019"], "K-0007")
})

test_that("the realisation check fails when it should", {
  con <- local_built_sample()
  # The named connector doesn't join the allocated components.
  realize_flow(con, "K-0019", "K-0001")
  row <- realisation_check(con)[realisation_check(con)$id == "K-0019", ]
  expect_false(row$realised)
  expect_match(row$problem, "doesn't join")
  # With no named connector, it looks for one, and finds K-0007.
  realize_flow(con, "K-0019", "")
  row <- realisation_check(con)[realisation_check(con)$id == "K-0019", ]
  expect_true(row$realised)
  expect_identical(row$carried_by, "K-0007")
  # A flow from a function nobody has allocated can't be realised.
  write_port(con, "P-F8", list(element_id = "F8", name = "o", direction = "out", interface_id = "IF-012"))
  write_port(con, "P-F2", list(element_id = "F2", name = "f8", direction = "in", interface_id = "IF-012"))
  write_connector(con, "K-F", list(source_port_id = "P-F8", target_port_id = "P-F2"))
  row <- realisation_check(con)[realisation_check(con)$id == "K-F", ]
  expect_match(row$problem, "isn't allocated")
  # Allocated, but no connector joins the components.
  allocate_function(con, "F8", "C-007")
  write_port(con, "P-F6", list(element_id = "F7", name = "f8", direction = "in", interface_id = "IF-012"))
  write_connector(con, "K-G", list(source_port_id = "P-F8", target_port_id = "P-F6"))
  row <- realisation_check(con)[realisation_check(con)$id == "K-G", ]
  expect_match(row$problem, "no physical connector")
})

test_that("two functions in one component need no connector", {
  con <- local_built_sample()
  allocate_function(con, "F8", "C-004")
  write_port(con, "P-F8", list(element_id = "F8", name = "o", direction = "out", interface_id = "IF-012"))
  write_port(con, "P-F2", list(element_id = "F2", name = "f8", direction = "in", interface_id = "IF-012"))
  write_connector(con, "K-F", list(source_port_id = "P-F8", target_port_id = "P-F2"))
  expect_identical(realisation_check(con)$carried_by[realisation_check(con)$id == "K-F"], "same component")
})

test_that("flows view shows types and quantities, with the environment", {
  con <- local_sample()
  view <- flows_view(con)
  expect_true("EXT-2" %in% view$nodes$id)
  expect_false("EXT-1" %in% view$nodes$id)
  label <- function(id) view$edges$label[view$edges$id == id]
  expect_identical(label("K-0006"), "28 V · 380 W")
  expect_identical(label("K-0005"), "28 V · 8 W")
  expect_identical(label("K-0001"), "50 Hz")
  expect_identical(label("K-0011"), "thrust")
  expect_identical(view$edges$flow_type[view$edges$id == "K-0012"], "matter")
  expect_false(any(view$edges$stub))
  data_only <- flows_view(con, "data")
  expect_true(all(data_only$edges$flow_type == "data"))
  expect_false("K-0006" %in% data_only$edges$id)
  expect_identical(flows_view(con, quantities = FALSE)$edges$label[flows_view(con, quantities = FALSE)$edges$id == "K-0006"], "PowerBus28V")
})

test_that("functional view shows allocation and prefixes flows", {
  view <- functional_view(local_sample())
  expect_identical(nrow(view$nodes), 8L)
  expect_identical(view$nodes$allocation[view$nodes$id == "F8"], "→ unallocated")
  expect_true(view$nodes$unallocated[view$nodes$id == "F8"])
  expect_match(view$nodes$allocation[view$nodes$id == "F5"], "Comms Link", fixed = TRUE)
  expect_identical(view$edges$label[view$edges$id == "K-0013"], "D: position estimate")
  expect_identical(view$edges$label[view$edges$id == "K-0017"], "E: electrical")
})

test_that("the trace matrix counts sub-components, and includes drafts", {
  con <- local_sample()
  matrix <- trace_matrix(con)
  expect_identical(nrow(matrix$rows), 13L)
  expect_true("SYS-003" %in% matrix$rows$id)
  expect_identical(matrix$rows$id[matrix$rows$unallocated], "SYS-021")
  expect_identical(matrix$coverage$allocated, 5L)
  expect_identical(matrix$coverage$allocatable, 6L)
  expect_true("C-002" %in% matrix$columns$id[matrix$columns$empty])
  link_requirement(con, "SYS-021", "C-009", "Autopilot checks it")
  matrix <- trace_matrix(con)
  row <- matrix$links[matrix$links$source_id == "SYS-021", ]
  expect_identical(row$column, "C-004")
  expect_identical(row$target_id, "C-009")
  expect_identical(nrow(matrix$cells$direct[matrix$cells$direct$source_id == "SYS-021", ]), 0L)
})

test_that("candidates rank by shared rare words, skip linked ones, and use handles", {
  con <- local_sample()
  ranked <- rank_candidates(con, "The camera shall capture images", "requirement", "SYS-021", 3L)
  expect_identical(ranked$id[1], "C-007")
  expect_identical(ranked$n, c("C1", "C2", "C3"))
  linked <- rank_candidates(con, "The camera shall capture images", "requirement", "SYS-018")
  expect_false("C-007" %in% linked$id)
  expect_false("C-000" %in% linked$id)
  expect_lte(nrow(rank_candidates(con, "x", "requirement", "SYS-021", 2L)), 2L)
  expect_identical(nrow(rank_candidates(local_project(), "x", "requirement")), 0L)
})

test_that("functions rank the components their neighbours sit on higher", {
  con <- local_sample()
  ranked <- rank_candidates(con, "Control flight", "element", "F2")
  expect_false("C-009" %in% ranked$id)
  expect_true(ranked$score[match("C-001", ranked$id)] >= 1)
})

test_that("the link coach prompt sends handles, and dialog equals sent", {
  con <- local_sample()
  prompt <- link_coach_prompt(con, "requirement", "SYS-021")
  expect_false(grepl("C-00", prompt$message, fixed = TRUE))
  expect_match(prompt$message, "C1 ", fixed = TRUE)
  expect_match(prompt$candidates_json, "C-00", fixed = TRUE)
  set_task_route(con, "trace_coach", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  fake <- fake_client(list(list(items = list())))
  local_mocked_bindings(build_llm_client = fake$builder)
  run_ai(con, prompt)
  expect_identical(fake$log$messages[1], prompt$message)
  expect_identical(fake$log$systems[1], prompt$system)
  expect_identical(link_coach_prompt(con, "element", "F8")$task, "allocation_coach")
})

test_that("a link answer maps handles, names them, drops the unknown and scrubs numbers", {
  con <- local_sample()
  set_task_route(con, "trace_coach", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  prompt <- link_coach_prompt(con, "requirement", "SYS-021")
  c1 <- prompt$candidates[1, ]
  answer <- list(items = list(
    list(question = "Should C1 run the check every 5 s?", candidates = list(list(handle = "C1", why = "C1 holds it at 50 Hz"), list(handle = "C99", why = "invented"))),
    list(question = "What is missing?", candidates = list(list(handle = "C77", why = "no such")))
  ))
  local_mocked_bindings(build_llm_client = fake_client(list(answer))$builder)
  result <- run_link_coach(con, "requirement", "SYS-021")
  rows <- DBI::dbGetQuery(con, sprintf("SELECT * FROM ai_proposal WHERE id IN ('%s') ORDER BY id", paste(result$proposal_ids, collapse = "','")))
  expect_identical(rows$proposal_kind, c("candidate_link", "question"))
  expect_match(rows$title[1], sprintf("Should %s run the check every [value] s?", c1$name), fixed = TRUE)
  stored <- jsonlite::fromJSON(rows$candidates_json[1])
  expect_identical(stored$id, c1$id)
  expect_match(stored$why, sprintf("%s holds it at [value] Hz", c1$name), fixed = TRUE)
  expect_identical(rows$target_id[1], "SYS-021")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM audit_log WHERE action = 'ai_dropped'")$n, 2L)
})

test_that("a link chosen from candidates can't be saved without a note", {
  con <- local_sample()
  id <- insert_proposal(con, list(task = "allocation_coach", proposal_kind = "candidate_link", target_kind = "element", target_id = "F8", title = "Who notices?",
                                  payload_json = "{}", candidates_json = canonical_json(list(list(n = "C2", kind = "element", id = "C-004", name = "Flight Controller", why = "x")))))
  expect_error(keep_candidate(con, id, "C2", " "), class = "modelbench_refused")
  expect_error(keep_candidate(con, id, "C9", "why"), class = "modelbench_refused")
  written <- keep_candidate(con, id, "C2", "It runs the watchdog")
  row <- read_entity(con, "link", written$id)
  expect_identical(row$link_type, "allocated_to")
  expect_identical(row$origin, "ai_candidate")
  expect_identical(row$rationale, "It runs the watchdog")
  expect_error(keep_candidate(con, id, "C2", "again"), class = "modelbench_refused")
  expect_identical(count_rows(con, "v_unallocated_functions"), 0L)
})

test_that("hand links may leave the rationale empty, and test cases link themselves", {
  con <- local_built_sample()
  written <- link_requirement(con, "SYS-021", "C-004", " ")
  expect_true(is.na(read_entity(con, "link", written$id)$rationale))
  expect_identical(count_rows(con, "v_unallocated_requirements"), 0L)
  case <- add_test_case(con, "SYS-021", "Fence test", "test")
  expect_identical(requirement_evidence(con, "SYS-021")$id, case$id)
  expect_true(is.na(requirement_evidence(con, "SYS-021")$result))
  expect_error(add_test_case(con, "SYS-021", " ", "test"), class = "modelbench_refused")
  write_verification_record(con, "SYS-021", case$id, "pass", "engineer", evidence_ref = "R1")
  expect_identical(requirement_evidence(con, "SYS-021")$result, "pass")
})
