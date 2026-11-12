test_that("an assertion's description and requirement can be changed, with its link following (B5)", {
  con <- local_built_sample()
  update_assertion(con, "A-001", "description", "GNSS loss leads home")
  expect_identical(read_entity(con, "sm_assertion", "A-001")$description, "GNSS loss leads home")
  update_assertion(con, "A-001", "requirement_id", "SYS-014")
  expect_identical(read_entity(con, "sm_assertion", "A-001")$requirement_id, "SYS-014")
  expect_identical(DBI::dbGetQuery(con, "SELECT source_id FROM link WHERE link_type = 'verified_by' AND target_id = 'A-001'")$source_id, "SYS-014")
  expect_error(update_assertion(con, "A-001", "script_json", "{}"), "Delete it and add it again")
})

test_that("a failure handled by a transition on another event than the one detecting it is flagged (C11)", {
  con <- local_built_sample()
  link_failure(con, "FM-0002", "handled_by", "sm_transition|T-05")
  expect_true(any(grepl("FM-0002 is detected by link_lost but handled by a transition on gnss_lost", failure_findings(con)$text, fixed = TRUE)))
})

test_that("failure modes and hosting reach the impact graph (D6), and a budget over several says so (G8)", {
  con <- local_built_sample()
  edges <- impact_edges(con)
  # Cause to effect: what fails reaches its failure mode; a host reaches its software.
  expect_true(any(edges$from == "element:C-004" & edges$to == "failure_mode:FM-0003"))
  expect_false(any(edges$from == "failure_mode:FM-0003" & edges$to == "element:C-004"))
  expect_true(any(edges$from == "element:C-008" & edges$to == "element:C-009"))
  mass <- DBI::dbGetQuery(con, "SELECT id FROM property_def WHERE name = 'mass'")$id
  add_budget(con, mass, "C-006", "2", "kg")
  expect_true("Propulsion (all 4)" %in% budget_status(con)$scope_name)
})

test_that("saved views, analysis limits and connector quantities can be added (B12, B15)", {
  con <- local_built_sample()
  view <- add_saved_view(con, "Hardware only", c("hw"))
  expect_true(all(structure_view(con, view)$nodes$id %in% DBI::dbGetQuery(con, "SELECT id FROM element WHERE domain = 'hw'")$id))
  expect_error(add_saved_view(con, "Nothing"), "Choose a domain or a stereotype")
  expect_error(delete_saved_view(con, "VW-all"), "Built-in")
  delete_saved_view(con, view)
  expect_false(view %in% saved_views(con)$id)
  analysis <- registered_analyses()[1]
  expect_error(add_analysis_limit(con, analysis, "C-000", "30"), "already has a limit")
  add_analysis_limit(con, analysis, "C-003", "10")
  expect_true("C-003" %in% analysis_status(con)$scope)
  voltage <- DBI::dbGetQuery(con, "SELECT id FROM property_def WHERE name = 'bus_voltage'")$id
  expect_error(add_property_to_connector(con, "K-0006", voltage), "already has")
  power <- DBI::dbGetQuery(con, "SELECT id FROM property_def WHERE name = 'power'")$id
  add_property_to_connector(con, "K-0001", power)
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) FROM property_value WHERE owner_kind = 'connector' AND owner_id = 'K-0001'")[[1]], 1L)
})

test_that("a shared property stays unresolved (F7, by design), and coaching keeps the text (F5)", {
  con <- local_built_sample()
  expect_false("Mass" %in% model_terms(con)$term)
  before <- read_entity(con, "requirement", "SYS-001")
  expect_error(mark_requirement_coached(con, "SYS-001", "Some other words."), "different text")
  mark_requirement_coached(con, "SYS-001", before$text, 1L)
  after <- read_entity(con, "requirement", "SYS-001")
  expect_identical(c(after$coach_state, after$text), c("reviewed", before$text))
})

test_that("a step's kind fills in what it needs, fragments delete as a whole, and a sketch reviews at once (G3, G6, G9)", {
  con <- local_built_sample()
  sc <- write_scenario(con, fields = list(name = "Probe", origin = "ai", review_state = "unreviewed"))$id
  a <- add_lifeline(con, sc, "C-004")
  b <- add_lifeline(con, sc, "C-005")
  note <- write_seq_step(con, fields = list(scenario_id = sc, seq = 1L, step_kind = "note", label = "Start", origin = "ai", review_state = "unreviewed"))$id
  update_step_field(con, note, "step_kind", "message")
  step <- read_entity(con, "seq_step", note)
  expect_false(is.na(step$from_lifeline_id) || is.na(step$to_lifeline_id))
  open <- write_seq_step(con, fields = list(scenario_id = sc, seq = 2L, step_kind = "fragment_open", fragment_kind = "alt", label = "If"))$id
  write_seq_step(con, fields = list(scenario_id = sc, seq = 3L, step_kind = "fragment_else", label = "Else"))
  write_seq_step(con, fields = list(scenario_id = sc, seq = 4L, step_kind = "fragment_close", label = "End"))
  delete_seq_step(con, open)
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) FROM seq_step WHERE scenario_id = ?", params = list(sc))[[1]], 1L)
  review_scenario_all(con, sc, "Matches the ops plan")
  expect_identical(read_entity(con, "scenario", sc)$review_state, "reviewed")
  expect_identical(read_entity(con, "seq_step", note)$review_state, "reviewed")
  write_scenario(con, sc, list(has_activity = 1L))
  expect_identical(read_entity(con, "scenario", sc)$has_activity, 1L)
})
