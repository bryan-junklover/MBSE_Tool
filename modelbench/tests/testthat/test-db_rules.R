# One negative test per database-enforced rule in SCHEMA §5.

req_sql <- function(status = "committed", text = "The UAV shall fly.", gate_pass = 1,
                    review_state = "reviewed", coach_state = "uncoached", origin = "human", note = "NULL") {
  sprintf("INSERT INTO requirement (id, level, text, status, gate_pass, review_state, coach_state, origin, review_note)
           VALUES ('R-T', 'system', '%s', '%s', %s, '%s', '%s', '%s', %s)",
          text, status, gate_pass, review_state, coach_state, origin, note)
}

test_that("a committed requirement passed the gate, has no placeholder, and was reviewed", {
  con <- local_project()
  expect_no_error(DBI::dbExecute(con, req_sql()))
  DBI::dbExecute(con, "DELETE FROM requirement")
  expect_db_refuses(con, req_sql(gate_pass = 0))
  expect_db_refuses(con, req_sql(text = "The UAV shall fly [value] km."))
  expect_db_refuses(con, req_sql(review_state = "unreviewed"))
  expect_db_refuses(con, req_sql(coach_state = "none"))
  expect_no_error(DBI::dbExecute(con, req_sql(status = "draft", text = "The UAV shall fly [value] km.", gate_pass = 0)))
})

test_that("an AI-originated row can't be reviewed without a note, on every tiered table", {
  con <- local_project()
  DBI::dbExecute(con, "INSERT INTO element (id, kind, name) VALUES ('C-1', 'component', 'a')")
  DBI::dbExecute(con, "INSERT INTO interface (id, name, flow_type) VALUES ('IF-1', 'i', 'data')")
  DBI::dbExecute(con, "INSERT INTO port (id, element_id, name, direction) VALUES ('P-1', 'C-1', 'x', 'out'), ('P-2', 'C-1', 'y', 'in')")
  DBI::dbExecute(con, "INSERT INTO scenario (id, name) VALUES ('SC-1', 's')")
  DBI::dbExecute(con, "INSERT INTO state_machine (id, owner_element_id, name) VALUES ('SM-1', 'C-1', 'm')")
  DBI::dbExecute(con, "INSERT INTO sm_state (id, sm_id, name) VALUES ('S-1', 'SM-1', 'st')")
  DBI::dbExecute(con, "INSERT INTO sm_event (id, sm_id, name) VALUES ('E-1', 'SM-1', 'ev')")
  ai <- "'ai', 'reviewed', '  '"
  statements <- c(
    sprintf("INSERT INTO element (id, kind, name, origin, review_state, review_note) VALUES ('C-2', 'component', 'b', %s)", ai),
    sprintf("INSERT INTO interface (id, name, flow_type, origin, review_state, review_note) VALUES ('IF-2', 'j', 'data', %s)", ai),
    sprintf("INSERT INTO connector (id, source_port_id, target_port_id, origin, review_state, review_note) VALUES ('K-1', 'P-1', 'P-2', %s)", ai),
    req_sql(status = "draft", origin = "ai", note = "'  '"),
    sprintf("INSERT INTO scenario (id, name, origin, review_state, review_note) VALUES ('SC-2', 't', %s)", ai),
    sprintf("INSERT INTO seq_step (id, scenario_id, seq, step_kind, label, origin, review_state, review_note) VALUES ('ST-1', 'SC-1', 1, 'note', 'n', %s)", ai),
    sprintf("INSERT INTO act_node (id, scenario_id, node_kind, origin, review_state, review_note) VALUES ('N-1', 'SC-1', 'action', %s)", ai),
    sprintf("INSERT INTO sm_state (id, sm_id, name, origin, review_state, review_note) VALUES ('S-2', 'SM-1', 'u', %s)", ai),
    sprintf("INSERT INTO sm_transition (id, sm_id, from_state_id, to_state_id, event_id, origin, review_state, review_note) VALUES ('T-1', 'SM-1', 'S-1', 'S-1', 'E-1', %s)", ai)
  )
  for (sql in statements) expect_db_refuses(con, sql)
})

test_that("a link from AI candidates needs a rationale and a proposal", {
  con <- local_project()
  DBI::dbExecute(con, "INSERT INTO ai_proposal (id, task, proposal_kind, title) VALUES ('PR-1', 'trace_coach', 'candidate_link', 'Which?')")
  base <- "INSERT INTO link (id, link_type, source_kind, source_id, target_kind, target_id, origin, rationale, ai_proposal_id)
           VALUES ('L-1', 'satisfied_by', 'requirement', 'a', 'element', 'b', 'ai_candidate', %s, %s)"
  expect_db_refuses(con, sprintf(base, "NULL", "'PR-1'"))
  expect_db_refuses(con, sprintf(base, "'   '", "'PR-1'"))
  expect_db_refuses(con, sprintf(base, "'why'", "NULL"))
  expect_no_error(DBI::dbExecute(con, sprintf(base, "'why'", "'PR-1'")))
})

test_that("keeping or dismissing a proposal needs a note or reason", {
  con <- local_project()
  base <- "INSERT INTO ai_proposal (id, task, proposal_kind, title, status, engineer_note, dismiss_reason) VALUES ('PR-1', 't', 'question', 'q?', %s)"
  expect_db_refuses(con, sprintf(base, "'kept', NULL, NULL"))
  expect_db_refuses(con, sprintf(base, "'kept', ' ', NULL"))
  expect_db_refuses(con, sprintf(base, "'dismissed', NULL, NULL"))
  expect_db_refuses(con, sprintf(base, "'dismissed', NULL, ' '"))
})

test_that("an ignore needs a rationale", {
  con <- local_project()
  DBI::dbExecute(con, "INSERT INTO element (id, kind, name) VALUES ('C-1', 'component', 'a')")
  DBI::dbExecute(con, "INSERT INTO state_machine (id, owner_element_id, name) VALUES ('SM-1', 'C-1', 'm')")
  DBI::dbExecute(con, "INSERT INTO sm_state (id, sm_id, name) VALUES ('S-1', 'SM-1', 'st')")
  DBI::dbExecute(con, "INSERT INTO sm_event (id, sm_id, name) VALUES ('E-1', 'SM-1', 'ev')")
  expect_db_refuses(con, "INSERT INTO sm_ignore (state_id, event_id, rationale) VALUES ('S-1', 'E-1', '  ')")
  expect_db_refuses(con, "INSERT INTO sm_ignore (state_id, event_id, rationale) VALUES ('S-1', 'E-1', NULL)")
})

test_that("property values: never set by the AI, and state matches content", {
  con <- local_project()
  DBI::dbExecute(con, "INSERT INTO property_def (id, name, label, value_type) VALUES ('PD-1', 'mass', 'Mass', 'number')")
  base <- "INSERT INTO property_value (id, owner_kind, owner_id, property_def_id, value_state, num_value, set_by) VALUES ('V-1', 'element', 'C-1', 'PD-1', %s)"
  expect_db_refuses(con, sprintf(base, "'set', 1, 'ai'"))
  expect_db_refuses(con, sprintf(base, "'set', NULL, 'human'"))
  expect_db_refuses(con, sprintf(base, "'placeholder', 1, 'human'"))
  expect_db_refuses(con, sprintf(base, "'computed', 1, 'human'"))
})

test_that("an interface field's unit must be in the registry", {
  con <- local_project()
  DBI::dbExecute(con, "INSERT INTO interface (id, name, flow_type) VALUES ('IF-1', 'i', 'data')")
  expect_db_refuses(con, "INSERT INTO interface_field (id, interface_id, name, value_type, unit) VALUES ('FLD-1', 'IF-1', 'f', 'float32', 'furlong')",
                    "FOREIGN KEY")
})
