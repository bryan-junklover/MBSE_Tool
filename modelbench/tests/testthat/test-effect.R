test_that("a handled loss stops the chain where it's handled", {
  con <- local_built_sample()
  chain <- effect_chain(con, "FM-0001")
  expect_identical(nrow(chain), 1L)
  expect_identical(chain$status, "handled")
  expect_match(chain$text, "Flight Controller loses NavSolution from Navigation Sensors. Handled (FM-0001): Return to home", fixed = TRUE)
})

test_that("losing power fails the receiver, its parts and the software that runs on them", {
  con <- local_built_sample()
  chain <- effect_chain(con, "FM-0003")
  expect_identical(chain$status[1], "open")
  expect_identical(chain$depth[1], 0L)
  expect_identical(chain$element[chain$status == "propagates"], c("C-004", "C-008", "C-009"))
  expect_match(chain$text[chain$element %in% "C-009"], "Autopilot Software stops working, because it runs on FC Hardware.", fixed = TRUE)
  expect_match(chain$text[chain$element %in% "C-008"], "FC Hardware stops working, as part of Flight Controller.", fixed = TRUE)
  # Losing data with nothing recorded is flagged, and the chain stops there.
  propulsion <- chain[chain$element %in% "C-006", , drop = FALSE]
  expect_identical(propulsion$status, "unrecorded")
  expect_match(propulsion$text, "Nothing records what Propulsion does without it.", fixed = TRUE)
  expect_identical(effect_summary(chain)$stopped, c("C-004", "C-008", "C-009"))
})

test_that("a failed part breaks everything it sends, once each, and stops at handled losses", {
  con <- local_built_sample()
  chain <- effect_chain(con, "FM-0004")
  stopped <- effect_summary(chain)$stopped
  expect_setequal(stopped, c("C-003", "C-004", "C-008", "C-009", "C-006", "C-005", "C-007"))
  expect_false(anyDuplicated(stopped) > 0L)
  expect_true(any(chain$status == "reaches" & chain$element == "EXT-2"))
  # The radio link loss reached through Comms Link is handled by FM-0002.
  expect_true(any(chain$status == "handled" & chain$failure_id %in% "FM-0002"))
  # Every row but the first follows from an earlier one.
  expect_true(all(is.na(chain$parent) | chain$parent < chain$step))
})

test_that("recording a response changes the chain on the next look", {
  con <- local_built_sample()
  update_failure_mode(con, "FM-0003", "response_note", "A hold-up capacitor rides through brown-outs")
  update_failure_mode(con, "FM-0003", "response_state", "accepted")
  chain <- effect_chain(con, "FM-0003")
  expect_identical(chain$status, "accepted")
  expect_match(chain$text, "Accepted (FM-0003): A hold-up capacitor rides through brown-outs.", fixed = TRUE)
  expect_false("C-004" %in% effect_summary(effect_chain(con, "FM-0004"))$stopped)
})

test_that("a chain starts sensibly from every kind of subject", {
  con <- local_built_sample()
  state <- add_failure_mode(con, "sm_state|S-HLD", "other")$id
  expect_match(effect_chain(con, state)$text, "stays inside its state machine", fixed = TRUE)
  degraded <- add_failure_mode(con, "element|C-001", "degraded")$id
  expect_match(effect_chain(con, degraded)$text, "Navigation Sensors is degraded; the model can't say how far that reaches.", fixed = TRUE)
  loose <- add_failure_mode(con, "port|P-0019", "lost")$id
  expect_match(effect_chain(con, loose)$text[1], "has no connector", fixed = TRUE)
  expect_error(effect_chain(con, "FM-0999"), "no failure mode")
  expect_match(effect_chain_markdown(effect_chain(con, "FM-0003")), "    - → Flight Controller stops working.", fixed = TRUE)
})
