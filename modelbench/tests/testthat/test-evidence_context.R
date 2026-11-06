current_or_stale <- function(con) {
  stale <- requirement_evidence(con, "SYS-001")$stale
  if (is.na(stale)) "current" else stale
}

test_that("evidence stays current through edits that change nothing it depends on", {
  con <- local_built_sample()
  write_verification_record(con, "SYS-001", "TC-007", "pass", "engineer", evidence_ref = "FT-2")
  expect_identical(current_or_stale(con), "current")

  edit <- write_property_value(con, "V-0003", list(num_value = 1.40))
  expect_match(current_or_stale(con), "Mass was 1.36 kg, now 1.4 kg", fixed = TRUE)
  revert_change_set(con, edit$change_set, "engineer", "Undo")
  expect_identical(current_or_stale(con), "current")

  save_requirement_details(con, "SYS-001", list(rationale = "Endurance sizes the battery", short_title = "Endurance"))
  expect_identical(current_or_stale(con), "current")
  commit_requirement(con, read_entity(con, "requirement", "SYS-001")$text, "SYS-001")
  expect_identical(current_or_stale(con), "current")
  expect_identical(nrow(baseline_blockers(con)[baseline_blockers(con)$group == "stale", ]), 0L)

  save_requirement_details(con, "SYS-001", list(verification_method = "analysis"))
  expect_match(current_or_stale(con), "requirement SYS-001 revised", fixed = TRUE)
})

test_that("evidence recorded before content hashes judges by the content of its revisions", {
  con <- local_built_sample()
  # VR-0001 was recorded at BL-03. Make it look like an older record, with revision numbers only.
  stored <- jsonlite::fromJSON(DBI::dbGetQuery(con, "SELECT context_json FROM verification_record WHERE id = 'VR-0001'")$context_json, simplifyVector = FALSE)
  expect_false(is.null(stored[[1]]$h))
  old <- canonical_json(lapply(stored, function(entry) entry[setdiff(names(entry), "h")]))
  DBI::dbExecute(con, "UPDATE verification_record SET context_json = ?, context_hash = ? WHERE id = 'VR-0001'", params = list(old, sha256(old)))
  expect_match(current_or_stale(con), "Battery capacity was 5000 mAh, now 4200 mAh", fixed = TRUE)
  rollback_to_baseline(con, "BL-03")
  expect_identical(current_or_stale(con), "current")
})
