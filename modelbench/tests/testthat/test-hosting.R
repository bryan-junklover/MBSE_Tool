test_that("software records the hardware it runs on, one host at a time", {
  con <- local_sample()
  expect_identical(software_host(con, "C-009")$host_id, "C-008")
  expect_identical(hosted_software(con, "C-008")$id, "C-009")

  host_software(con, "C-002", "EXT-1")
  expect_identical(software_host(con, "C-002")$host_id, "EXT-1")
  host_software(con, "C-002", "C-008")
  expect_identical(software_host(con, "C-002")$host_id, "C-008")
  expect_setequal(hosted_software(con, "C-008")$id, c("C-002", "C-009"))

  host_software(con, "C-002", "")
  expect_identical(nrow(software_host(con, "C-002")), 0L)
})

test_that("only software runs on things, and only on hardware", {
  con <- local_sample()
  expect_error(host_software(con, "C-008", "C-003"), "isn't a software component")
  expect_error(host_software(con, "F1", "C-008"), "isn't a software component")
  expect_error(host_software(con, "C-002", "C-009"), "isn't hardware")
  expect_error(host_software(con, "C-002", "F1"), "isn't hardware")
  expect_error(host_software(con, "C-002", "C-002"), "itself")
  expect_error(host_software(con, "C-002", "C-999"), "isn't in the model")
  expect_false("C-002" %in% host_choices(con, "C-002")$id)
  expect_false("C-009" %in% host_choices(con, "C-002")$id)
  expect_true(all(c("C-004", "C-008", "EXT-1") %in% host_choices(con, "C-002")$id))
})

test_that("unhosted software is a gap, and a stale host link is a finding", {
  con <- local_sample()
  expect_identical(DBI::dbGetQuery(con, "SELECT id FROM v_unhosted_software")$id, "C-002")
  expect_identical(model_health(con)$architecture$unhosted, 1L)
  view <- structure_view(con)
  expect_true(view$nodes$gap[view$nodes$id == "C-002"])
  expect_match(view$nodes$gap_note[view$nodes$id == "C-002"], "Nothing recorded for it to run on")

  expect_identical(nrow(hosting_findings(con)), 0L)
  write_element(con, "C-009", list(domain = "hw"), "engineer")
  findings <- element_gate_findings(con)
  expect_match(findings$problem[findings$kind == "link"], "Autopilot Software isn't a software component")
})

test_that("hosting can be undone, and goes with the element", {
  con <- local_sample()
  written <- host_software(con, "C-002", "C-008")
  revert_change_set(con, written$change_set, "engineer", "Undo")
  expect_identical(nrow(software_host(con, "C-002")), 0L)
  delete_entity(con, "element", "C-008")
  expect_identical(nrow(hosted_software(con, "C-008")), 0L)
})
