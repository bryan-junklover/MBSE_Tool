test_that("Health renders the sample's numbers and fix links", {
  state <- local_sample_state()
  shiny::testServer(mod_health_server, args = list(state = state), {
    health <- session$getReturned()$health()
    html <- html_of(render_health(health, "go"))
    expect_match(html, "8 blockers", fixed = TRUE)
    expect_match(html, "3.31 / 3.60 kg", fixed = TRUE)
    expect_match(html, "412 / 450 W", fixed = TRUE)
    expect_match(html, "31 / 35 min · FAIL", fixed = TRUE)
    expect_match(html, "TC-007", fixed = TRUE)
    expect_match(html, "Battery capacity was 5000 mAh, now 4200 mAh", fixed = TRUE)
    expect_match(html, "9 / 9", fixed = TRUE)
    expect_match(html, "Power System.return_reserve · [value] mAh", fixed = TRUE)
  })
})

test_that("each blocker link goes where it can be fixed", {
  state <- local_sample_state()
  shiny::testServer(mod_health_server, args = list(state = state), {
    session$setInputs(go = list(place = "element", id = "C-003"))
    expect_identical(state$selected_entity()$ids, "C-003")
    expect_identical(state$nav()$workspace, "Model")
    session$setInputs(go = list(place = "interface", id = "IF-002"))
    expect_identical(state$selected_interface(), "IF-002")
    expect_identical(state$nav()$tab, "Interfaces")
    session$setInputs(go = list(place = "spec"))
    expect_identical(state$nav()$tab, "Specification")
  })
  expect_identical(blocker_target("property_value", "V-0014", "element", "C-003"), list(place = "element", id = "C-003"))
  expect_identical(blocker_target("interface_field", "FLD-0005", "interface", "IF-002"), list(place = "interface", id = "IF-002"))
  expect_identical(blocker_target("sm_transition", "T-09", "state_machine", "SM-001")$place, "behavior")
})

test_that("with no blockers the banner reads Ready to baseline", {
  con <- local({
    path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = parent.frame())
    init_project_db(path)
  })
  on.exit(DBI::dbDisconnect(con))
  expect_match(html_of(render_health(model_health(con), "go")), "Ready to baseline", fixed = TRUE)
})
