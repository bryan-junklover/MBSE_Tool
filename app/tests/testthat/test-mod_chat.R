options(modelbench.no_keyring = TRUE)

test_that("tool calls show as chips above the answer", {
  html <- chat_chips(list(list(name = "rollup", args = "mass"), list(name = "find", args = "<b>")))
  expect_match(html, "mb-tool-chip\">rollup(mass)", fixed = TRUE)
  expect_match(html, "&lt;b&gt;", fixed = TRUE)
  expect_identical(chat_chips(list()), "")
})

test_that("Highlight selects the answer's elements, and Attach adds to the proposal", {
  state <- local_sample_state()
  shiny::testServer(mod_chat_server, args = list(state = state), {
    session$getReturned()$last(list(ok = TRUE, text = "The Power System is fine.", elements = "C-003", provider_id = "lmstudio", model = "mock-model"))
    session$setInputs(highlight = 1)
    expect_identical(state$selected_entity()$ids, "C-003")
    expect_identical(state$nav()$tab, "Structure")
    session$setInputs(proposal = "PR-0002", attach = 1)
    expect_match(DBI::dbGetQuery(con(), "SELECT body FROM ai_proposal WHERE id = 'PR-0002'")$body, "From the chat: The Power System is fine.", fixed = TRUE)
    expect_match(html_of(output$footer$html), "read-only", fixed = TRUE)
  })
})

test_that("the export buttons are on Specification and Interfaces", {
  expect_match(html_of(mod_spec_ui("spec")), "spec-export_reqif", fixed = TRUE)
  expect_match(html_of(mod_interfaces_ui("interfaces")), "interfaces-icd_html", fixed = TRUE)
})

test_that("the fonts are bundled, not fetched", {
  css <- readLines(file.path(ROOT, "app", "www", "fonts.css"))
  files <- regmatches(css, regexpr("fonts/[A-Za-z-]+\\.woff2", css))
  expect_length(files, 5L)
  for (file in files) expect_true(file.exists(file.path(ROOT, "app", "www", file)), label = file)
  page <- htmltools::renderTags(app_ui())
  html <- paste(page$head, page$html)
  expect_match(html, "fonts.css?v=", fixed = TRUE)
  expect_false(grepl("fonts.googleapis", html, fixed = TRUE))
})
