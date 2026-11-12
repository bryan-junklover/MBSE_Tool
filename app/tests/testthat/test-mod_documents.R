test_that("the documents screen previews each viewpoint, from the working copy or a baseline", {
  state <- local_sample_state()
  shiny::testServer(mod_documents_server, args = list(state = state), {
    session$setInputs(source = "working")
    html <- as.character(output$preview$html)
    expect_match(html, "Survey UAV: Hardware view", fixed = TRUE)
    expect_match(html, "<table>", fixed = TRUE)
    expect_no_match(html, "{#if-", fixed = TRUE)
    expect_match(as.character(output$list$html), "For hardware engineers", fixed = TRUE)
    session$setInputs(select = "VP-software")
    expect_match(as.character(output$preview$html), "Signals", fixed = TRUE)
    session$setInputs(source = "BL-03")
    expect_match(as.character(output$preview$html), "baseline BL-03", fixed = TRUE)
  })
})

test_that("the preview drops Quarto's anchors and turns callouts into quotes", {
  md <- "## IF-001 · Power {#if-001}\n\n::: {.callout-warning}\nThis interface is an unreviewed AI draft.\n:::\n"
  out <- preview_markdown(md)
  expect_match(out, "## IF-001 · Power\n", fixed = TRUE)
  expect_match(out, "> This interface is an unreviewed AI draft.", fixed = TRUE)
})
