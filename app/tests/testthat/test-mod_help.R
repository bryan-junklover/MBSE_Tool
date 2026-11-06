withr::local_options(modelbench.docs_dir = file.path(ROOT, "docs"), .local_envir = teardown_env())

test_that("both help pages are found and render with a contents column", {
  for (file in c("USER_GUIDE.md", "HOW_TO.md")) {
    path <- help_path(file)
    expect_false(is.na(path), info = file)
    page <- render_help_markdown(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), "x")
    expect_gt(nrow(page$contents), 5L)
    expect_false(anyDuplicated(page$contents$id) > 0L)
    for (id in page$contents$id) expect_match(page$html, sprintf("<h2 id=\"%s\">", id), fixed = TRUE)
  }
  html <- html_of(mod_help_ui("help_guide", "USER_GUIDE.md"))
  expect_match(html, "ModelBench user guide", fixed = TRUE)
  expect_match(html, "On this page", fixed = TRUE)
  expect_match(html, "scrollIntoView", fixed = TRUE)
  expect_match(html_of(mod_help_ui("help_howto", "HOW_TO.md")), "<table>", fixed = TRUE)
  expect_match(html_of(mod_help_ui("x", "NOPE.md")), "isn't where the app expects it", fixed = TRUE)
})

test_that("headings become ids the contents can jump to", {
  page <- render_help_markdown("# Title\n\n## 1. Starting the app\n\nText.\n\n## Write: **requirements**\n", "p")
  expect_identical(page$contents$id, c("p-1-starting-the-app", "p-write-requirements"))
  expect_identical(page$contents$title, c("1. Starting the app", "Write: requirements"))
})

# Keeps the guide in step with the app: every workspace and screen has its place in it.
test_that("the user guide covers every workspace and screen", {
  guide <- paste(readLines(help_path("USER_GUIDE.md"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  for (workspace in setdiff(WORKSPACES, "Help")) expect_match(guide, workspace, fixed = TRUE, info = workspace)
  for (tab in unlist(WORKSPACE_TABS[setdiff(names(WORKSPACE_TABS), "Help")])) {
    expect_true(grepl(paste0("### ", tab), guide, fixed = TRUE) || grepl(paste0("**", tab, "**"), guide, fixed = TRUE), info = tab)
  }
})
