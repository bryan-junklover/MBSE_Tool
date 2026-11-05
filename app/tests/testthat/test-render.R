test_that("the sentence renderer marks each part and escapes text", {
  html <- html_of(render_parsed_sentence(parse_sentence("When <b>armed</b>, the power system shall report status within 5 s.")))
  expect_match(html, "part-condition", fixed = TRUE)
  expect_match(html, "part-actor", fixed = TRUE)
  expect_match(html, "part-constraint", fixed = TRUE)
  expect_false(grepl("<b>armed", html, fixed = TRUE))
  expect_match(html, "&lt;b&gt;", fixed = TRUE)
})

test_that("the gate panel states score, result and hard rules first", {
  html <- html_of(render_gate(check_requirement("The system should be reliable.")))
  expect_match(html, "score 0 · FAIL", fixed = TRUE)
  expect_match(html, "banned wording", fixed = TRUE)
  expect_lt(regexpr("banned wording", html)[1], regexpr("mb-ledger", html)[1])
  passing <- html_of(render_gate(check_requirement("When armed, the door shall close within 2 s.")))
  expect_match(passing, "score 100 · PASS", fixed = TRUE)
  expect_match(passing, "+40", fixed = TRUE)
})

test_that("the terms panel asks about unknown terms and offers what fits the role", {
  terms <- data.frame(term = c("power system", "gimbal", "flight time"), role = c("object", "actor", "object"),
                      resolution = c("model", "unresolved", "unresolved"), element_id = c("C-003", NA, NA),
                      element_name = c("Power System", NA, NA), stereotype = c("HW", NA, NA),
                      source = c("element", NA, NA), position = 1:3, stringsAsFactors = FALSE)
  html <- html_of(render_terms(terms, "term_action"))
  expect_match(html, "Power System «HW» · in the model", fixed = TRUE)
  expect_match(html, "Add component…", fixed = TRUE)
  expect_match(html, "Add interface field…", fixed = TRUE)
  expect_match(html, "Not a model term", fixed = TRUE)
})

test_that("the status strip shows the sample's counts", {
  expect_match(html_of(render_status_strip(list(unreviewed = 3L, placeholders = 4L), "nav")),
               "3 unreviewed AI drafts · 4 placeholders", fixed = TRUE)
})

test_that("no button offers Accept or Apply wording", {
  sources <- unlist(lapply(list.files(file.path(ROOT, "app", "R"), full.names = TRUE), readLines))
  expect_false(any(grepl('"(Accept|Apply|Accept all)"', sources)))
})

test_that("the shell writes nothing to the database directly", {
  sources <- unlist(lapply(list.files(file.path(ROOT, "app", "R"), full.names = TRUE), readLines))
  expect_false(any(grepl("dbExecute|dbSendStatement|dbWriteTable|dbAppendTable", sources)))
})

test_that("the UI builds, and the app opens on Write", {
  html <- as.character(app_ui())
  expect_match(html, "ModelBench", fixed = TRUE)
  expect_true(grepl('data-value="Write"', html, fixed = TRUE))
})

test_that("projects are created, listed and refused when they clash", {
  dir <- withr::local_tempdir()
  path <- create_project(dir, "My UAV!")
  expect_identical(basename(path), "my_uav.sqlite")
  expect_identical(list_projects(dir), "my_uav.sqlite")
  expect_error(create_project(dir, "my uav"), "already")
  expect_error(create_project(dir, "!!"), "needs a name")
  con <- open_default_project(dir)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(get_setting(con, "project_name"), "My UAV!")
})

test_that("the first run opens the sample", {
  dir <- withr::local_tempdir()
  con <- open_default_project(dir)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(get_setting(con, "project_name"), "Survey UAV")
})

test_that("the diagram widget keeps blank positions and single selections in shape", {
  widget <- mbdiagram(list(nodes = data.frame(id = "C-1", x = NA_real_), selected = as.list("C-1"), tool = "select"))
  parsed <- jsonlite::fromJSON(widget$x$json, simplifyVector = FALSE)
  expect_null(parsed$nodes[[1]]$x)
  expect_identical(parsed$selected, list("C-1"))
})

test_that("nothing outside the tests loads the test helpers, which switch off real AI", {
  scripts <- c(file.path(ROOT, "app", "app.R"), list.files(file.path(ROOT, "tools"), pattern = "[.]R$", full.names = TRUE))
  for (script in scripts) {
    loads <- grep("load_all", readLines(script), value = TRUE)
    expect_true(all(grepl("helpers = FALSE", loads)), info = script)
  }
})

test_that("select options always carry a label, even from unnamed choices", {
  html <- html_of(select_tag(c("alt", "opt"), "opt", "", blank = "None"))
  expect_match(html, ">None</option>", fixed = TRUE)
  expect_match(html, ">alt</option>", fixed = TRUE)
  expect_match(html, "selected>opt</option>", fixed = TRUE)
  named <- html_of(select_tag(c("Message" = "message", "self"), "message", ""))
  expect_match(named, ">Message</option>", fixed = TRUE)
  expect_match(named, ">self</option>", fixed = TRUE)
  expect_false(grepl("></option>", html_of(select_tag(STEP_KINDS, "note", "")), fixed = TRUE))
})
