test_that("the table filters, and selecting shows the inspector's findings", {
  state <- local_sample_state()
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(search = "", type = "all", low_first = TRUE)
    expect_identical(nrow(session$getReturned()$filtered()), 13L)
    expect_identical(session$getReturned()$filtered()$id[1], "SYS-003")
    session$setInputs(search = "geofence")
    expect_identical(session$getReturned()$filtered()$id, "SYS-021")
    session$setInputs(search = "", type = "nfr")
    expect_identical(session$getReturned()$filtered()$id, "SYS-003")

    session$setInputs(select = "SYS-007")
    expect_identical(selected(), "SYS-007")
    expect_identical(current()$id, "SYS-007")
  })
})

test_that("details save on change, through the write path", {
  state <- local_sample_state()
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(select = "SYS-001")
    start <- read_entity(con(), "requirement", "SYS-001")$rev
    session$setInputs(d_verify = "analysis")
    expect_identical(read_entity(con(), "requirement", "SYS-001")$verification_method, "analysis")
    session$setInputs(d_rationale = "Survey needs it")
    session$elapse(450)
    session$flushReact()
    row <- read_entity(con(), "requirement", "SYS-001")
    expect_identical(row$rationale, "Survey needs it")
    expect_identical(row$rev, start + 2L)
    expect_gt(DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM revision WHERE entity_id = 'SYS-001'")$n, 2L)
  })
})

test_that("a parent cycle from the inspector is refused and changes nothing", {
  state <- local_sample_state()
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(select = "STK-001")
    session$setInputs(d_parent = "SYS-001")
    expect_true(is.na(read_entity(con(), "requirement", "STK-001")$parent_id))
  })
})

test_that("import sends passing rows to drafts and works the queue one at a time", {
  state <- local_sample_state()
  file <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("When the bus drops, the power system shall report status within 5 s.",
               "The drone should be quick.", "The trap shall be safe."), file)
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(import = list(name = "reqs.txt", datapath = file))
    session$flushReact()
    queue <- session$getReturned()$pending()
    expect_identical(nrow(queue), 2L)
    expect_identical(DBI::dbGetQuery(con(), "SELECT status FROM requirement WHERE id = 'SYS-032'")$status, "draft")

    session$setInputs(triage_text = "Still should be vague.", triage_fix = 1)
    expect_identical(nrow(session$getReturned()$pending()), 2L)
    session$setInputs(triage_text = "When armed, the power system shall hold the bus at 28 V.", triage_fix = 2)
    session$flushReact()
    expect_identical(nrow(session$getReturned()$pending()), 1L)
    session$setInputs(triage_skip = 1)
    session$flushReact()
    expect_identical(nrow(session$getReturned()$pending()), 0L)
  })
})

test_that("a bad import file is refused without a crash", {
  state <- local_sample_state()
  file <- withr::local_tempfile(fileext = ".pdf")
  writeLines("x", file)
  shiny::testServer(mod_spec_server, args = list(state = state), {
    before <- DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n
    session$setInputs(import = list(name = "reqs.pdf", datapath = file))
    expect_identical(DBI::dbGetQuery(con(), "SELECT count(*) AS n FROM requirement")$n, before)
  })
})

test_that("'Rewrite it in Write' opens that requirement on the Draft tab", {
  state <- local_sample_state()
  shiny::testServer(mod_spec_server, args = list(state = state), {
    session$setInputs(select = "SYS-007", rewrite = 1)
    expect_identical(shiny::isolate(state$open_in_write()), "SYS-007")
    expect_identical(shiny::isolate(state$nav())$tab, "Draft")
  })
})
