# Handoff §7 / UX review 4.6: no screen scrolls sideways at 900 px (16 screens with Documents). Runs the real app in
# headless Chrome, so it needs shinytest2 and a Chrome that chromote can find.
# Set MODELBENCH_SKIP_BROWSER_TESTS=true to leave it out.
test_that("no screen scrolls sideways at 900 px", {
  skip_if(isTRUE(as.logical(Sys.getenv("MODELBENCH_SKIP_BROWSER_TESTS", "false"))), "browser tests turned off")
  skip_if_not_installed("shinytest2")
  skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome for chromote")
  # AppDriver skips itself "on CRAN" unless NOT_CRAN is set; this app never goes to CRAN.
  withr::local_envvar(NOT_CRAN = "true", MODELBENCH_WORKSPACE_DIR = withr::local_tempdir(),
                      ANTHROPIC_API_KEY = NA, OPENAI_API_KEY = NA, GOOGLE_API_KEY = NA, GEMINI_API_KEY = NA)

  app <- shinytest2::AppDriver$new(file.path(ROOT, "app"), name = "layout", width = 900, height = 800,
                                   load_timeout = 60000, timeout = 20000, view = FALSE)
  withr::defer(app$stop())
  # The startup restore of the last screen would otherwise land after the first click.
  app$wait_for_value(input = "nav_restored")

  screens <- c(list(list(workspace = "Project", tab = "Project")),
               unlist(lapply(names(WORKSPACE_TABS), function(workspace) {
                 lapply(WORKSPACE_TABS[[workspace]], function(tab) list(workspace = workspace, tab = tab))
               }), recursive = FALSE))
  expect_length(screens, 16L)
  overflow <- vapply(screens, function(screen) {
    # Clicks the workspace, then the tab, as a user would.
    click <- function(scope, label) {
      app$run_js(sprintf("[...document.querySelectorAll(%s)].filter(b => b.offsetParent && b.textContent.trim() === %s)[0].click();",
                         jsonlite::toJSON(scope, auto_unbox = TRUE), jsonlite::toJSON(label, auto_unbox = TRUE)))
      app$wait_for_idle(duration = 300)
    }
    click(".mb-nav button", screen$workspace)
    if (screen$workspace != "Project") click(".mb-tabbar button", screen$tab)
    shown <- app$get_js("[...document.querySelectorAll('.tab-pane.active')].filter(e => e.offsetParent).map(e => e.dataset.value)")
    expect_identical(unlist(shown)[if (screen$workspace == "Project") 1L else 2L], screen$tab)
    app$get_js("document.documentElement.scrollWidth - document.documentElement.clientWidth")
  }, numeric(1))
  names(overflow) <- vapply(screens, function(screen) paste(screen$workspace, screen$tab, sep = " / "), "")
  expect_equal(overflow[overflow > 0], stats::setNames(numeric(0), character(0)))
})
