WORKSPACES <- c("Project", "Write", "Model", "Behavior", "Analyze")

# Fingerprinted so a changed stylesheet can't be served stale (a Coach lesson).
asset_url <- function(file) {
  path <- file.path("www", file)
  stamp <- if (file.exists(path)) as.integer(file.mtime(path)) else 0L
  sprintf("%s?v=%d", file, stamp)
}

app_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = "#F4F4F2", fg = "#1C1D1F", primary = "#1C1D1F", secondary = "#5B5E63",
    info = "#2F5BD3", warning = "#B35A00", base_font = "IBM Plex Sans, system-ui, sans-serif",
    code_font = "IBM Plex Mono, ui-monospace, monospace", "border-radius" = "4px", "border-color" = "#D4D5D8",
    "font-size-base" = "0.875rem", "link-color" = "#2F5BD3"
  )
}

phase_placeholder <- function(what, phase) {
  shiny::tags$div(class = "mb-placeholder", sprintf("%s arrives in Phase %d.", what, phase))
}

app_ui <- function() {
  bslib::page_fillable(
    theme = app_theme(), padding = 0, gap = 0, fillable_mobile = FALSE, title = "ModelBench",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = asset_url("tokens.css")),
      shiny::tags$link(rel = "stylesheet", href = asset_url("app.css")),
      shiny::tags$script(src = asset_url("app.js"))
    ),
    shiny::tags$div(
      style = "display:block;overflow:auto",
      shiny::tags$header(
        class = "mb-topbar",
        shiny::tags$div(class = "mb-brand",
                        shiny::tags$span(class = "mb-product", "ModelBench"),
                        shiny::textOutput("context", inline = TRUE, container = shiny::span) |>
                          htmltools::tagAppendAttributes(class = "mb-context")),
        shiny::uiOutput("top_nav", container = shiny::tags$div, style = "margin-left:auto")
      ),
      shiny::uiOutput("status_strip"),
      bslib::navset_hidden(
        id = "workspace", selected = "Write",
        bslib::nav_panel_hidden("Project", mod_project_ui("project")),
        bslib::nav_panel_hidden(
          "Write",
          shiny::tags$div(class = "mb-tabbar", shiny::uiOutput("write_tabs")),
          bslib::navset_hidden(
            id = "write_tab", selected = "Draft",
            bslib::nav_panel_hidden("Draft", mod_write_ui("write")),
            bslib::nav_panel_hidden("Specification", mod_spec_ui("spec")),
            bslib::nav_panel_hidden("Traceability", phase_placeholder("Traceability", 4L))
          )
        ),
        bslib::nav_panel_hidden("Model", phase_placeholder("The model workspace", 2L)),
        bslib::nav_panel_hidden("Behavior", phase_placeholder("The behavior workspace", 6L)),
        bslib::nav_panel_hidden("Analyze", phase_placeholder("Health", 2L))
      )
    )
  )
}

WRITE_TABS <- c("Draft", "Specification", "Traceability")

app_server <- function(input, output, session) {
  dir <- workspace_dir()
  state <- new_app_state(open_default_project(dir))
  session$onSessionEnded(function() {
    con <- shiny::isolate(state$con())
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })

  output$context <- shiny::renderText({
    db_dep(state, "settings")
    db_dep(state, "baseline")
    project_context(state$con())
  })
  output$status_strip <- shiny::renderUI({
    db_dep(state, "model")
    db_dep(state, "behavior")
    db_dep(state, "ai")
    render_status_strip(status_counts(state$con()), "nav_click")
  })
  output$top_nav <- shiny::renderUI(render_top_nav(state$nav()$workspace, "nav_click"))
  output$write_tabs <- shiny::renderUI({
    active <- state$nav()$tab
    shiny::tags$div(role = "tablist", `aria-label` = "Write", class = "mb-segmented", lapply(WRITE_TABS, function(tab) {
      shiny::tags$button(type = "button", role = "tab", `aria-pressed` = tolower(as.character(identical(tab, active))),
                         `aria-selected` = tolower(as.character(identical(tab, active))),
                         onclick = event_js("nav_click", list(workspace = "Write", tab = tab)), tab)
    }))
  })

  apply_nav <- function(target) {
    tab <- if (is.null(target$tab)) shiny::isolate(state$nav()$tab) else target$tab
    if (is.null(tab) || !tab %in% WRITE_TABS) tab <- "Draft"
    if (!target$workspace %in% WORKSPACES) return(invisible(NULL))
    state$nav(list(workspace = target$workspace, tab = tab))
  }
  shiny::observeEvent(input$nav_click, apply_nav(input$nav_click))
  # Saving starts only after the restore, or the startup default would overwrite it.
  nav_restored <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$nav_restored, {
    apply_nav(input$nav_restored)
    nav_restored(TRUE)
  })
  shiny::observe({
    target <- state$nav()
    shiny::updateTabsetPanel(session, "workspace", selected = target$workspace)
    if (!is.null(target$tab)) shiny::updateTabsetPanel(session, "write_tab", selected = target$tab)
    if (nav_restored()) session$sendCustomMessage("mb_nav_save", list(project = shiny::isolate(basename(DBI::dbGetInfo(state$con())$dbname)),
                                                                      workspace = target$workspace, tab = target$tab))
  })
  # The app opens on Write unless this project remembers somewhere else.
  shiny::observe({
    nav_restored(FALSE)
    session$sendCustomMessage("mb_nav_restore", list(project = basename(DBI::dbGetInfo(state$con())$dbname)))
  })

  mod_project_server("project", state, dir)
  mod_write_server("write", state)
  mod_spec_server("spec", state)
}
