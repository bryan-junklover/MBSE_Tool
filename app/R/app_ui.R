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
    info = "#2F5BD3", warning = "#B35A00",
    # The fonts ship in www/fonts and are declared in fonts.css, so nothing loads from Google
    # at runtime (handoff §6.3). bslib::font_face() keeps one face per family, dropping weights.
    base_font = "'IBM Plex Sans', system-ui, sans-serif", code_font = "'IBM Plex Mono', ui-monospace, monospace",
    "border-radius" = "4px", "border-color" = "#D4D5D8",
    "font-size-base" = "0.875rem", "link-color" = "#2F5BD3"
  )
}

phase_placeholder <- function(what, phase) {
  shiny::tags$div(class = "mb-placeholder", sprintf("%s arrives in Phase %d.", what, phase))
}

app_ui <- function() {
  # Each screen carries its workspace's tab bar, drawn for that screen, in its own bar.
  screen <- function(workspace, tab, ui, ...) bslib::nav_panel_hidden(tab, ui(..., tabs = render_tab_bar(workspace, tab)))
  bslib::page_fillable(
    theme = app_theme(), padding = 0, gap = 0, fillable_mobile = FALSE, title = "ModelBench",
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = asset_url("fonts.css")),
      shiny::tags$link(rel = "stylesheet", href = asset_url("tokens.css")),
      shiny::tags$link(rel = "stylesheet", href = asset_url("app.css")),
      shiny::tags$script(src = asset_url("app.js"))
    ),
    shiny::tags$div(
      class = "mb-frame",
      shiny::tags$header(
        class = "mb-topbar",
        shiny::tags$div(class = "mb-brand",
                        shiny::tags$span(class = "mb-product", "ModelBench"),
                        shiny::textOutput("context", inline = TRUE, container = shiny::span) |>
                          htmltools::tagAppendAttributes(class = "mb-context")),
        shiny::uiOutput("top_nav", container = shiny::tags$div, style = "margin-left:auto"),
        shiny::tags$button(type = "button", class = "mb-btn mb-btn-ai mb-chat-toggle", `aria-controls` = "mb-chat", `aria-expanded` = "false",
                           onclick = "mbToggleChat()", "Ask the model")
      ),
      shiny::uiOutput("status_strip"),
      shiny::uiOutput("return_bar"),
      bslib::navset_hidden(
        id = "workspace", selected = "Write",
        bslib::nav_panel_hidden("Project", mod_project_ui("project")),
        bslib::nav_panel_hidden(
          "Write",
          bslib::navset_hidden(
            id = "write_tab", selected = "Draft",
            screen("Write", "Draft", mod_write_ui, "write"),
            screen("Write", "Specification", mod_spec_ui, "spec"),
            screen("Write", "Traceability", mod_trace_ui, "trace")
          )
        ),
        bslib::nav_panel_hidden(
          "Model",
          bslib::navset_hidden(
            id = "model_tab", selected = "Structure",
            screen("Model", "Structure", mod_model_ui, "model"),
            screen("Model", "Flows", mod_model_ui, "flows", "flows"),
            screen("Model", "Functional", mod_model_ui, "functional", "functional"),
            screen("Model", "Interfaces", mod_interfaces_ui, "interfaces"),
            screen("Model", "Stakeholder", mod_stakeholder_ui, "stakeholder")
          )
        ),
        bslib::nav_panel_hidden(
          "Behavior",
          bslib::navset_hidden(
            id = "behavior_tab", selected = "Scenarios",
            screen("Behavior", "Scenarios", mod_scenarios_ui, "scenarios"),
            screen("Behavior", "State machines", mod_machines_ui, "machines"),
            screen("Behavior", "Behavioral impact", mod_behavior_impact_ui, "behavior_impact")
          )
        ),
        bslib::nav_panel_hidden(
          "Analyze",
          bslib::navset_hidden(
            id = "analyze_tab", selected = "Health",
            screen("Analyze", "Health", mod_health_ui, "health"),
            screen("Analyze", "Review queue", mod_review_ui, "review"),
            screen("Analyze", "Baselines & impact", mod_baselines_ui, "baselines")
          )
        )
      )
    ),
    mod_chat_ui("chat_panel")
  )
}

WORKSPACE_TABS <- list(
  Write = c("Draft", "Specification", "Traceability"),
  Model = c("Structure", "Flows", "Functional", "Interfaces", "Stakeholder"),
  Behavior = c("Scenarios", "State machines", "Behavioral impact"),
  Analyze = c("Health", "Review queue", "Baselines & impact")
)
TAB_SETS <- c(Write = "write_tab", Model = "model_tab", Behavior = "behavior_tab", Analyze = "analyze_tab")

render_return_bar <- function(back, back_input, dismiss_input) {
  shiny::tags$div(class = "mb-returnbar", role = "note",
                  shiny::tags$span(class = "mb-caption", sprintf("From %s:", back$from$tab)), shiny::tags$span(back$label),
                  shiny::tags$button(type = "button", class = "mb-linkish", onclick = event_js(back_input, "back"), sprintf("Back to %s", tolower(back$from$tab))),
                  shiny::tags$button(type = "button", class = "mb-btn mb-btn-small mb-push", onclick = event_js(dismiss_input, "dismiss"), "Dismiss"))
}

render_tab_bar <- function(workspace, active) {
  tabs <- setdiff(WORKSPACE_TABS[[workspace]], "Stakeholder")
  bar <- shiny::tags$div(role = "tablist", `aria-label` = workspace, class = "mb-segmented", lapply(tabs, function(tab) {
    on <- tolower(as.character(identical(tab, active)))
    shiny::tags$button(type = "button", role = "tab", `aria-pressed` = on, `aria-selected` = on,
                       onclick = event_js("nav_click", list(workspace = workspace, tab = tab)), tab)
  }))
  if (workspace != "Model") return(bar)
  # The audience toggle: Stakeholder swaps the whole view for a plain one.
  stakeholder <- identical(active, "Stakeholder")
  audience <- function(label, on, tab) {
    shiny::tags$button(type = "button", `aria-pressed` = tolower(as.character(on)), onclick = event_js("nav_click", list(workspace = "Model", tab = tab)), label)
  }
  shiny::tags$div(style = "display:flex;flex-wrap:wrap;gap:8px 16px", if (!stakeholder) bar,
                  shiny::tags$div(role = "group", `aria-label` = "Audience", class = "mb-segmented",
                                  audience("Engineer", !stakeholder, "Structure"), audience("Stakeholder", stakeholder, "Stakeholder")))
}

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
    db_dep(state, "settings")
    # Checks the local server every so often, with a short timeout.
    shiny::invalidateLater(30000)
    render_status_strip(status_counts(state$con()), "nav_click", coach_status(state$con()), ai_usage_today(state$con()),
                        get_setting(state$con(), "coaching_policy", "tiered"))
  })
  output$top_nav <- shiny::renderUI(render_top_nav(state$nav()$workspace, "nav_click"))
  # The tab last used in each workspace, kept when moving between them.
  tabs <- shiny::reactiveVal(list(Write = "Draft", Model = "Structure", Behavior = "Scenarios", Analyze = "Health"))

  # The way back after a jump, shown on the screen the jump went to (UX review §2.26).
  output$return_bar <- shiny::renderUI({
    back <- state$return_to()
    here <- state$nav()
    if (is.null(back) || !identical(here$workspace, back$at$workspace) || !identical(tabs()[[here$workspace]], back$at$tab)) return(NULL)
    render_return_bar(back, "return_back", "return_dismiss")
  })
  shiny::observeEvent(input$return_back, {
    back <- state$return_to()
    state$return_to(NULL)
    if (!is.null(back)) apply_nav(back$from)
  })
  shiny::observeEvent(input$return_dismiss, state$return_to(NULL))

  apply_nav <- function(target) {
    if (!target$workspace %in% WORKSPACES) return(invisible(NULL))
    state$nav(list(workspace = target$workspace, tab = target$tab))
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
    workspace <- target$workspace
    known <- shiny::isolate(tabs())
    if (!is.null(target$tab) && target$tab %in% WORKSPACE_TABS[[workspace]]) {
      known[[workspace]] <- target$tab
      tabs(known)
    }
    shiny::updateTabsetPanel(session, "workspace", selected = workspace)
    tab <- known[[workspace]]
    if (!is.null(tab)) shiny::updateTabsetPanel(session, TAB_SETS[[workspace]], selected = tab)
    if (nav_restored()) {
      session$sendCustomMessage("mb_nav_save", list(project = shiny::isolate(basename(DBI::dbGetInfo(state$con())$dbname)),
                                                    workspace = workspace, tab = tab))
    }
  })
  # The app opens on Write unless this project remembers somewhere else.
  shiny::observe({
    nav_restored(FALSE)
    session$sendCustomMessage("mb_nav_restore", list(project = basename(DBI::dbGetInfo(state$con())$dbname)))
  })

  mod_project_server("project", state, dir)
  mod_write_server("write", state)
  mod_spec_server("spec", state)
  mod_model_server("model", state)
  mod_model_server("flows", state, "flows")
  mod_model_server("functional", state, "functional")
  mod_trace_server("trace", state)
  mod_interfaces_server("interfaces", state)
  mod_health_server("health", state)
  mod_review_server("review", state)
  mod_baselines_server("baselines", state)
  mod_stakeholder_server("stakeholder", state)
  mod_scenarios_server("scenarios", state)
  mod_machines_server("machines", state)
  mod_behavior_impact_server("behavior_impact", state)
  mod_ai_settings_server("ai_settings", state)
  mod_chat_server("chat_panel", state)
}
