# Project workspace: which project is open, scope, glossary and id prefixes.

mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "mb-body",
    shiny::tags$main(
      class = "mb-main", style = "max-width:1200px",
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$b("Project"),
                          shiny::uiOutput(ns("project_picker")),
                          shiny::tags$div(class = "mb-fields",
                                          shiny::textInput(ns("new_name"), "New project name"),
                                          shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("create"), "Create project", class = "mb-btn")))),
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$b("Scope"),
                          shiny::textAreaInput(ns("scope"), "What this system is, in a few sentences. Saved as you type.", rows = 3, width = "100%")),
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$b("Glossary"),
                          shiny::tags$span(class = "mb-caption", "A term with an element is an alias of it. One with no element isn't a model term."),
                          shiny::uiOutput(ns("glossary")),
                          shiny::tags$div(class = "mb-fields",
                                          shiny::textInput(ns("g_term"), "Term"),
                                          shiny::textInput(ns("g_definition"), "Definition"),
                                          shiny::selectInput(ns("g_element"), "Alias of", choices = with_blank(character(0), "No element")),
                                          shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("g_add"), "Add term", class = "mb-btn")))),
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$b("ID prefixes"),
                          shiny::tags$span(class = "mb-caption", "Used for new items. Existing ids never change."),
                          shiny::uiOutput(ns("prefixes")),
                          shiny::tags$div(shiny::actionButton(ns("save_prefixes"), "Save prefixes", class = "mb-btn"))),
      mod_ai_settings_ui("ai_settings")
    )
  )
}

mod_project_server <- function(id, state, workspace_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    refresh_projects <- shiny::reactiveVal(0L)

    output$project_picker <- shiny::renderUI({
      refresh_projects()
      files <- list_projects(workspace_dir)
      current <- basename(DBI::dbGetInfo(con())$dbname)
      shiny::selectInput(ns("project"), "Open project", choices = files, selected = current)
    })
    shiny::observeEvent(input$project, {
      target <- file.path(workspace_dir, input$project)
      if (identical(normalizePath(target, mustWork = FALSE), normalizePath(DBI::dbGetInfo(con())$dbname, mustWork = FALSE))) return()
      switch_project(state, target)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$create, {
      opened <- guarded_edit(create_project(workspace_dir, input$new_name))
      if (is.null(opened)) return()
      switch_project(state, opened)
      refresh_projects(refresh_projects() + 1L)
      shiny::updateTextInput(session, "new_name", value = "")
    })

    # Filled once per project, so saving doesn't move the cursor.
    shiny::observe({
      connection <- con()
      shiny::updateTextAreaInput(session, "scope", value = get_setting(connection, "scope_text", ""))
    })
    scope_debounced <- shiny::debounce(shiny::reactive(input$scope), 400)
    shiny::observeEvent(scope_debounced(), {
      if (!identical(scope_debounced(), get_setting(con(), "scope_text", ""))) {
        set_setting(con(), "scope_text", scope_debounced())
        invalidate_db(state, "settings")
      }
    }, ignoreInit = TRUE)

    output$glossary <- shiny::renderUI({
      db_dep(state, "settings")
      rows <- DBI::dbGetQuery(con(), "SELECT g.term, g.definition, e.name AS element FROM glossary g
                                      LEFT JOIN element e ON e.id = g.element_id ORDER BY g.term")
      if (nrow(rows) == 0L) return(shiny::tags$span(class = "mb-caption", "No terms yet."))
      shiny::tags$div(class = "mb-table", shiny::tags$table(
        class = "mb-simple",
        shiny::tags$thead(shiny::tags$tr(shiny::tags$th("Term"), shiny::tags$th("Definition"), shiny::tags$th("Alias of"), shiny::tags$th(""))),
        shiny::tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
          text <- function(x) if (is.na(x)) "" else x
          shiny::tags$tr(
            shiny::tags$td(class = "mono", rows$term[i]), shiny::tags$td(text(rows$definition[i])),
            shiny::tags$td(if (is.na(rows$element[i])) "Not a model term" else rows$element[i]),
            shiny::tags$td(shiny::tags$button(type = "button", class = "mb-btn mb-btn-small",
                                              onclick = event_js(ns("g_delete"), rows$term[i]),
                                              `aria-label` = paste("Remove", rows$term[i]), "Remove"))
          )
        }))
      ))
    })
    shiny::observe({
      db_dep(state, "model")
      elements <- DBI::dbGetQuery(con(), "SELECT id, name FROM element ORDER BY name")
      shiny::updateSelectInput(session, "g_element", choices = with_blank(stats::setNames(elements$id, elements$name), "No element"))
    })
    shiny::observeEvent(input$g_add, {
      term <- trimws(input$g_term)
      if (!nzchar(term)) return(refusal_toast("A glossary entry needs a term."))
      blank <- function(x) if (is.null(x) || !nzchar(x)) NULL else x
      upsert_glossary(con(), term, blank(input$g_definition), blank(input$g_element))
      invalidate_db(state, "settings")
      shiny::updateTextInput(session, "g_term", value = "")
      shiny::updateTextInput(session, "g_definition", value = "")
    })
    shiny::observeEvent(input$g_delete, {
      delete_glossary(con(), input$g_delete)
      invalidate_db(state, "settings")
    })

    output$prefixes <- shiny::renderUI({
      db_dep(state, "settings")
      shiny::tags$div(class = "mb-fields", lapply(names(CONFIGURABLE_PREFIXES), function(what) {
        shiny::textInput(ns(paste0("prefix_", what)), term_to_title(what), configured_prefix(con(), what))
      }))
    })
    shiny::observeEvent(input$save_prefixes, {
      for (what in names(CONFIGURABLE_PREFIXES)) {
        value <- input[[paste0("prefix_", what)]]
        if (!is.null(value) && !identical(toupper(trimws(value)), configured_prefix(con(), what))) {
          guarded_edit(set_id_prefix(con(), what, value))
        }
      }
      invalidate_db(state, "settings")
    })
  })
}
