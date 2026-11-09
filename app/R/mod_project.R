# Project workspace: which project is open, scope, glossary and id prefixes.

PROJECT_SECTIONS <- c("General", "Glossary", "Units & properties", "AI", "Data protection")

# Project settings in four tabs, in a centred reading column (UX review §2.29).
mod_project_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(shiny::uiOutput(ns("sections"))),
    shiny::tags$div(
      class = "mb-scroll",
      shiny::tags$div(
        class = "mb-reading mb-project",
        bslib::navset_hidden(
          id = ns("section"), selected = "General",
          bslib::nav_panel_hidden(
            "General",
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Project"),
                                shiny::uiOutput(ns("project_picker")),
                                shiny::uiOutput(ns("project_admin")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("new_name"), "New project name"),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("create"), "Create project", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Scope"),
                                shiny::textAreaInput(ns("scope"), "What this system is, in a few sentences. Saved as you type.", rows = 3, width = "100%")),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("ID prefixes"),
                                shiny::tags$span(class = "mb-caption", "Used for new items. Existing ids never change."),
                                shiny::uiOutput(ns("prefixes")),
                                shiny::tags$div(shiny::actionButton(ns("save_prefixes"), "Save prefixes", class = "mb-btn")))
          ),
          bslib::nav_panel_hidden(
            "Glossary",
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Glossary"),
                                shiny::tags$span(class = "mb-caption", "A term with an element is an alias of it. One with no element isn't a model term."),
                                shiny::uiOutput(ns("glossary")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("g_term"), "Term"),
                                                shiny::textInput(ns("g_definition"), "Definition"),
                                                shiny::selectInput(ns("g_element"), "Alias of", choices = with_blank(character(0), "No element"), selectize = FALSE),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("g_add"), "Add term", class = "mb-btn"))))
          ),
          bslib::nav_panel_hidden(
            "Units & properties",
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Properties"),
                                shiny::tags$span(class = "mb-caption", "What a component can hold, such as Mass in kg. A roll-up adds values up the tree, times how many."),
                                shiny::uiOutput(ns("properties")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("p_label"), "Name"),
                                                shiny::selectInput(ns("p_type"), "Kind", choices = c("Number" = "number", "Text" = "text"), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("p_unit"), "Unit", choices = c("None" = ""), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("p_rollup"), "Roll-up", choices = c("None" = "none", "Sum" = "sum", "Largest" = "max", "Smallest" = "min"),
                                                                   width = "auto", selectize = FALSE),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("p_add"), "Add property", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Budgets"),
                                shiny::tags$span(class = "mb-caption", "A limit on a rolled-up property over part of the system. Health and the Structure inspector check it on every edit."),
                                shiny::uiOutput(ns("budgets")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::selectInput(ns("b_property"), "Property", choices = character(0), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("b_scope"), "Over", choices = character(0), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("b_comparison"), "Must be", choices = c("At most" = "max", "At least" = "min"), width = "auto", selectize = FALSE),
                                                shiny::textInput(ns("b_limit"), "Limit", width = "120px"),
                                                shiny::selectInput(ns("b_unit"), "Unit", choices = character(0), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("b_requirement"), "From requirement", choices = c("None" = ""), width = "auto", selectize = FALSE),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("b_add"), "Add budget", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Analysis limits"),
                                shiny::tags$span(class = "mb-caption", "A limit on a calculated result, such as endurance, over part of the system."),
                                shiny::uiOutput(ns("limits")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::selectInput(ns("al_analysis"), "Analysis", choices = character(0), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("al_scope"), "Over", choices = character(0), width = "auto", selectize = FALSE),
                                                shiny::selectInput(ns("al_comparison"), "Must be", choices = c("At least" = "min", "At most" = "max"), width = "auto", selectize = FALSE),
                                                shiny::textInput(ns("al_limit"), "Limit", width = "120px"),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("al_add"), "Add limit", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Saved views"),
                                shiny::tags$span(class = "mb-caption", "Filtered Structure views, offered under Saved view on Structure."),
                                shiny::uiOutput(ns("views")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("sv_name"), "Name", width = "160px"),
                                                shiny::checkboxGroupInput(ns("sv_domains"), "Domains",
                                                                          c("Hardware" = "hw", "Software" = "sw", "Hardware and software" = "hw_sw", "Other" = "other"), inline = TRUE),
                                                shiny::selectInput(ns("sv_stereotype"), "Stereotype", choices = c("Any" = ""), width = "auto", selectize = FALSE),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("sv_add"), "Add view", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Stereotypes"),
                                shiny::tags$span(class = "mb-caption", "Labels for kinds of component, shown on the diagram and used by saved views."),
                                shiny::uiOutput(ns("stereotypes")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("st_name"), "Name", width = "160px"),
                                                shiny::textInput(ns("st_description"), "Description"),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("st_add"), "Add stereotype", class = "mb-btn")))),
            shiny::tags$section(class = "mb-panel",
                                shiny::tags$b("Units"),
                                shiny::tags$span(class = "mb-caption",
                                                 "Every project has the standard units. Add your own here; a quantity already listed keeps its SI unit, so values convert."),
                                shiny::uiOutput(ns("units")),
                                shiny::tags$div(class = "mb-fields",
                                                shiny::textInput(ns("u_symbol"), "Symbol", width = "100px"),
                                                shiny::textInput(ns("u_quantity"), "Measures", placeholder = "speed", width = "140px"),
                                                shiny::textInput(ns("u_si"), "SI unit", placeholder = "m/s", width = "100px"),
                                                shiny::textInput(ns("u_factor"), "1 of it in SI", placeholder = "0.5144", width = "120px"),
                                                shiny::textInput(ns("u_offset"), "Offset", placeholder = "0", width = "90px"),
                                                shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("u_add"), "Add unit", class = "mb-btn"))))
          ),
          bslib::nav_panel_hidden("AI", mod_ai_settings_ui("ai_settings", "ai")),
          bslib::nav_panel_hidden("Data protection", mod_ai_settings_ui("ai_settings", "data"))
        )
      )
    )
  )
}

mod_project_server <- function(id, state, workspace_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    refresh_projects <- shiny::reactiveVal(0L)
    section <- shiny::reactiveVal("General")

    output$sections <- shiny::renderUI({
      shiny::tags$div(role = "tablist", `aria-label` = "Project", class = "mb-segmented", lapply(PROJECT_SECTIONS, function(name) {
        on <- tolower(as.character(identical(section(), name)))
        shiny::tags$button(type = "button", role = "tab", `aria-pressed` = on, `aria-selected` = on, onclick = event_js(ns("pick_section"), name), name)
      }))
    })
    shiny::observeEvent(input$pick_section, {
      section(input$pick_section)
      shiny::updateTabsetPanel(session, "section", selected = input$pick_section)
    })

    output$project_picker <- shiny::renderUI({
      refresh_projects()
      files <- list_projects(workspace_dir)
      current <- basename(DBI::dbGetInfo(con())$dbname)
      shiny::selectInput(ns("project"), "Open project", choices = files, selected = current, selectize = FALSE)
    })
    shiny::observeEvent(input$project, {
      target <- file.path(workspace_dir, input$project)
      if (identical(normalizePath(target, mustWork = FALSE), normalizePath(DBI::dbGetInfo(con())$dbname, mustWork = FALSE))) return()
      switch_project(state, target)
    }, ignoreInit = TRUE)
    output$project_admin <- shiny::renderUI({
      refresh_projects()
      db_dep(state, "settings")
      current <- basename(DBI::dbGetInfo(con())$dbname)
      others <- setdiff(list_projects(workspace_dir), current)
      shiny::tags$div(class = "mb-fields",
                      shiny::textInput(ns("project_name"), "Name of this project", get_setting(con(), "project_name", "")),
                      shiny::tags$div(style = "align-self:end", shiny::actionButton(ns("copy_project"), "Copy as new project…", class = "mb-btn")),
                      if (length(others) > 0L) {
                        shiny::tags$div(style = "align-self:end",
                                        select_tag(stats::setNames(others, others), "", change_js(ns("delete_project_pick"), list()), "Delete another project…",
                                                   label = "Delete another project"))
                      })
    })
    name_debounced <- shiny::debounce(shiny::reactive(input$project_name), 600)
    shiny::observeEvent(name_debounced(), {
      name <- trimws(name_debounced())
      if (nzchar(name) && !identical(name, get_setting(con(), "project_name", ""))) {
        set_setting(con(), "project_name", name)
        invalidate_db(state, "settings")
      }
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$copy_project, {
      shiny::showModal(shiny::modalDialog(title = "Copy as a new project",
                                          shiny::textInput(ns("copy_name"), "New project name", paste(get_setting(con(), "project_name", "Project"), "copy")),
                                          shiny::tags$p(class = "mb-caption", "The copy has everything: history, baselines and settings. It opens straight away."),
                                          footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("copy_confirm"), "Copy", class = "mb-btn mb-btn-primary"))))
    })
    shiny::observeEvent(input$copy_confirm, {
      path <- guarded_edit(copy_project(workspace_dir, con(), input$copy_name))
      if (is.null(path)) return()
      shiny::removeModal()
      switch_project(state, path)
      refresh_projects(refresh_projects() + 1L)
    })
    pending_delete <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$delete_project_pick, {
      if (!nzchar(input$delete_project_pick$value)) return()
      pending_delete(input$delete_project_pick$value)
      shiny::showModal(shiny::modalDialog(title = sprintf("Delete %s?", input$delete_project_pick$value),
                                          shiny::tags$p("The project's file goes for good, with its history and baselines. This can't be undone."),
                                          footer = shiny::tagList(shiny::modalButton("Cancel"),
                                                                  shiny::actionButton(ns("delete_project_confirm"), "Delete project for good", class = "mb-btn mb-btn-primary"))))
    })
    shiny::observeEvent(input$delete_project_confirm, {
      done <- guarded_edit(delete_project(workspace_dir, pending_delete(), DBI::dbGetInfo(con())$dbname))
      shiny::removeModal()
      if (!is.null(done)) refresh_projects(refresh_projects() + 1L)
    })
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
    # --- Units & properties ---
    output$properties <- shiny::renderUI({
      db_dep(state, "model")
      rows <- property_def_list(con())
      if (nrow(rows) == 0L) return(shiny::tags$span(class = "mb-caption", "No properties yet."))
      rollups <- c("None" = "none", "Sum" = "sum", "Largest" = "max", "Smallest" = "min")
      units <- unit_list(con())
      simple_table(c("Property", "Kind", "Unit", "Roll-up", "Held by", ""),
                   lapply(seq_len(nrow(rows)), function(i) {
                     change <- function(field) change_js(ns("p_change"), list(id = rows$id[i], field = field))
                     number <- rows$value_type[i] == "number"
                     # A unit changes only within its quantity, so values stay comparable.
                     same <- if (is.na(rows$unit[i])) units$symbol else units$symbol[units$quantity == units$quantity[units$symbol == rows$unit[i]]]
                     list(shiny::tags$input(type = "text", value = rows$label[i], onchange = change("label"), `aria-label` = paste("Name of", rows$label[i])),
                          rows$value_type[i],
                          if (number) {
                            select_tag(stats::setNames(same, same), if (is.na(rows$unit[i])) "" else rows$unit[i], change("unit"), "None", label = paste("Unit of", rows$label[i]))
                          } else {
                            "—"
                          },
                          if (number) select_tag(rollups, rows$rollup[i], change("rollup"), label = paste("Roll-up of", rows$label[i])) else "—",
                          sprintf("%d component%s", rows$used[i], if (rows$used[i] == 1L) "" else "s"),
                          if (rows$used[i] == 0L) {
                            shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Delete", rows$label[i]),
                                               onclick = event_js(ns("p_delete"), rows$id[i]), "Delete")
                          })
                   }))
    })
    output$budgets <- shiny::renderUI({
      db_dep(state, "model")
      rows <- budget_list(con())
      if (nrow(rows) == 0L) return(shiny::tags$span(class = "mb-caption", "No budgets yet."))
      simple_table(c("Budget", "Over", "Now / limit", ""),
                   lapply(seq_len(nrow(rows)), function(i) {
                     list(sprintf("%s, %s", rows$label[i], if (rows$comparison[i] == "max") "at most" else "at least"), rows$scope_name[i],
                          shiny::tags$span(class = paste("mono", if (!rows$pass[i]) "mb-gap"), budget_status_text(rows$text[i], rows$pass[i], rows$note[i])),
                          confirm_button("Remove", ns("b_delete"), rows$id[i], sprintf("Remove the %s budget?", tolower(rows$label[i])),
                                         sprintf("%s no longer has a %s limit. Undo brings it back.", rows$scope_name[i], tolower(rows$label[i])),
                                         confirm = "Remove budget", class = "mb-btn mb-btn-small"))
                   }))
    })
    output$units <- shiny::renderUI({
      db_dep(state, "settings")
      rows <- unit_list(con())
      groups <- split(rows$symbol, rows$quantity)
      shiny::tags$div(class = "mb-prop-grid", style = "grid-template-columns: minmax(120px,auto) minmax(0,1fr)",
                      unlist(lapply(names(groups), function(quantity) {
                        list(shiny::tags$span(class = "mb-caption", quantity), shiny::tags$span(class = "mono", paste(groups[[quantity]], collapse = "  ")))
                      }), recursive = FALSE))
    })
    shiny::observe({
      db_dep(state, "settings")
      db_dep(state, "model")
      db_dep(state, "spec")
      units <- unit_list(con())$symbol
      shiny::updateSelectInput(session, "p_unit", choices = c("None" = "", stats::setNames(units, units)))
      defs <- property_def_list(con())
      defs <- defs[defs$rollup != "none" & !is.na(defs$unit), , drop = FALSE]
      shiny::updateSelectInput(session, "b_property", choices = stats::setNames(defs$id, sprintf("%s (%s)", defs$label, defs$unit)))
      components <- DBI::dbGetQuery(con(), "SELECT id, name FROM element WHERE kind = 'component' ORDER BY parent_id IS NOT NULL, sort_order, name")
      shiny::updateSelectInput(session, "b_scope", choices = stats::setNames(components$id, components$name))
      requirements <- DBI::dbGetQuery(con(), "SELECT id, coalesce(short_title, substr(text, 1, 40)) AS title FROM requirement WHERE status = 'committed' ORDER BY id")
      shiny::updateSelectInput(session, "b_requirement", choices = c("None" = "", stats::setNames(requirements$id, paste(requirements$id, requirements$title))))
    })
    # The budget's unit choices are the property's own quantity.
    shiny::observe({
      db_dep(state, "settings")
      def <- if (is.null(input$b_property) || !nzchar(input$b_property)) NULL else DBI::dbGetQuery(con(), "SELECT unit FROM property_def WHERE id = ?", params = list(input$b_property))
      units <- unit_list(con())
      same <- if (is.null(def) || nrow(def) == 0L || is.na(def$unit)) character(0) else units$symbol[units$quantity == units$quantity[units$symbol == def$unit]]
      shiny::updateSelectInput(session, "b_unit", choices = same, selected = if (!is.null(def) && nrow(def) == 1L) def$unit)
    })
    output$limits <- shiny::renderUI({
      db_dep(state, "model")
      rows <- analysis_status(con())
      if (nrow(rows) == 0L) return(shiny::tags$span(class = "mb-caption", "No analysis limits yet."))
      names <- DBI::dbGetQuery(con(), "SELECT id, name FROM element")
      simple_table(c("Analysis", "Over", "Now / limit", ""), lapply(seq_len(nrow(rows)), function(i) {
        list(term_to_title(rows$analysis[i]), names$name[match(rows$scope[i], names$id)],
             shiny::tags$span(class = paste("mono", if (!rows$pass[i]) "mb-gap"), sprintf("%s%s", rows$text[i], if (rows$pass[i]) "" else " · FAIL")),
             shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Remove the", rows$analysis[i], "limit"),
                                onclick = event_js(ns("al_delete"), paste(rows$analysis[i], rows$scope[i], sep = "|")), "Remove"))
      }))
    })
    shiny::observe({
      db_dep(state, "model")
      shiny::updateSelectInput(session, "al_analysis", choices = stats::setNames(registered_analyses(), term_to_title(registered_analyses())))
      components <- DBI::dbGetQuery(con(), "SELECT id, name FROM element WHERE kind = 'component' ORDER BY parent_id IS NOT NULL, sort_order, name")
      shiny::updateSelectInput(session, "al_scope", choices = stats::setNames(components$id, components$name))
      stereo <- stereotype_list(con())
      shiny::updateSelectInput(session, "sv_stereotype", choices = c("Any" = "", stats::setNames(stereo$id, stereo$name)))
    })
    shiny::observeEvent(input$al_add, {
      if (!is.null(model_edit(state, add_analysis_limit(con(), input$al_analysis, input$al_scope, input$al_limit, input$al_comparison), "add analysis limit"))) {
        shiny::updateTextInput(session, "al_limit", value = "")
      }
    })
    shiny::observeEvent(input$al_delete, model_edit(state, delete_entity(con(), "analysis_limit", input$al_delete), "remove analysis limit"))
    output$views <- shiny::renderUI({
      db_dep(state, "settings")
      rows <- saved_views(con())
      simple_table(c("View", "Shows", ""), lapply(seq_len(nrow(rows)), function(i) {
        filter <- if (is.na(rows$filter_json[i])) list() else jsonlite::fromJSON(rows$filter_json[i])
        shows <- paste(c(if (!is.null(filter$domains)) paste("domain", paste(filter$domains, collapse = " or ")),
                         if (!is.null(filter$stereotypes)) paste("stereotype", paste(filter$stereotypes, collapse = " or ")),
                         if (!is.null(filter$property)) sprintf("%s %s %s", filter$property$name, filter$property$op, filter$property$value)), collapse = ", ")
        list(rows$name[i], if (nzchar(shows)) shows else "all components",
             if (rows$is_builtin[i] == 0L) shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Delete view", rows$name[i]),
                                                             onclick = event_js(ns("sv_delete"), rows$id[i]), "Delete") else "built in")
      }))
    })
    shiny::observeEvent(input$sv_add, {
      added <- guarded_edit(add_saved_view(con(), input$sv_name, if (is.null(input$sv_domains)) character(0) else input$sv_domains, input$sv_stereotype))
      if (is.null(added)) return()
      invalidate_db(state, c("settings", "model"))
      shiny::updateTextInput(session, "sv_name", value = "")
    })
    shiny::observeEvent(input$sv_delete, {
      if (!is.null(guarded_edit(delete_saved_view(con(), input$sv_delete)))) invalidate_db(state, c("settings", "model"))
    })
    output$stereotypes <- shiny::renderUI({
      db_dep(state, "model")
      rows <- stereotype_list(con())
      if (nrow(rows) == 0L) return(shiny::tags$span(class = "mb-caption", "No stereotypes yet."))
      simple_table(c("Stereotype", "Description", "Used by", ""), lapply(seq_len(nrow(rows)), function(i) {
        list(rows$name[i], if (is.na(rows$description[i])) "" else rows$description[i], sprintf("%d component%s", rows$used[i], if (rows$used[i] == 1L) "" else "s"),
             if (rows$used[i] == 0L) shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Delete", rows$name[i]),
                                                       onclick = event_js(ns("st_delete"), rows$id[i]), "Delete"))
      }))
    })
    shiny::observeEvent(input$st_add, {
      if (!is.null(model_edit(state, add_stereotype(con(), input$st_name, input$st_description), "add stereotype"))) {
        shiny::updateTextInput(session, "st_name", value = "")
        shiny::updateTextInput(session, "st_description", value = "")
      }
    })
    shiny::observeEvent(input$st_delete, model_edit(state, delete_stereotype(con(), input$st_delete), "delete stereotype"))
    shiny::observeEvent(input$p_change, model_edit(state, update_property_def(con(), input$p_change$id, input$p_change$field, input$p_change$value), "edit property"))
    shiny::observeEvent(input$p_delete, model_edit(state, delete_property_def(con(), input$p_delete), "delete property"))
    shiny::observeEvent(input$p_add, {
      written <- model_edit(state, add_property_def(con(), input$p_label, input$p_type, input$p_unit, input$p_rollup), "add property")
      if (!is.null(written)) shiny::updateTextInput(session, "p_label", value = "")
    })
    shiny::observeEvent(input$b_add, {
      written <- model_edit(state, add_budget(con(), input$b_property, input$b_scope, input$b_limit, input$b_unit, input$b_comparison, input$b_requirement), "add budget")
      if (!is.null(written)) shiny::updateTextInput(session, "b_limit", value = "")
    })
    shiny::observeEvent(input$b_delete, model_edit(state, delete_entity(con(), "budget", input$b_delete), "remove budget"))
    shiny::observeEvent(input$u_add, {
      added <- guarded_edit(add_unit(con(), input$u_symbol, input$u_quantity, input$u_si, input$u_factor, input$u_offset))
      if (is.null(added)) return()
      invalidate_db(state, "settings")
      for (field in c("u_symbol", "u_quantity", "u_si", "u_factor", "u_offset")) shiny::updateTextInput(session, field, value = "")
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

# A small read-only table: a header row and one list of cells per row.
simple_table <- function(header, rows) {
  shiny::tags$div(class = "mb-table", shiny::tags$table(
    class = "mb-simple",
    shiny::tags$thead(shiny::tags$tr(lapply(header, shiny::tags$th))),
    shiny::tags$tbody(lapply(rows, function(cells) shiny::tags$tr(lapply(cells, shiny::tags$td))))
  ))
}
