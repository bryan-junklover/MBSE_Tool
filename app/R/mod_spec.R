# Write › Specification: outline, table, inspector, and import with a triage queue.

mod_spec_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "mb-tabbar",
      shiny::textInput(ns("search"), NULL, placeholder = "Search ID or text", width = "200px"),
      shiny::selectInput(ns("type"), NULL, choices = c("All types" = "all", REQ_TYPES), width = "160px"),
      shiny::checkboxInput(ns("low_first"), "Lowest score first", value = TRUE),
      shiny::tags$div(class = "mb-actions",
                      shiny::fileInput(ns("import"), NULL, buttonLabel = "Import…", placeholder = "csv, xlsx or txt",
                                       accept = c(".csv", ".xlsx", ".txt"), width = "260px"),
                      shiny::actionButton(ns("add"), "+ Requirement", class = "mb-btn"))
    ),
    shiny::tags$div(
      class = "mb-body",
      shiny::tags$aside(class = "mb-left",
                        shiny::tags$div(class = "mb-label", "Outline"),
                        shiny::uiOutput(ns("outline"))),
      shiny::tags$main(class = "mb-main",
                       shiny::uiOutput(ns("triage")),
                       shiny::tags$div(class = "mb-table", reactable::reactableOutput(ns("table"))),
                       shiny::textOutput(ns("count"))),
      shiny::tags$aside(class = "mb-right", shiny::uiOutput(ns("inspector")))
    )
  )
}

# reactable columns. Gaps are amber text with a word, never colour alone.
spec_columns <- function() {
  gap_text <- function(value, label) {
    if (is.na(value) || !nzchar(value)) shiny::tags$span(class = "mb-gap", label) else value
  }
  list(
    id = reactable::colDef(name = "ID", width = 85, class = "mono"),
    text = reactable::colDef(name = "Requirement", minWidth = 220),
    req_type = reactable::colDef(name = "Type", width = 95, cell = function(value) gap_text(value, "missing")),
    verification_method = reactable::colDef(name = "Verify by", width = 105, cell = function(value) gap_text(value, "missing")),
    allocated_to = reactable::colDef(name = "Allocated to", minWidth = 110, cell = function(value) gap_text(value, "none")),
    status = reactable::colDef(name = "Status", width = 85),
    gate_score = reactable::colDef(name = "Score", width = 60, class = "mono", cell = function(value, index) {
      if (is.na(value)) return("")
      if (value < 50L) shiny::tags$span(class = "mb-gap", style = "font-weight:500", value) else value
    })
  )
}

mod_spec_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    selected <- state$selected_requirement
    import_note <- shiny::reactiveVal(NULL)

    rows <- shiny::reactive({
      db_dep(state, "spec")
      requirement_table(con())
    })
    filtered <- shiny::reactive({
      filter_requirements(rows(), if (is.null(input$search)) "" else input$search,
                          if (is.null(input$type)) "all" else input$type, isTRUE(input$low_first))
    })

    output$table <- reactable::renderReactable({
      data <- filtered()[, c("id", "text", "req_type", "verification_method", "allocated_to", "status", "gate_score")]
      chosen <- selected()
      reactable::reactable(
        data, columns = spec_columns(), pagination = FALSE, highlight = TRUE, compact = TRUE, borderless = FALSE,
        onClick = reactable::JS(sprintf("function(rowInfo) { Shiny.setInputValue('%s', rowInfo.original.id, {priority: 'event'}); }", ns("select"))),
        rowStyle = function(index) {
          if (identical(data$id[index], chosen)) list(outline = "2px solid var(--ink)", outlineOffset = "-2px", background = "var(--surface-muted)")
        },
        theme = reactable::reactableTheme(headerStyle = list(background = "var(--surface-muted)", fontWeight = 500), style = list(fontSize = "13px", fontFamily = "inherit"))
      )
    })
    output$count <- shiny::renderText({
      all_rows <- rows()
      sprintf("Showing %d of %d · %d failing the gate", nrow(filtered()), nrow(all_rows), sum(all_rows$gate_pass == 0L))
    })
    output$outline <- shiny::renderUI({
      db_dep(state, "spec")
      render_outline(requirement_outline(con()), ns("select"), selected())
    })
    shiny::observeEvent(input$select, selected(input$select))

    # --- Inspector: details save on change ---
    current <- shiny::reactive({
      db_dep(state, "spec")
      id <- selected()
      if (is.null(id)) return(NULL)
      read_entity(con(), "requirement", id)
    })

    output$inspector <- shiny::renderUI({
      # Rebuilt on selection only, so saving a field doesn't drop the focus.
      id <- selected()
      if (is.null(id)) return(shiny::tags$p(class = "mb-caption", "Select a requirement to see its details."))
      row <- shiny::isolate(read_entity(con(), "requirement", id))
      if (is.null(row)) return(NULL)
      value <- function(x) if (is.na(x)) "" else x
      others <- shiny::isolate(requirement_outline(con()))
      others <- others[others$id != id, ]
      parents <- stats::setNames(others$id, ifelse(is.na(others$short_title), others$id, paste(others$id, others$short_title)))
      shiny::tagList(
        shiny::tags$div(
          shiny::tags$div(class = "mono mb-caption", sprintf("%s · %s", id, row$status)),
          shiny::tags$div(style = "font-size:16px;line-height:1.4", row$text)
        ),
        shiny::textInput(ns("d_title"), "Short title", value(row$short_title)),
        shiny::selectInput(ns("d_parent"), "Parent", choices = with_blank(parents, "None"), selected = value(row$parent_id)),
        shiny::selectInput(ns("d_type"), "Type", choices = with_blank(REQ_TYPES), selected = value(row$req_type)),
        shiny::selectInput(ns("d_verify"), "Verify by", choices = with_blank(VERIFY_METHODS), selected = value(row$verification_method)),
        shiny::textAreaInput(ns("d_rationale"), "Rationale", value(row$rationale), rows = 3),
        shiny::uiOutput(ns("findings")),
        shiny::actionButton(ns("rewrite"), "Rewrite it in Write", class = "mb-btn")
      )
    })
    output$findings <- shiny::renderUI({
      row <- current()
      shiny::req(row)
      db_dep(state, "model")
      render_findings(requirement_findings(row, model_terms(con())))
    })

    save_detail <- function(field, value) {
      row <- shiny::isolate(current())
      if (is.null(row)) return(invisible(NULL))
      written <- guarded_edit(save_requirement_details(con(), row$id, stats::setNames(list(value), field)))
      if (!is.null(written)) invalidate_db(state, written$area)
      invisible(NULL)
    }
    title_debounced <- shiny::debounce(shiny::reactive(input$d_title), 400)
    rationale_debounced <- shiny::debounce(shiny::reactive(input$d_rationale), 400)
    shiny::observeEvent(title_debounced(), save_detail("short_title", title_debounced()), ignoreInit = TRUE)
    shiny::observeEvent(rationale_debounced(), save_detail("rationale", rationale_debounced()), ignoreInit = TRUE)
    shiny::observeEvent(input$d_parent, save_detail("parent_id", input$d_parent), ignoreInit = TRUE)
    shiny::observeEvent(input$d_type, save_detail("req_type", input$d_type), ignoreInit = TRUE)
    shiny::observeEvent(input$d_verify, save_detail("verification_method", input$d_verify), ignoreInit = TRUE)

    shiny::observeEvent(input$rewrite, {
      state$open_in_write(selected())
      go_to(state, "Write", "Draft")
    })
    shiny::observeEvent(input$add, {
      state$open_in_write(list(text = ""))
      go_to(state, "Write", "Draft")
    })

    # --- Import and triage ---
    shiny::observeEvent(input$import, {
      file <- input$import
      result <- guarded_edit({
        parsed <- read_import_file(file$datapath, file$name)
        import_requirements(con(), parsed, file$name)
      })
      if (is.null(result)) return()
      import_note(sprintf("Imported %s: %d added as drafts, %d to fix.", file$name, length(result$added), result$queued))
      invalidate_db(state, "spec")
    })

    pending <- shiny::reactive({
      db_dep(state, "spec")
      triage_pending(con())
    })
    output$triage <- shiny::renderUI({
      queue <- pending()
      note <- import_note()
      if (nrow(queue) == 0L) {
        return(if (!is.null(note)) shiny::tags$div(class = "mb-panel", note))
      }
      row <- queue[1, ]
      blockers <- jsonlite::fromJSON(row$gate_json)
      shiny::tags$section(
        class = "mb-panel mb-queue",
        shiny::tags$div(class = "mb-head", shiny::tags$b(sprintf("Fix imported rows · %d left", nrow(queue))),
                        shiny::tags$span(class = "mono mb-caption mb-push", sprintf("%s · row %d", row$source_name, row$row_no))),
        if (!is.null(note)) shiny::tags$div(class = "mb-caption", note),
        lapply(blockers, function(b) shiny::tags$div(class = "mb-gap", b)),
        shiny::textAreaInput(ns("triage_text"), "Requirement", row$raw_text, rows = 2, width = "100%"),
        shiny::tags$div(class = "mb-term-actions",
                        shiny::actionButton(ns("triage_fix"), "Check it and add as a draft", class = "mb-btn"),
                        shiny::actionButton(ns("triage_skip"), "Skip this row", class = "mb-btn"))
      )
    })
    shiny::observeEvent(input$triage_fix, {
      queue <- pending()
      shiny::req(nrow(queue) > 0L)
      if (!is.null(guarded_edit(triage_fix(con(), queue$id[1], input$triage_text)))) invalidate_db(state, "spec")
    })
    shiny::observeEvent(input$triage_skip, {
      queue <- pending()
      shiny::req(nrow(queue) > 0L)
      triage_skip(con(), queue$id[1])
      invalidate_db(state, "spec")
    })

    invisible(list(filtered = filtered, pending = pending))
  })
}
