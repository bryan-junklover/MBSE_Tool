# Write › Specification: outline, table, inspector, and import with a triage queue.

export_menu <- function(...) {
  shiny::tags$details(class = "mb-menu", shiny::tags$summary(class = "mb-btn", "Export ▾"),
                      shiny::tags$div(class = "mb-menu-list", role = "group", `aria-label` = "Export", ...))
}

mod_spec_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(
      tabs,
      aria_label(shiny::textInput(ns("search"), NULL, placeholder = "Search ID or text", width = "200px"), "input", "Search ID or text"),
      aria_label(shiny::selectInput(ns("type"), NULL, choices = c("All types" = "all", REQ_TYPES), width = "150px", selectize = FALSE), "select", "Type"),
      shiny::checkboxInput(ns("low_first"), "Lowest score first", value = TRUE),
      actions = shiny::tagList(
        # A plain button that opens the file picker; the picker's text box isn't needed (§2.6).
        shiny::tags$div(class = "mb-import",
                        htmltools::tagQuery(shiny::fileInput(ns("import"), NULL, buttonLabel = "Import…", placeholder = "csv, xlsx or txt",
                                                             accept = c(".csv", ".xlsx", ".txt")))$
                          find("input")$filter(function(x, i) identical(x$attribs$type, "file"))$addAttrs(`aria-label` = "Import requirements from a csv, xlsx or txt file")$allTags()),
        export_menu(export_button(ns("export_reqif"), "ReqIF"), export_button(ns("export_csv"), "CSV"),
                    export_button(ns("export_md"), "Markdown"), export_button(ns("export_pdf"), "PDF")),
        shiny::actionButton(ns("add"), "+ Requirement", class = "mb-btn mb-btn-primary mb-btn-compact")
      )
    ),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "spec.outline", "Outline", width = 260,
                  shiny::tags$div(class = "mb-label", "Outline"),
                  shiny::uiOutput(ns("outline"))),
      shiny::tags$main(class = "mb-main",
                       shiny::uiOutput(ns("triage")),
                       shiny::tags$div(class = "mb-table", reactable::reactableOutput(ns("table"))),
                       shiny::textOutput(ns("count"))),
      # Hidden until a requirement is chosen, so the table has the room (§2.5).
      side_column("right", "spec.inspector", "Details", width = 360, auto = TRUE, shiny::uiOutput(ns("inspector")))
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
    text = reactable::colDef(name = "Requirement", minWidth = 180),
    req_type = reactable::colDef(name = "Type", width = 100, style = list(whiteSpace = "nowrap"), cell = function(value) gap_text(value, "missing")),
    verification_method = reactable::colDef(name = "Verify by", width = 110, style = list(whiteSpace = "nowrap"), cell = function(value) gap_text(value, "missing")),
    allocated_to = reactable::colDef(name = "Allocated to", minWidth = 110, cell = function(value) gap_text(value, "none")),
    status = reactable::colDef(name = "Status", width = 90, style = list(whiteSpace = "nowrap")),
    gate_score = reactable::colDef(name = "Score", width = 64, style = list(whiteSpace = "nowrap"), class = "mono", cell = function(value, index) {
      if (is.na(value)) return("")
      if (value < 50L) shiny::tags$span(class = "mb-gap", style = "font-weight:500", value) else value
    })
  )
}

render_evidence <- function(cases, ns, checks = NULL) {
  shiny::tags$section(
    class = "mb-panel",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Verification"),
                    shiny::tags$button(type = "button", class = "mb-btn mb-btn-small mb-push", onclick = event_js(ns("add_case"), "new"), "Add test case…")),
    if (nrow(cases) == 0L && (is.null(checks) || nrow(checks) == 0L)) shiny::tags$span(class = "mb-caption", "No test cases or assertions yet."),
    # Assertions verify by simulation: checked now, and as last recorded.
    if (!is.null(checks)) lapply(seq_len(nrow(checks)), function(i) {
      check <- checks[i, ]
      shiny::tags$div(
        style = "display:flex;flex-direction:column;gap:4px",
        shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mono", check$id), shiny::tags$span(sprintf("%s · %s", check$machine, check$description))),
        shiny::tags$span(class = if (check$live == "pass") "mb-check-pass" else "mb-check-fail", sprintf("Now: %s · %s", toupper(check$live), check$live_message)),
        shiny::tags$span(class = "mb-caption", if (is.na(check$recorded)) "Not recorded yet. Run and record it on State machines." else sprintf("Recorded %s · %s", toupper(check$recorded), check$run_at))
      )
    }),
    lapply(seq_len(nrow(cases)), function(i) {
      case <- cases[i, ]
      shiny::tags$div(
        style = "display:flex;flex-direction:column;gap:4px",
        shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mono", case$id), shiny::tags$span(case$name),
                        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small mb-push", onclick = event_js(ns("record"), case$id), "Record a result…")),
        shiny::tags$span(class = "mb-caption", if (is.na(case$result)) "No result recorded." else sprintf("%s · %s", toupper(case$result), case$recorded_at)),
        if (!is.na(case$stale)) shiny::tags$span(class = "mb-gap", style = "font-size:12px", sprintf("Stale evidence: %s", case$stale))
      )
    })
  )
}

mod_spec_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$export_reqif <- export_handler(state$con, "requirements", "reqif", export_reqif)
    output$export_csv <- export_handler(state$con, "requirements", "csv", export_csv)
    output$export_md <- export_handler(state$con, "requirements", "md", export_markdown)
    output$export_pdf <- export_handler(state$con, "requirements", "pdf", export_requirements_pdf)
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
    shiny::observe(reveal_column(session, "inspector", !is.null(selected())))

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
        # Two fields a row once the column is wide enough (§2.7).
        shiny::tags$div(class = "mb-field-grid",
                        shiny::textInput(ns("d_title"), "Short title", value(row$short_title)),
                        shiny::selectInput(ns("d_parent"), "Parent", choices = with_blank(parents, "None"), selected = value(row$parent_id), selectize = FALSE),
                        shiny::selectInput(ns("d_type"), "Type", choices = with_blank(REQ_TYPES), selected = value(row$req_type), selectize = FALSE),
                        shiny::selectInput(ns("d_verify"), "Verify by", choices = with_blank(VERIFY_METHODS), selected = value(row$verification_method), selectize = FALSE)),
        shiny::tags$details(class = "mb-fold", open = if (nzchar(value(row$rationale))) NA,
                            shiny::tags$summary("Rationale"),
                            aria_label(shiny::textAreaInput(ns("d_rationale"), NULL, value(row$rationale), rows = 3, width = "100%"), "textarea", "Rationale")),
        shiny::uiOutput(ns("findings")),
        shiny::uiOutput(ns("evidence")),
        shiny::actionButton(ns("rewrite"), "Rewrite it in Write", class = "mb-btn"),
        danger_zone(if (row$status == "draft") {
          confirm_button("Delete draft…", ns("delete_draft"), id, sprintf("Delete draft %s?", id),
                         paste(delete_effects_text(shiny::isolate(delete_effects(con(), "requirement", id)))), confirm = "Delete draft")
        } else {
          shiny::actionButton(ns("deprecate"), "Deprecate…", class = "mb-btn")
        })
      )
    })
    shiny::observeEvent(input$delete_draft, {
      if (!is.null(model_edit(state, delete_draft_requirement(con(), input$delete_draft), "delete draft"))) selected(NULL)
    })
    shiny::observeEvent(input$deprecate, {
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Deprecate %s?", selected()),
        shiny::tags$p("It leaves the specification, the gate counts and Health. Baselines and history keep it, and Undo brings it back.",
                      "Its links stay, but failure modes it handles are no longer counted as handled by it."),
        shiny::textAreaInput(ns("deprecate_reason"), "Why", width = "100%", rows = 2, placeholder = "Superseded by SYS-031"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("deprecate_confirm"), "Deprecate", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$deprecate_confirm, {
      if (is.null(model_edit(state, deprecate_requirement(con(), selected(), input$deprecate_reason), "deprecate"))) return()
      shiny::removeModal()
      selected(NULL)
    })
    output$findings <- shiny::renderUI({
      row <- current()
      shiny::req(row)
      db_dep(state, "model")
      render_findings(requirement_findings(row, model_terms(con())))
    })

    output$evidence <- shiny::renderUI({
      db_dep(state, "model")
      row <- current()
      shiny::req(row)
      db_dep(state, "behavior")
      render_evidence(requirement_evidence(con(), row$id), ns, requirement_assertions(con(), row$id))
    })
    shiny::observeEvent(input$add_case, {
      shiny::showModal(shiny::modalDialog(
        title = "Add a test case",
        shiny::textInput(ns("case_name"), "Name"),
        shiny::selectInput(ns("case_method"), "Method", choices = VERIFY_METHODS, selectize = FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("case_confirm"), "Add test case", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$case_confirm, {
      written <- guarded_edit(add_test_case(con(), selected(), input$case_name, input$case_method))
      if (is.null(written)) return()
      shiny::removeModal()
      invalidate_db(state, c("spec", "model"))
    })
    pending_case <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$record, {
      pending_case(input$record)
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Record a result for %s", input$record),
        shiny::selectInput(ns("record_result"), "Result", choices = c("Pass" = "pass", "Fail" = "fail", "Inconclusive" = "inconclusive"), selectize = FALSE),
        shiny::textInput(ns("record_ref"), "Evidence reference", placeholder = "Report id, file or link"),
        shiny::textAreaInput(ns("record_note"), "Note"),
        shiny::tags$p(class = "mb-caption", "It's stored against the model as it stands now, and goes stale if anything it depended on changes."),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("record_confirm"), "Record result", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$record_confirm, {
      blank <- function(x) if (is.null(x) || !nzchar(trimws(x))) NULL else x
      written <- guarded_edit(write_verification_record(con(), selected(), pending_case(), input$record_result, "engineer",
                                                        evidence_ref = blank(input$record_ref), note = blank(input$record_note)))
      if (is.null(written)) return()
      shiny::removeModal()
      invalidate_db(state, c("spec", "model"))
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
