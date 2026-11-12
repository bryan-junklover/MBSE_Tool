# Write › Traceability: requirements × components, and functions × components.

mod_trace_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, shiny::uiOutput(ns("tabs"))),
    shiny::tags$div(
      class = "mb-body",
      shiny::tags$main(class = "mb-main mb-matrix-main", shiny::uiOutput(ns("coverage")),
                       shiny::uiOutput(ns("matrix"), class = "mb-matrix-scroll"),
                       shiny::tags$div(class = "mb-legend", shiny::tags$span("■ linked"), shiny::tags$span("▪ linked to a part of it"),
                                       shiny::tags$span(class = "mb-gap", "shaded amber = gap"), shiny::tags$span("shaded grey = draft"),
                                       shiny::tags$span(style = "color:var(--ai-text)", "dashed = AI candidate, not yet chosen"))),
      # Opens when a row is chosen, so the matrix has the width until then (§3).
      side_column("right", "trace.side", "Allocation", width = 360, auto = TRUE, shiny::uiOutput(ns("side")))
    )
  )
}

# Column heads and cells are real buttons. A cell says what it holds in its label, not only its look.
render_trace_table <- function(rows, columns, cell_of, row_class, row_label, select_input, selected) {
  # Heads are turned 45°, so a long name takes height, not width; the head row and the
  # first column stay in view while the matrix scrolls (UX review §2.8).
  shiny::tags$div(class = "mb-table mb-matrix-wrap", shiny::tags$table(
    class = "mb-simple mb-matrix",
    shiny::tags$thead(shiny::tags$tr(
      # Each turned name leans over the heads to its right, and sticky heads are opaque, so
      # every head sits above the ones after it, and the corner above them all.
      shiny::tags$th(class = "mb-matrix-corner", style = sprintf("z-index:%d", nrow(columns) + 4L), "Requirement"),
      lapply(seq_len(nrow(columns)), function(j) {
        shiny::tags$th(class = paste("mb-rot", if (isTRUE(columns$empty[j])) "mb-gap"), title = columns$name[j],
                       style = sprintf("z-index:%d", nrow(columns) - j + 3L),
                       shiny::tags$div(shiny::tags$span(columns$name[j])))
      })
    )),
    shiny::tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
      shiny::tags$tr(class = paste(row_class(rows[i, ]), if (identical(rows$id[i], selected)) "mb-row-selected"),
                     shiny::tags$td(style = "text-align:left", shiny::tags$button(type = "button", class = "mb-linkish", style = "color:inherit;text-decoration:none",
                                                                                  onclick = event_js(select_input, rows$id[i]), row_label(rows[i, ]))),
                     lapply(seq_len(nrow(columns)), function(j) shiny::tags$td(cell_of(rows[i, ], columns[j, ]))))
    }))
  ))
}

matrix_cell <- function(label, state_kind, onclick) {
  mark <- switch(state_kind, direct = "■", indirect = "▪", candidate = "AI?", "")
  shiny::tags$button(type = "button", class = paste("mb-cell", paste0("mb-cell-", state_kind)), `aria-label` = label, title = label,
                     disabled = if (state_kind == "indirect") NA, onclick = onclick, mark)
}

mod_trace_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    tab <- shiny::reactiveVal("requirements")
    selected <- shiny::reactiveVal(NULL)
    pending <- shiny::reactiveVal(NULL)
    coach_message <- shiny::reactiveVal(NULL)

    output$tabs <- shiny::renderUI({
      button <- function(value, label) {
        on <- tolower(as.character(tab() == value))
        shiny::tags$button(type = "button", role = "tab", `aria-pressed` = on, `aria-selected` = on, onclick = event_js(ns("set_tab"), value), label)
      }
      shiny::tags$div(role = "tablist", `aria-label` = "Matrix", class = "mb-segmented",
                      button("requirements", "Requirements × components"), button("functions", "Functional → physical allocation"))
    })
    shiny::observeEvent(input$set_tab, {
      tab(input$set_tab)
      selected(NULL)
    })

    matrix_data <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      trace_matrix(con())
    })
    allocation_data <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      allocation_matrix(con())
    })
    open_links <- shiny::reactive({
      db_dep(state, "ai")
      rows <- proposal_list(con(), "open", "ai")
      rows[rows$task %in% c("trace_coach", "allocation_coach"), , drop = FALSE]
    })

    candidate_on <- function(target, column) {
      rows <- open_links()
      rows <- rows[rows$target_id %in% target & !is.na(rows$candidates_json), , drop = FALSE]
      any(vapply(rows$candidates_json, function(json) column %in% jsonlite::fromJSON(json)$id, TRUE))
    }

    output$matrix <- shiny::renderUI({
      if (tab() == "requirements") {
        data <- matrix_data()
        cell <- function(row, column) {
          link <- data$links[data$links$source_id == row$id & data$links$column %in% column$id, , drop = FALSE]
          direct <- link[link$target_id == column$id, , drop = FALSE]
          kind <- if (nrow(direct) > 0L) "direct" else if (nrow(link) > 0L) "indirect" else if (candidate_on(row$id, column$id)) "candidate" else "empty"
          label <- sprintf("%s and %s: %s", row$id, column$name, switch(kind, direct = "linked", indirect = "linked to a part of it", candidate = "AI candidate", "not linked"))
          onclick <- if (kind == "direct") event_js(ns("unlink"), direct$id[1]) else event_js(ns("ask_link"), list(source = row$id, target = column$id, name = column$name, kind = "requirement"))
          matrix_cell(label, kind, onclick)
        }
        render_trace_table(data$rows, data$columns, cell,
                           function(row) if (row$status == "draft") "mb-draft-row" else if (row$unallocated) "mb-gap-row",
                           function(row) {
                             shiny::tagList(shiny::tags$span(class = "mono", row$id), " ", if (!is.na(row$short_title)) row$short_title,
                                            if (row$unallocated) shiny::tags$b(class = "mb-gap", " · unallocated"), if (row$status == "draft") " · draft")
                           },
                           ns("select"), selected())
      } else {
        data <- allocation_data()
        cell <- function(row, column) {
          link <- data$links[data$links$source_id == row$id & data$links$target_id == column$id, , drop = FALSE]
          kind <- if (nrow(link) > 0L) "direct" else if (candidate_on(row$id, column$id)) "candidate" else "empty"
          label <- sprintf("%s and %s: %s", row$id, column$name, switch(kind, direct = "allocated", candidate = "AI candidate", "not allocated"))
          onclick <- if (kind == "direct") event_js(ns("unlink"), link$id[1]) else event_js(ns("ask_link"), list(source = row$id, target = column$id, name = column$name, kind = "function"))
          matrix_cell(label, kind, onclick)
        }
        render_trace_table(data$rows, data$columns, cell, function(row) if (row$unallocated) "mb-gap-row",
                           function(row) shiny::tagList(shiny::tags$span(class = "mono", row$id), " ", row$name, if (row$unallocated) shiny::tags$b(class = "mb-gap", " · unallocated")),
                           ns("select"), selected())
      }
    })

    output$coverage <- shiny::renderUI({
      item <- function(label, a, b) shiny::tags$span(class = if (a < b) "mb-gap", sprintf("%s %d / %d", label, a, b))
      line <- if (tab() != "requirements") {
        data <- allocation_data()
        list(item("Functions allocated", sum(!data$rows$unallocated), nrow(data$rows)))
      } else {
        coverage <- matrix_data()$coverage
        list(item("Requirements allocated", coverage$allocated, coverage$allocatable),
             item("Components with requirements", coverage$components, coverage$component_total),
             item("With a verification method", coverage$verified, coverage$allocatable))
      }
      shiny::tags$div(class = "mb-facts", shiny::tags$b(style = "font-family:var(--font-sans)", "Coverage"), line,
                      shiny::tags$span(class = "mb-caption", "counted from the model · select a row to ask the AI · click a cell to link or unlink"))
    })

    coach_task <- shiny::ExtendedTask$new(function(connection, kind, target) run_link_coach_async(connection, kind, target))
    output$side <- shiny::renderUI({
      target <- selected()
      if (is.null(target)) return(shiny::tags$p(class = "mb-caption", "Select a row to ask the AI about it. Click a cell to link or unlink by hand."))
      cards <- open_links()
      cards <- cards[cards$target_id %in% target, , drop = FALSE]
      shiny::tagList(
        shiny::tags$section(class = "mb-sketch-panel",
                            ai_header(if (tab() == "requirements") "Allocation question" else "Allocation question", "on demand"),
                            shiny::tags$span(class = "mono", target),
                            shiny::tags$div(shiny::actionButton(ns("ask_coach"), "Ask AI about this", class = "mb-btn")),
                            if (coach_task$status() == "running") shiny::tags$span(class = "mb-caption", "Checking…"),
                            if (!is.null(coach_message())) shiny::tags$span(coach_message())),
        lapply(seq_len(nrow(cards)), function(i) render_proposal_card(cards[i, ], ns))
      )
    })
    shiny::observeEvent(input$select, selected(input$select))
    shiny::observe(reveal_column(session, "side", !is.null(selected())))
    shiny::observeEvent(input$ask_coach, {
      coach_message(NULL)
      coach_task$invoke(con(), if (tab() == "requirements") "requirement" else "element", selected())
    })
    shiny::observeEvent(coach_task$result(), {
      result <- coach_task$result()
      coach_message(if (isTRUE(result$ok)) NULL else result$error)
      invalidate_db(state, "ai")
    })

    # By hand a rationale is optional, so the dialog says so.
    shiny::observeEvent(input$ask_link, {
      pending(input$ask_link)
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Link %s to %s", input$ask_link$source, input$ask_link$name),
        shiny::textInput(ns("link_note"), "Rationale (optional)", width = "100%"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("link_confirm"), sprintf("Link to %s", input$ask_link$name), class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$link_confirm, {
      target <- pending()
      shiny::req(target)
      shiny::removeModal()
      model_edit(state, if (identical(target$kind, "requirement")) {
        link_requirement(con(), target$source, target$target, input$link_note)
      } else {
        allocate_function(con(), target$source, target$target, input$link_note)
      }, "link")
      pending(NULL)
    })
    shiny::observeEvent(input$unlink, model_edit(state, delete_entity(con(), "link", input$unlink), "unlink"))
    shiny::observeEvent(input$keep_link, {
      written <- model_edit(state, keep_candidate(con(), input$keep_link$id, input$keep_link$handle, input$keep_link$note), "link")
      if (!is.null(written)) invalidate_db(state, "ai")
    })
    pending_dismiss <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$dismiss, {
      pending_dismiss(input$dismiss)
      ask_dismiss(ns, input$dismiss, "dismiss_confirm", "dismiss_reason")
    })
    shiny::observeEvent(input$dismiss_confirm, {
      done <- guarded_edit({
        dismiss_proposal(con(), pending_dismiss(), input$dismiss_reason)
        TRUE
      })
      if (!is.null(done)) {
        shiny::removeModal()
        invalidate_db(state, "ai")
      }
    })
    invisible(list(matrix = matrix_data, allocation = allocation_data, selected = selected, open_links = open_links, tab = tab))
  })
}
