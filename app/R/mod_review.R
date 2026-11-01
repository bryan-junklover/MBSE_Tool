# Analyze › Review queue: structural gaps from code and AI proposals (handoff §5.3).

mod_review_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "mb-body",
    shiny::tags$aside(class = "mb-left",
                      shiny::checkboxGroupInput(ns("sources"), "Source", choices = c("Structural check" = "structural", "AI-inferred" = "ai"), selected = c("structural", "ai")),
                      shiny::radioButtons(ns("status"), "Status", choices = c("Open" = "open", "Kept" = "kept", "Dismissed" = "dismissed"))),
    shiny::tags$main(class = "mb-main",
                     shiny::tags$div(class = "mb-head", shiny::tags$h1(style = "margin:0;font-size:20px;font-weight:600", "Review queue"),
                                     shiny::tags$span(class = "mb-caption", "The AI asks; it may sketch structure as unreviewed drafts. Values and links stay yours, and nothing reaches a baseline without your note.")),
                     shiny::uiOutput(ns("cards"))),
    shiny::tags$aside(class = "mb-right",
                      shiny::tags$b("Run analysis"),
                      shiny::tags$span(class = "mb-caption", "Structural checks run by code, with no AI, every time this page refreshes."),
                      shiny::actionButton(ns("run"), "Ask AI about gaps", class = "mb-btn mb-btn-ai"),
                      shiny::uiOutput(ns("run_status")),
                      shiny::uiOutput(ns("provenance")))
  )
}

render_proposal_card <- function(row, ns) {
  payload <- jsonlite::fromJSON(row$payload_json, simplifyVector = FALSE)
  open <- row$status == "open"
  structural <- row$source == "structural"
  meta <- paste(c(gsub("_", " ", row$proposal_kind), if (!is.na(row$confidence)) paste("confidence", row$confidence)), collapse = " · ")
  dismiss <- if (open) shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("dismiss"), row$id), "Dismiss with reason")
  shiny::tags$article(
    class = paste("mb-panel", if (!structural) "mb-card-ai"),
    shiny::tags$div(class = "mb-head", shiny::tags$span(class = paste("mb-tag", if (!structural) "mb-tag-ai"), if (structural) "STRUCTURAL" else "AI-INFERRED"),
                    shiny::tags$span(class = "mb-caption", meta)),
    shiny::tags$b(row$title),
    if (!is.na(row$body) && nzchar(row$body)) shiny::tags$div(class = "mb-caption", row$body),
    if (!is.null(payload$`_warning`)) shiny::tags$div(class = "mb-gap", style = "font-size:12px", payload$`_warning`),
    if (!is.null(payload$`_invented`)) shiny::tags$div(class = "mb-caption", sprintf("%d value%s the model tried to invent became [value].", payload$`_invented`, if (payload$`_invented` == 1L) "" else "s")),
    if (row$proposal_kind %in% c("sketch_element", "sketch_function")) {
      ports <- vapply(payload$ports, function(port) sprintf("%s %s", port$direction, port$interface), "")
      shiny::tagList(
        shiny::tags$div(class = "mb-sketch-box",
                        shiny::tags$div(class = "mono mb-caption", "Sketch · structure only, no values"),
                        shiny::tags$b(payload$name), if (nzchar(payload$stereotype %||% "")) sprintf(" «%s»", payload$stereotype),
                        if (length(ports) > 0L) paste(" · ports:", paste(ports, collapse = ", "))),
        if (open) {
          note <- ns(paste0("note_", row$id))
          confirm <- ns("keep")
          shiny::tagList(
            shiny::tags$label(style = "display:flex;flex-direction:column;gap:4px;font-size:13px", "Your note: why the system needs this (required)",
                              shiny::tags$input(type = "text", id = note, class = "mb-table-input", placeholder = "In your own words",
                                                oninput = sprintf("document.getElementById('%s').disabled = !this.value.trim()", paste0(note, "_go")))),
            shiny::tags$div(class = "mb-term-actions",
                            shiny::tags$button(type = "button", id = paste0(note, "_go"), class = "mb-btn mb-btn-ai", disabled = NA,
                                               onclick = sprintf("Shiny.setInputValue('%s', {id: %s, note: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})",
                                                                 confirm, jsonlite::toJSON(row$id, auto_unbox = TRUE), note),
                                               "Add as unreviewed draft"),
                            dismiss)
          )
        }
      )
    } else if (row$proposal_kind == "sketch_requirement") {
      shiny::tagList(
        shiny::tags$div(class = "mb-sketch-box",
                        shiny::tags$div(class = "mono mb-caption", "Starting draft · the gate re-checks it"),
                        payload$draft),
        if (open) shiny::tags$div(class = "mb-term-actions",
                                  shiny::tags$button(type = "button", class = "mb-btn mb-btn-ai", onclick = event_js(ns("start"), row$id), "Start from this in Write"),
                                  dismiss)
      )
    } else if (structural) {
      if (open) {
        shiny::tags$div(class = "mb-term-actions",
                        shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("show"), list(kind = row$target_kind, id = row$target_id)), "Show on diagram"),
                        shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("ask"), row$title), "Ask AI to draft…"),
                        dismiss)
      }
    } else if (open) {
      shiny::tags$div(class = "mb-term-actions", dismiss)
    },
    if (!open && !is.na(row$engineer_note)) shiny::tags$div(class = "mb-caption", sprintf("Your note: %s", row$engineer_note)),
    if (!open && !is.na(row$dismiss_reason)) shiny::tags$div(class = "mb-caption", sprintf("Reason: %s", row$dismiss_reason)),
    if (!structural && !is.na(row$prompt_version)) {
      shiny::tags$div(class = "mono mb-caption", style = "font-size:11px", sprintf("%s · %s · %s", row$provider_id, row$model, row$prompt_version), " ",
                      shiny::tags$button(type = "button", class = "mb-linkish", onclick = event_js(ns("see_call"), row$llm_call_id), "See the prompt"))
    }
  )
}

mod_review_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    pending_dismiss <- shiny::reactiveVal(NULL)
    run_message <- shiny::reactiveVal(NULL)
    gaps_task <- shiny::ExtendedTask$new(function(connection, focus) run_gaps_async(connection, "gap_inference", focus))

    proposals <- shiny::reactive({
      db_dep(state, "ai")
      db_dep(state, "model")
      db_dep(state, "spec")
      refresh_structural_gaps(con())
      sources <- if (is.null(input$sources)) character(0) else input$sources
      if (length(sources) == 0L) return(proposal_list(con(), "open", "none"))
      proposal_list(con(), if (is.null(input$status)) "open" else input$status, sources)
    })
    output$cards <- shiny::renderUI({
      rows <- proposals()
      if (nrow(rows) == 0L) return(shiny::tags$p(class = "mb-caption", "Nothing here."))
      lapply(seq_len(nrow(rows)), function(i) render_proposal_card(rows[i, ], ns))
    })

    shiny::observeEvent(input$keep, {
      written <- guarded_edit(keep_sketch(con(), input$keep$id, input$keep$note))
      if (is.null(written)) return()
      state$commands(push_command(state$commands(), change_command(written$change_set, "add sketch")))
      invalidate_db(state, c("ai", "model"))
    })
    shiny::observeEvent(input$start, {
      draft <- guarded_edit(start_from_sketch(con(), input$start))
      if (is.null(draft)) return()
      invalidate_db(state, "ai")
      state$open_in_write(list(text = draft))
      go_to(state, "Write", "Draft")
    })
    shiny::observeEvent(input$dismiss, {
      pending_dismiss(input$dismiss)
      ask_dismiss(ns, input$dismiss, "dismiss_confirm", "dismiss_reason")
    })
    shiny::observeEvent(input$dismiss_confirm, {
      done <- guarded_edit({
        dismiss_proposal(con(), pending_dismiss(), input$dismiss_reason)
        TRUE
      })
      if (is.null(done)) return()
      shiny::removeModal()
      invalidate_db(state, "ai")
    })
    shiny::observeEvent(input$show, {
      target <- input$show
      element <- if (target$kind == "port") read_entity(con(), "port", target$id)$element_id else target$id
      state$selected_entity(list(kind = "element", ids = element))
      go_to(state, "Model", "Structure")
    })

    shiny::observeEvent(input$run, {
      run_message(NULL)
      gaps_task$invoke(con(), NULL)
    })
    shiny::observeEvent(input$ask, {
      run_message(NULL)
      gaps_task$invoke(con(), input$ask)
    })
    shiny::observeEvent(gaps_task$result(), {
      result <- gaps_task$result()
      run_message(if (isTRUE(result$ok)) sprintf("%d new item%s.", length(result$proposal_ids), if (length(result$proposal_ids) == 1L) "" else "s") else result$error)
      invalidate_db(state, "ai")
    })
    output$run_status <- shiny::renderUI({
      if (gaps_task$status() == "running") return(shiny::tags$span(class = "mb-caption", "Checking…"))
      if (!is.null(run_message())) shiny::tags$span(run_message())
    })
    output$provenance <- shiny::renderUI({
      shiny::tags$span(class = "mb-caption", "Each kept item is stored with the model, prompt version and your note, and stays marked as AI-originated.")
    })
    shiny::observeEvent(input$see_call, {
      call <- DBI::dbGetQuery(con(), "SELECT system_prompt, user_message, schema_json FROM llm_call WHERE id = ?", params = list(input$see_call))
      shiny::req(nrow(call) == 1L)
      shiny::showModal(shiny::modalDialog(title = "See the prompt", size = "l", easyClose = TRUE,
                                          shiny::tags$pre(style = "white-space:pre-wrap;font-size:12px",
                                                          paste0("SYSTEM\n", call$system_prompt, "\n\nMESSAGE\n", call$user_message, "\n\nANSWER SCHEMA\n", call$schema_json)),
                                          footer = shiny::modalButton("Close")))
    })
    invisible(list(proposals = proposals, task = gaps_task))
  })
}
