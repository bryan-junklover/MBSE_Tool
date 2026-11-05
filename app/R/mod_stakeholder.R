# Model › Stakeholder: the system in plain words (handoff §3, artboard 2). No ids,
# interface names or stereotypes, and every number comes from code.

CHIP_LABELS <- c(not_started = "Not started", in_progress = "In progress", designed = "Designed", decision_needed = "Decision needed", open_question = "Open question")

mod_stakeholder_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, shiny::tags$span(class = "mb-caption", "A plain overview for people outside engineering."),
               actions = shiny::tagList(shiny::actionButton(ns("write"), "Write the summary with AI", class = "mb-btn mb-btn-ai"),
                                        shiny::downloadButton(ns("brief"), "Export as PDF brief", class = "mb-btn", icon = NULL))),
    shiny::tags$div(class = "mb-scroll", shiny::uiOutput(ns("page")))
  )
}

render_stakeholder <- function(overview, go_input, status_line = NULL) {
  shiny::tags$div(
    class = "mb-page-grid mb-reading", style = "max-width:1320px",
    shiny::tags$main(
      style = "flex:999 1 600px;min-width:0;display:flex;flex-direction:column;gap:20px",
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$div(class = "mb-head", shiny::tags$h1(style = "margin:0;font-size:22px;font-weight:600", sprintf("%s in one paragraph", overview$project)),
                                          if (overview$paragraph_is_ai) shiny::tags$span(class = "mb-badge-ai", "AI-drafted")),
                          shiny::tags$p(style = "margin:0;max-width:72ch;line-height:1.55;font-size:15px", if (nzchar(overview$paragraph)) overview$paragraph else "No scope has been written yet."),
                          status_line),
      shiny::tags$section(style = "display:flex;flex-direction:column;gap:12px",
                          shiny::tags$h2(style = "margin:0;font-size:16px;font-weight:600", "The main parts"),
                          shiny::tags$div(class = "mb-parts", lapply(seq_len(nrow(overview$parts)), function(i) {
                            part <- overview$parts[i, ]
                            shiny::tags$div(class = "mb-panel", shiny::tags$b(part$name),
                                            shiny::tags$span(style = "line-height:1.45", if (is.na(part$plain_summary)) "No plain summary yet." else part$plain_summary),
                                            shiny::tags$span(class = paste("mb-status-chip", paste0("chip-", part$status)), CHIP_LABELS[[part$status]]))
                          })))
    ),
    shiny::tags$aside(
      style = "flex:1 1 340px;display:flex;flex-direction:column;gap:20px",
      shiny::tags$section(class = "mb-panel", style = "gap:16px",
                          shiny::tags$h2(style = "margin:0;font-size:16px;font-weight:600", "Are we on track?"),
                          lapply(overview$meters, function(meter) {
                            shiny::tags$div(style = "display:flex;flex-direction:column;gap:6px",
                                            shiny::tags$div(class = "mb-head", shiny::tags$span(meter$label), shiny::tags$span(class = "mono mb-push", style = "font-size:13px", meter$value_text)),
                                            shiny::tags$div(class = "mb-meter-track", role = "img", `aria-label` = sprintf("%s: %s", meter$label, meter$value_text),
                                                            shiny::tags$div(class = paste("mb-meter-fill", if (!meter$pass) "fail"), style = sprintf("width:%d%%", round(meter$fraction * 100)))),
                                            shiny::tags$span(class = if (meter$pass) "mb-caption" else "mb-gap", style = "font-size:12px", meter$line))
                          })),
      shiny::tags$section(class = "mb-panel",
                          shiny::tags$h2(style = "margin:0;font-size:16px;font-weight:600", "Open questions, in plain terms"),
                          if (length(overview$questions) == 0L) shiny::tags$span(class = "mb-caption", "None right now."),
                          shiny::tags$ol(style = "margin:0;padding-left:20px;display:flex;flex-direction:column;gap:8px;line-height:1.45", lapply(overview$questions, shiny::tags$li)),
                          shiny::tags$button(type = "button", class = "mb-linkish", style = "align-self:flex-start", onclick = event_js(go_input, "detail"), "See the engineering detail"))
    )
  )
}

mod_stakeholder_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    message <- shiny::reactiveVal(NULL)
    overview <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      db_dep(state, "ai")
      db_dep(state, "settings")
      stakeholder_overview(con())
    })
    task <- shiny::ExtendedTask$new(function(connection) run_stakeholder_async(connection))
    shiny::observeEvent(input$write, {
      message(NULL)
      task$invoke(con())
    })
    shiny::observeEvent(task$result(), {
      result <- task$result()
      message(if (isTRUE(result$ok)) NULL else result$error)
      invalidate_db(state, c("ai", "model"))
    })
    output$page <- shiny::renderUI({
      line <- if (task$status() == "running") shiny::tags$span(class = "mb-caption", "Checking…") else if (!is.null(message())) shiny::tags$span(message())
      render_stakeholder(overview(), ns("go"), line)
    })
    shiny::observeEvent(input$go, go_to(state, "Analyze", "Review queue"))
    output$brief <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", overview()$project), "_brief.pdf"),
      content = function(file) {
        outcome <- tryCatch(write_brief_pdf(con(), file), error = function(e) e)
        if (inherits(outcome, "error")) {
          refusal_toast(conditionMessage(outcome))
          stop(conditionMessage(outcome))
        }
      }
    )
    invisible(list(overview = overview))
  })
}
