# "Ask the model", docked on the right (handoff §5.11). It reads through tools and
# can't change the model.

chat_chips <- function(calls) {
  if (length(calls) == 0L) return("")
  chips <- vapply(calls, function(call) sprintf("<span class=\"mb-tool-chip\">%s(%s)</span>", html_escape(call$name), html_escape(call$args)), "")
  paste0("<div class=\"mb-tool-chips\">", paste(chips, collapse = " "), "</div>\n\n")
}

mod_chat_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$aside(
    id = "mb-chat", class = "mb-chat", `aria-label` = "Ask the model",
    shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b("Ask the model"),
                    shiny::tags$button(type = "button", class = "mb-btn mb-btn-small mb-push", `aria-label` = "Close the chat", onclick = "mbToggleChat(false)", "Close")),
    shiny::tags$p(class = "mb-caption", "It reads the model through tools and can't change it."),
    shinychat::chat_ui(ns("chat"), placeholder = "Ask about the model…", fill = TRUE, height = "100%", width = "100%"),
    shiny::uiOutput(ns("actions")),
    shiny::uiOutput(ns("footer"))
  )
}

mod_chat_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    registry <- shiny::reactiveVal(new_chat_registry())
    client <- shiny::reactiveVal(NULL)
    last <- shiny::reactiveVal(NULL)

    # A different project is a new conversation.
    shiny::observeEvent(con(), {
      registry(new_chat_registry())
      client(NULL)
      last(NULL)
    }, ignoreInit = TRUE)

    task <- shiny::ExtendedTask$new(function(connection, reg, question, kept) run_chat_async(connection, reg, question, kept))
    shiny::observeEvent(input$chat_user_input, {
      if (task$status() == "running") return()
      task$invoke(con(), registry(), input$chat_user_input, client())
    })
    shiny::observeEvent(task$result(), {
      result <- task$result()
      if (!is.null(result$client)) client(result$client)
      invalidate_db(state, "ai")
      if (!isTRUE(result$ok)) {
        shinychat::chat_append("chat", if (is.null(result$error)) "The answer didn't come back." else result$error)
        return()
      }
      last(result)
      shinychat::chat_append("chat", paste0(chat_chips(result$calls), result$text))
    })

    output$actions <- shiny::renderUI({
      answer <- last()
      if (is.null(answer)) return(NULL)
      db_dep(state, "ai")
      open <- DBI::dbGetQuery(con(), "SELECT id, title FROM ai_proposal WHERE status = 'open' AND source = 'ai' ORDER BY id")
      shiny::tags$div(
        class = "mb-chat-actions",
        if (length(answer$elements) > 0L) shiny::actionButton(ns("highlight"), sprintf("Highlight on diagram (%d)", length(answer$elements)), class = "mb-btn mb-btn-small"),
        if (nrow(open) > 0L) {
          shiny::tags$div(class = "mb-head",
                          shiny::selectInput(ns("proposal"), NULL, stats::setNames(open$id, sprintf("%s %s", open$id, open$title)), width = "100%", selectize = FALSE),
                          shiny::actionButton(ns("attach"), "Attach to proposal", class = "mb-btn mb-btn-small"))
        }
      )
    })
    shiny::observeEvent(input$highlight, {
      state$selected_entity(list(kind = "element", ids = last()$elements))
      go_to(state, "Model", "Structure")
    })
    shiny::observeEvent(input$attach, {
      answer <- last()
      if (is.null(answer) || is.null(input$proposal)) return()
      guarded_edit(append_proposal_body(con(), input$proposal, sprintf("From the chat: %s", answer$text)))
      invalidate_db(state, "ai")
      shiny::showNotification(sprintf("Attached to %s.", input$proposal), duration = 4)
    })
    output$footer <- shiny::renderUI({
      db_dep(state, "settings")
      answer <- last()
      attempt <- chat_attempt(con())
      routed <- if (!is.null(answer)) {
        paste(answer$provider_id, answer$model, sep = " · ")
      } else if (!is.null(attempt$blocked)) {
        blocked_text(attempt$blocked)
      } else {
        paste(attempt$provider_id, attempt$model, sep = " · ")
      }
      shiny::tags$div(class = "mono mb-caption", style = "font-size:11px", sprintf("via %s · read-only: it can't edit the model", routed))
    })

    invisible(list(task = task, last = last, registry = registry))
  })
}
