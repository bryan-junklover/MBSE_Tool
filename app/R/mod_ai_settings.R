# Project › AI: providers, task routing, coaching policy and data protection (handoff §5.12).

TASK_LABELS <- c(req_light_check = "Requirement light check", req_deep_check = "Requirement deep check", scope_gaps = "Gaps the scope implies",
                 trace_coach = "Trace coach", gap_inference = "Gap inference", allocation_coach = "Allocation coach", impact_narrative = "Change impact narrative",
                 behavior_sketch = "Behavior sketch", behavior_analysis = "Behavior analysis", stakeholder_summary = "Stakeholder summaries", model_chat = "Model Q&A chat")
TASK_SENDS <- c(req_light_check = "one requirement, its parent, related requirements", req_deep_check = "one requirement, up to 40 related, the scope", scope_gaps = "scope, model names, committed requirements",
                trace_coach = "one requirement and numbered candidates", gap_inference = "scope, model names, committed requirements", allocation_coach = "one function and numbered candidates",
                impact_narrative = "the diff and impact set", behavior_sketch = "a description and model names", behavior_analysis = "a scenario and its interfaces",
                stakeholder_summary = "view contents", model_chat = "what it asks for through tools")

POLICY_ROWS <- list(
  c("Requirement wording", "Ask questions; show example rewrites, folded, as word diffs", "Every word committed", "No way to apply a rewrite; the gate re-checks every commit"),
  c("Values", "Point out that one is missing", "Every number", "The AI writes [value]; placeholders block a baseline"),
  c("Links", "Rank numbered candidates, argue as questions", "Choosing the link, plus a note", "A note is required; the model never names an id"),
  c("Structure", "Sketch unreviewed drafts", "Reviewing each item, with a note", "Drafts block a baseline until each has your note"),
  c("Explanations", "Write freely", "Decisions; numbers come from code", "Numbers come from code, never the model")
)

mod_ai_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$section(class = "mb-panel", shiny::tags$b("AI providers"), shiny::uiOutput(ns("providers"))),
    shiny::tags$section(class = "mb-panel",
                        shiny::tags$div(class = "mb-head", shiny::tags$b("Task routing"), shiny::tags$span(class = "mb-caption", "Each AI task picks its own provider and model, with a fallback if the first is unavailable.")),
                        shiny::uiOutput(ns("routes"))),
    shiny::tags$section(class = "mb-panel",
                        shiny::tags$div(class = "mb-head", shiny::tags$b("Coaching policy"), shiny::tags$span(class = "mb-caption", "Tiered. Each rule is enforced by code, not by the prompt.")),
                        shiny::tags$div(class = "mb-table", shiny::tags$table(
                          class = "mb-simple",
                          shiny::tags$thead(shiny::tags$tr(lapply(c("Kind", "The AI may", "Always yours", "Enforced by"), shiny::tags$th))),
                          shiny::tags$tbody(lapply(POLICY_ROWS, function(row) shiny::tags$tr(lapply(row, shiny::tags$td))))
                        ))),
    shiny::tags$section(class = "mb-panel",
                        shiny::tags$b("Data protection"),
                        shiny::checkboxInput(ns("local_only"), "Local only: route every task to the local provider, and block cloud calls"),
                        shiny::textAreaInput(ns("redact"), "Redact before cloud calls (one term per line)", rows = 2, width = "100%"),
                        shiny::textInput(ns("spend_cap"), "Daily cloud spend cap, in US dollars", placeholder = "no cap"),
                        shiny::textInput(ns("coach_wait"), "Coach wait after typing stops, in milliseconds"),
                        shiny::tags$span(class = "mb-caption", "Every prompt and response is logged to this project. That can't be turned off in this version."))
  )
}

mod_ai_settings_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    refresh <- shiny::reactiveVal(0L)
    test_results <- shiny::reactiveVal(list())

    shiny::observe({
      connection <- con()
      shiny::updateCheckboxInput(session, "local_only", value = local_only(connection))
      terms <- redact_terms(connection)
      shiny::updateTextAreaInput(session, "redact", value = paste(terms, collapse = "\n"))
      shiny::updateTextInput(session, "spend_cap", value = get_setting(connection, "spend_cap_usd", ""))
      shiny::updateTextInput(session, "coach_wait", value = get_setting(connection, "coach_wait_ms", "2000"))
    })
    shiny::observeEvent(input$local_only, {
      value <- if (isTRUE(input$local_only)) "1" else "0"
      if (!identical(value, get_setting(con(), "local_only", "0"))) {
        set_setting(con(), "local_only", value)
        invalidate_db(state, c("settings", "ai"))
      }
    }, ignoreInit = TRUE)
    redact_debounced <- shiny::debounce(shiny::reactive(input$redact), 500)
    shiny::observeEvent(redact_debounced(), {
      if (!identical(paste(redact_terms(con()), collapse = "\n"), redact_debounced())) set_redact_terms(con(), redact_debounced())
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$spend_cap, {
      value <- trimws(input$spend_cap)
      if (nzchar(value) && is.na(suppressWarnings(as.numeric(value)))) return(refusal_toast("The spend cap has to be a number."))
      if (!identical(value, get_setting(con(), "spend_cap_usd", ""))) set_setting(con(), "spend_cap_usd", value)
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$coach_wait, {
      value <- suppressWarnings(as.numeric(input$coach_wait))
      if (is.na(value) || value < 300) return(refusal_toast("The coach wait has to be at least 300 milliseconds."))
      if (!identical(as.character(value), get_setting(con(), "coach_wait_ms", "2000"))) {
        set_setting(con(), "coach_wait_ms", as.character(value))
        invalidate_db(state, "settings")
      }
    }, ignoreInit = TRUE)

    providers <- shiny::reactive({
      refresh()
      db_dep(state, "settings")
      DBI::dbGetQuery(con(), "SELECT * FROM provider ORDER BY is_local, label")
    })
    output$providers <- shiny::renderUI({
      rows <- providers()
      results <- test_results()
      shiny::tags$div(class = "mb-provider-grid", lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, ]
        if (row$is_local == 1L) {
          server <- local_server_models(row$base_url)
          chip <- if (server$reachable) sprintf("Reachable · %d models loaded", length(server$models)) else "Not reachable"
          return(shiny::tags$div(class = "mb-panel",
                                 shiny::tags$div(class = "mb-head", shiny::tags$b(row$label), shiny::tags$span(class = paste("mb-chip mb-push", if (!server$reachable) "mb-gap"), chip)),
                                 labelled("Base URL", shiny::tags$input(type = "text", class = "mb-table-input mono", value = row$base_url, onchange = change_js(ns("base_url"), list(id = row$id)))),
                                 shiny::tags$span(class = "mb-caption", "Local · no data leaves the machine"),
                                 shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("recheck"), row$id), "Check again"))))
        }
        connected <- provider_has_key(row$id)
        tested <- results[[row$id]]
        shiny::tags$div(class = "mb-panel",
                        shiny::tags$div(class = "mb-head", shiny::tags$b(row$label),
                                        shiny::tags$span(class = paste("mb-chip mb-push", if (identical(tested$ok, TRUE)) "mb-chip-on"),
                                                         if (identical(tested$ok, TRUE)) "Connected" else if (connected) "Key set, not tested" else "No key")),
                        labelled("API key", shiny::tags$input(type = "password", class = "mb-table-input", autocomplete = "off", placeholder = if (connected) "Stored" else "Paste a key",
                                                              onchange = paste0(change_js(ns("api_key"), list(id = row$id)), "; this.value = ''"))),
                        shiny::tags$span(class = "mb-caption", "Cloud · the key goes to the OS keyring, never the project"),
                        shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("test"), row$id), "Test the connection")),
                        if (!is.null(tested) && !isTRUE(tested$ok)) shiny::tags$span(class = "mb-gap", tested$text))
      }))
    })
    shiny::observeEvent(input$recheck, refresh(refresh() + 1L))
    test_task <- shiny::ExtendedTask$new(function(connection, provider_id) test_provider_async(connection, provider_id))
    shiny::observeEvent(input$test, test_task$invoke(con(), input$test))
    shiny::observeEvent(test_task$result(), {
      result <- test_task$result()
      known <- test_results()
      known[[result$provider_id]] <- result
      test_results(known)
    })
    shiny::observeEvent(input$base_url, {
      value <- trimws(input$base_url$value)
      if (!nzchar(value)) return(refusal_toast("A local provider needs a base URL."))
      set_provider_base_url(con(), input$base_url$id, value)
      refresh(refresh() + 1L)
      invalidate_db(state, "ai")
    })
    shiny::observeEvent(input$api_key, {
      value <- trimws(input$api_key$value)
      if (!nzchar(value)) return()
      saved <- guarded_edit(store_provider_key(input$api_key$id, value))
      if (!is.null(saved)) {
        refresh(refresh() + 1L)
        invalidate_db(state, "ai")
      }
    })

    output$routes <- shiny::renderUI({
      db_dep(state, "settings")
      refresh()
      routes <- routes_table(con())
      rows <- providers()
      choices <- stats::setNames(rows$id, rows$label)
      local <- rows[rows$is_local == 1L, , drop = FALSE]
      loaded <- if (nrow(local) > 0L) local_server_models(local$base_url[1])$models else character(0)
      shiny::tags$div(class = "mb-table", shiny::tags$table(
        class = "mb-simple",
        shiny::tags$thead(shiny::tags$tr(lapply(c("Task", "Runs", "Provider", "Model", "Fallback", "Fallback model", "Sends"), shiny::tags$th))),
        shiny::tags$tbody(lapply(seq_len(nrow(routes)), function(i) {
          route <- routes[i, ]
          cell <- function(field) change_js(ns("route_change"), list(task = route$task, field = field))
          text <- function(x) if (is.na(x)) "" else x
          is_check <- route$task %in% c("req_light_check", "req_deep_check")
          shiny::tags$tr(
            shiny::tags$td(shiny::tags$b(TASK_LABELS[[route$task]])),
            shiny::tags$td(if (is_check) {
              select_tag(c("Every edit" = "every_edit", "On demand" = "on_demand"), route$trigger_mode, cell("trigger_mode")) |> htmltools::tagAppendAttributes(class = "mb-table-input", `aria-label` = "Runs")
            } else {
              gsub("_", " ", route$trigger_mode)
            }),
            shiny::tags$td(select_tag(choices, text(route$provider_id), cell("provider_id")) |> htmltools::tagAppendAttributes(class = "mb-table-input", `aria-label` = "Provider")),
            shiny::tags$td(shiny::tags$input(type = "text", class = "mb-table-input mono", value = text(route$model), placeholder = "Choose a model", `aria-label` = "Model",
                                             list = ns("loaded_models"), onchange = cell("model"))),
            shiny::tags$td(select_tag(choices, text(route$fallback_provider_id), cell("fallback_provider_id"), "None") |> htmltools::tagAppendAttributes(class = "mb-table-input", `aria-label` = "Fallback")),
            shiny::tags$td(shiny::tags$input(type = "text", class = "mb-table-input mono", value = text(route$fallback_model), `aria-label` = "Fallback model",
                                             list = ns("loaded_models"), onchange = cell("fallback_model"))),
            shiny::tags$td(class = "mb-caption", TASK_SENDS[[route$task]])
          )
        }))
      ), shiny::tags$datalist(id = ns("loaded_models"), lapply(loaded, function(model) shiny::tags$option(value = model))))
    })
    shiny::observeEvent(input$route_change, {
      change <- input$route_change
      value <- if (nzchar(trimws(change$value))) trimws(change$value) else NA_character_
      allowed <- c("trigger_mode", "provider_id", "model", "fallback_provider_id", "fallback_model")
      shiny::req(change$field %in% allowed)
      set_task_route(con(), change$task, stats::setNames(list(value), change$field))
      invalidate_db(state, c("settings", "ai"))
    })
  })
}
