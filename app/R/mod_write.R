# Write › Draft: one requirement through the gate, terms and commit. The coach joins
# in Phase 3; with none configured, commit runs on the gate alone (BUILD_PLAN §5).

REQ_TYPES <- c("Functional" = "functional", "Performance" = "performance", "Physical" = "physical",
               "Interface" = "interface", "Non-functional" = "nfr", "Constraint" = "constraint")
REQ_LEVELS <- c("System" = "system", "Stakeholder" = "stakeholder", "Subsystem" = "subsystem")
VERIFY_METHODS <- c("Test" = "test", "Analysis" = "analysis", "Inspection" = "inspection",
                    "Demonstration" = "demonstration", "Simulation" = "simulation")

with_blank <- function(choices, label = "Not set") c(stats::setNames("", label), choices)

# Shiny holds text input back 250 ms, so at the moment of a click the server can
# still have older words. The click carries what's on screen, and that's what is saved.
send_draft_js <- function(request_input, text_input) {
  sprintf("Shiny.setInputValue('%s', {text: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})",
          request_input, text_input)
}

mod_write_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, shiny::uiOutput(ns("mode")),
               actions = shiny::actionButton(ns("new"), "New requirement", class = "mb-btn")),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "write.outline", "Outline", width = 280,
                  shiny::tags$div(class = "mb-label", "Specification outline"),
                  shiny::uiOutput(ns("outline")),
                  shiny::uiOutput(ns("actors_note"))),
      shiny::tags$main(
        class = "mb-main mb-draft-main",
        shiny::conditionalPanel(sprintf("output['%s'] == 'ears'", ns("mode_value")), mod_write_ears_ui(ns)),
        shiny::tags$div(class = "mb-draft",
                        shiny::textAreaInput(ns("text"), shiny::textOutput(ns("draft_label"), inline = TRUE), rows = 3, width = "100%")),
        shiny::tags$div(class = "mb-fields",
                        shiny::selectInput(ns("parent"), "Parent", choices = with_blank(character(0), "None"), selectize = FALSE),
                        shiny::selectInput(ns("level"), "Level", choices = REQ_LEVELS, selectize = FALSE),
                        shiny::selectInput(ns("type"), "Type", choices = with_blank(REQ_TYPES), selectize = FALSE),
                        shiny::selectInput(ns("verify"), "Verify by", choices = with_blank(VERIFY_METHODS), selectize = FALSE),
                        shiny::textInput(ns("title"), "Short title")),
        shiny::tags$section(class = "mb-panel",
                            shiny::tags$div(class = "mb-label", "Parsed sentence"),
                            shiny::uiOutput(ns("sentence"))),
        shiny::uiOutput(ns("terms")),
        # Commit stays in reach however long the page gets (UX review §2.1).
        shiny::tags$div(class = "mb-sticky-actions",
                        shiny::actionButton(ns("commit"), "Commit to specification", class = "mb-btn mb-btn-primary", disabled = TRUE,
                                            onclick = send_draft_js(ns("commit_request"), ns("text"))),
                        shiny::actionButton(ns("save_draft"), "Save draft", class = "mb-btn",
                                            onclick = send_draft_js(ns("save_request"), ns("text"))),
                        shiny::uiOutput(ns("commit_note"), class = "mb-caption"))
      ),
      # The checks column: the gate (code, every edit), then the coach (AI).
      side_column("right", "write.checks", "Checks", width = 380,
                  shiny::uiOutput(ns("gate")),
                  shiny::uiOutput(ns("coach")),
                  shiny::uiOutput(ns("scope_gaps")))
    )
  )
}

ask_dismiss <- function(ns, id, confirm_input, reason_input) {
  shiny::showModal(shiny::modalDialog(title = "Dismiss with reason",
                                      shiny::textAreaInput(ns(reason_input), "Why doesn't this apply?", width = "100%"),
                                      footer = shiny::tagList(shiny::modalButton("Cancel"),
                                                              shiny::actionButton(ns(confirm_input), "Dismiss", class = "mb-btn mb-btn-primary"))))
}

mod_write_ears_ui <- function(ns) {
  shiny::tags$section(
    class = "mb-panel",
    shiny::tags$div(class = "mb-label", "EARS builder"),
    shiny::tags$div(class = "mb-fields",
                    shiny::selectInput(ns("ears_pattern"), "Pattern", choices = EARS_PATTERNS, selectize = FALSE),
                    shiny::textInput(ns("ears_trigger"), "Trigger, state or condition"),
                    shiny::selectInput(ns("ears_actor"), "Actor", choices = with_blank(character(0), "Choose an element"), selectize = FALSE)),
    shiny::textInput(ns("ears_response"), "Response", width = "100%"),
    shiny::tags$div(class = "mb-fields",
                    shiny::selectInput(ns("ears_bound"), "Bound", choices = c("within", "at least", "no more than", "no less than"), selectize = FALSE),
                    shiny::textInput(ns("ears_value"), "Value", placeholder = "[value]"),
                    shiny::selectInput(ns("ears_unit"), "Unit", choices = with_blank(EARS_UNIT_CHOICES, "None"), selectize = FALSE)),
    shiny::tags$div(shiny::actionButton(ns("ears_use"), "Put the sentence in the draft", class = "mb-btn"))
  )
}

# Shiny already holds text input back 250 ms, so 50 more gives the 300 ms gate clock.
mod_write_server <- function(id, state, debounce_ms = 50, coach_ms = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    mode <- shiny::reactiveVal("free")
    editing <- shiny::reactiveVal(NULL)
    last_commit <- shiny::reactiveVal(NULL)
    pending_term <- shiny::reactiveVal(NULL)

    draft <- shiny::reactive(if (is.null(input$text)) "" else input$text)
    # The gate clock (handoff §5.1).
    draft_debounced <- shiny::debounce(draft, debounce_ms)

    dictionary <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "settings")
      model_terms(con())
    })
    gate <- shiny::reactive(check_requirement(draft_debounced(), dictionary()))

    # --- The coach clock (handoff §5.1): it depends on the draft text only, never on gate
    # output, which reads the database and once made this loop in the Coach.
    coach_wait <- shiny::reactive({
      db_dep(state, "settings")
      value <- suppressWarnings(as.numeric(get_setting(con(), "coach_wait_ms", "2000")))
      if (is.na(value)) 2000 else value
    })
    draft_for_coach <- shiny::debounce(draft, shiny::reactive(if (is.null(coach_ms)) coach_wait() else coach_ms))
    review <- shiny::reactiveVal(NULL)
    coach_failure <- shiny::reactiveVal(NULL)
    last_processed <- shiny::reactiveVal("")
    scope_gaps_ran <- shiny::reactiveVal(FALSE)

    light_task <- shiny::ExtendedTask$new(function(connection, text, parent) run_coach_async(connection, text, "req_light_check", parent))
    deep_task <- shiny::ExtendedTask$new(function(connection, text, parent) run_coach_async(connection, text, "req_deep_check", parent))
    gaps_task <- shiny::ExtendedTask$new(function(connection) run_gaps_async(connection, "scope_gaps"))

    current_squished <- shiny::reactive(stringr::str_squish(draft()))

    shiny::observeEvent(draft_for_coach(), {
      text <- stringr::str_squish(draft_for_coach())
      connection <- shiny::isolate(con())
      # debounce() doesn't dedupe by value, so this guard does (BUILD_PLAN §5).
      key <- paste(DBI::dbGetInfo(connection)$dbname, text, sep = "\r")
      if (identical(key, shiny::isolate(last_processed()))) return()
      last_processed(key)
      if (!nzchar(text) || !coach_configured(connection)) return()
      if (!check_requirement(text, shiny::isolate(dictionary()))$passes_gate) return()
      parent <- shiny::isolate(input$parent)
      cached <- coach_from_cache(connection, text, "req_light_check", parent)
      if (!is.null(cached)) {
        coach_failure(NULL)
        review(cached)
        return()
      }
      light_task$invoke(connection, text, parent)
    }, ignoreNULL = FALSE)

    # A result is cached by core when it arrives, but it's only current if the text still matches.
    take_review <- function(result) {
      if (is.null(result$text)) result$text <- shiny::isolate(current_squished())
      if (isTRUE(result$ok)) {
        if (identical(result$text, shiny::isolate(current_squished()))) {
          coach_failure(NULL)
          review(result)
        }
      } else {
        coach_failure(result)
      }
      invalidate_db(state, "ai")
    }
    shiny::observeEvent(light_task$result(), take_review(light_task$result()))
    shiny::observeEvent(deep_task$result(), take_review(deep_task$result()))
    shiny::observeEvent(gaps_task$result(), invalidate_db(state, "ai"))
    shiny::observeEvent(input$deep_check, deep_task$invoke(con(), current_squished(), input$parent))

    current_review <- shiny::reactive({
      held <- review()
      if (!is.null(held) && identical(held$text, current_squished())) held else NULL
    })
    current_failure <- shiny::reactive({
      held <- coach_failure()
      if (!is.null(held) && identical(held$text, current_squished())) held else NULL
    })

    commit_info <- shiny::reactive({
      # Checks the live text, so a stale gate result can't leave Commit on while typing.
      current <- current_squished()
      result <- gate()
      if (!identical(result$text, current)) return(list(enabled = FALSE, reason = "Checking…"))
      db_dep(state, "ai")
      held <- current_review()
      failed <- current_failure()
      commit_state(result, current, coach_configured(con()), if (is.null(held)) NA_character_ else held$text,
                   if (is.null(failed)) NA_character_ else failed$text)
    })

    output$coach <- shiny::renderUI({
      db_dep(state, "ai")
      running <- light_task$status() == "running" || deep_task$status() == "running"
      render_coach_panel(current_review(), current_failure(), running, coach_configured(con()), ns)
    })
    output$scope_gaps <- shiny::renderUI({
      db_dep(state, "ai")
      render_scope_gaps(proposal_list(con(), "open", "ai"), ns, gaps_task$status() == "running")
    })
    shiny::observeEvent(input$see_prompt, {
      prompt <- coach_inputs(con(), current_squished(), input$see_prompt, input$parent)
      shiny::showModal(shiny::modalDialog(title = "See the prompt", size = "l", easyClose = TRUE,
                                          shiny::tags$pre(style = "white-space:pre-wrap;font-size:12px", prompt_dialog_text(prompt)),
                                          footer = shiny::modalButton("Close")))
    })
    shiny::observeEvent(input$coach_dismiss, {
      shiny::showModal(shiny::modalDialog(title = "Dismiss with reason",
                                          shiny::textAreaInput(ns("coach_dismiss_reason"), "Why doesn't this apply?", width = "100%"),
                                          footer = shiny::tagList(shiny::modalButton("Cancel"),
                                                                  shiny::actionButton(ns("coach_dismiss_confirm"), "Dismiss", class = "mb-btn mb-btn-primary"))))
    })
    shiny::observeEvent(input$coach_dismiss_confirm, {
      held <- current_review()
      shiny::req(held)
      if (!nzchar(trimws(input$coach_dismiss_reason))) return(refusal_toast("Dismissing needs a reason."))
      shiny::removeModal()
      first <- if (length(held$answer$questions) > 0L) held$answer$questions[[1]]$text else "Coach review"
      id <- insert_proposal(con(), list(task = "req_light_check", proposal_kind = "question", title = first, payload_json = "{}", llm_call_id = held$call_id))
      dismiss_proposal(con(), id, input$coach_dismiss_reason)
      invalidate_db(state, "ai")
    })
    shiny::observeEvent(input$gap_draft, {
      draft_text <- guarded_edit(start_from_sketch(con(), input$gap_draft))
      if (!is.null(draft_text)) {
        clear_form()
        shiny::updateTextAreaInput(session, "text", value = draft_text)
        invalidate_db(state, "ai")
      }
    })
    shiny::observeEvent(input$gap_dismiss, ask_dismiss(ns, input$gap_dismiss, "gap_dismiss_confirm", "gap_dismiss_reason"))
    pending_dismiss <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$gap_dismiss, pending_dismiss(input$gap_dismiss))
    shiny::observeEvent(input$gap_dismiss_confirm, {
      if (!is.null(guarded_edit({
        dismiss_proposal(con(), pending_dismiss(), input$gap_dismiss_reason)
        TRUE
      }))) {
        shiny::removeModal()
        invalidate_db(state, "ai")
      }
    })

    shiny::observe({
      shiny::updateActionButton(session, "commit", disabled = !commit_info()$enabled)
    })

    output$mode <- shiny::renderUI({
      button <- function(value, label) {
        shiny::tags$button(type = "button", `aria-pressed` = tolower(as.character(mode() == value)),
                           onclick = event_js(ns("set_mode"), value), label)
      }
      shiny::tags$div(role = "group", `aria-label` = "Input mode", class = "mb-segmented",
                      button("free", "Free text"), button("ears", "EARS builder"))
    })
    output$mode_value <- shiny::renderText(mode())
    shiny::outputOptions(output, "mode_value", suspendWhenHidden = FALSE)
    shiny::observeEvent(input$set_mode, mode(input$set_mode))

    output$draft_label <- shiny::renderText({
      id_now <- editing()
      parent <- input$parent
      where <- if (!is.null(parent) && nzchar(parent)) paste(" · child of", parent) else ""
      if (is.null(id_now)) paste0("New requirement", where) else paste0("Editing ", id_now, where)
    })
    output$sentence <- shiny::renderUI(render_parsed_sentence(parse_sentence(draft_debounced())))
    output$gate <- shiny::renderUI(render_gate(gate()))
    output$terms <- shiny::renderUI(render_terms(gate()$terms, ns("term_action")))
    output$commit_note <- shiny::renderUI({
      just <- last_commit()
      if (!is.null(just) && !nzchar(trimws(draft()))) return(shiny::tags$span(sprintf("Committed %s.", just)))
      shiny::tags$span(commit_info()$reason)
    })
    outline <- shiny::reactive({
      db_dep(state, "spec")
      requirement_outline(con())
    })
    output$outline <- shiny::renderUI(render_outline(outline(), ns("open"), editing()))
    output$actors_note <- shiny::renderUI({
      db_dep(state, "model")
      counts <- DBI::dbGetQuery(con(), "SELECT kind, count(*) AS n FROM element GROUP BY kind")
      get <- function(kind) sum(counts$n[counts$kind == kind])
      shiny::tags$div(class = "mb-caption", style = "padding-top:12px;border-top:1px solid var(--line)",
                      sprintf("Actors come from the model: %d components and %d external actors. A term that isn't in the model is a question, not an error.",
                              get("component"), get("external")))
    })

    shiny::observe({
      rows <- outline()
      labels <- ifelse(is.na(rows$short_title), rows$id, paste(rows$id, rows$short_title))
      current <- shiny::isolate(input$parent)
      shiny::updateSelectInput(session, "parent", choices = with_blank(stats::setNames(rows$id, labels), "None"),
                               selected = if (is.null(current)) "" else current)
    })
    shiny::observe({
      db_dep(state, "model")
      elements <- DBI::dbGetQuery(con(), "SELECT name FROM element WHERE kind = 'component' ORDER BY name")$name
      shiny::updateSelectInput(session, "ears_actor", choices = with_blank(elements, "Choose an element"))
    })

    load_requirement <- function(id) {
      row <- read_entity(con(), "requirement", id)
      if (is.null(row)) return(invisible(NULL))
      editing(id)
      value <- function(x) if (is.na(x)) "" else x
      shiny::updateTextAreaInput(session, "text", value = row$text)
      shiny::updateSelectInput(session, "parent", selected = value(row$parent_id))
      shiny::updateSelectInput(session, "level", selected = row$level)
      shiny::updateSelectInput(session, "type", selected = value(row$req_type))
      shiny::updateSelectInput(session, "verify", selected = value(row$verification_method))
      shiny::updateTextInput(session, "title", value = value(row$short_title))
    }
    clear_form <- function() {
      editing(NULL)
      shiny::updateTextAreaInput(session, "text", value = "")
      shiny::updateSelectInput(session, "type", selected = "")
      shiny::updateSelectInput(session, "verify", selected = "")
      shiny::updateTextInput(session, "title", value = "")
    }
    shiny::observeEvent(input$open, load_requirement(input$open))
    shiny::observeEvent(input$new, {
      last_commit(NULL)
      clear_form()
    })
    shiny::observeEvent(state$open_in_write(), {
      target <- state$open_in_write()
      if (is.list(target)) {
        clear_form()
        shiny::updateTextAreaInput(session, "text", value = target$text)
      } else {
        load_requirement(target)
      }
      state$open_in_write(NULL)
    }, ignoreNULL = TRUE)

    form_fields <- function() {
      blank <- function(x) if (is.null(x) || !nzchar(x)) NA_character_ else x
      fields <- list(parent_id = blank(input$parent), req_type = blank(input$type),
                     verification_method = blank(input$verify), short_title = blank(input$title))
      if (!is.null(input$level)) fields$level <- input$level
      fields
    }

    # Core re-runs the gate here, so a stale button can't commit a failing draft.
    shiny::observeEvent(input$commit_request, {
      text <- stringr::str_squish(input$commit_request$text)
      held <- shiny::isolate(review())
      reviewed <- !is.null(held) && identical(held$text, text)
      written <- guarded_edit(commit_requirement(con(), text, editing(), form_fields(),
                                                 coach_state = if (reviewed) "reviewed" else "uncoached",
                                                 assessed_text = if (reviewed) held$text, coach_call_id = if (reviewed) held$call_id))
      if (is.null(written)) return()
      if (reviewed) record_conflicts(con(), written$id, held$answer, held$call_id)
      last_commit(written$id)
      invalidate_db(state, written$area)
      clear_form()
      # Scope gaps run once per session, in the background, after the first commit.
      if (!scope_gaps_ran() && coach_configured(con())) {
        scope_gaps_ran(TRUE)
        gaps_task$invoke(con())
      }
    })
    shiny::observeEvent(input$save_request, {
      written <- guarded_edit(save_draft(con(), input$save_request$text, editing(), form_fields()))
      if (is.null(written)) return()
      editing(written$id)
      invalidate_db(state, written$area)
    })

    shiny::observeEvent(input$ears_use, {
      unit <- if (is.null(input$ears_unit)) "" else input$ears_unit
      sentence <- build_ears(input$ears_pattern, input$ears_actor, input$ears_response, input$ears_trigger,
                             input$ears_bound, input$ears_value, unit)
      shiny::updateTextAreaInput(session, "text", value = sentence)
    })

    # --- Terms panel ---
    shiny::observeEvent(input$term_action, {
      action <- input$term_action
      if (identical(action$action, "not_model")) {
        guarded_edit(mark_not_model_term(con(), action$term))
        invalidate_db(state, "settings")
        return()
      }
      pending_term(action)
      elements <- DBI::dbGetQuery(con(), "SELECT id, name FROM element WHERE kind = 'component' ORDER BY name")
      element_choices <- stats::setNames(elements$id, elements$name)
      units <- with_blank(DBI::dbGetQuery(con(), "SELECT symbol FROM unit ORDER BY symbol")$symbol, "None")
      fields <- switch(
        action$action,
        property = shiny::tagList(shiny::selectInput(ns("term_element"), "Property of", choices = with_blank(element_choices, "Choose an element"), selectize = FALSE),
                                  shiny::selectInput(ns("term_unit"), "Unit", choices = units, selectize = FALSE)),
        field = {
          interfaces <- DBI::dbGetQuery(con(), "SELECT id, name FROM interface WHERE layer = 'physical' ORDER BY name")
          shiny::tagList(shiny::selectInput(ns("term_interface"), "Field of", choices = with_blank(stats::setNames(interfaces$id, interfaces$name), "Choose an interface"),
                                            selectize = FALSE),
                         shiny::selectInput(ns("term_type"), "Type", choices = c("float32", "float64", "int32", "uint32", "uint64", "bool", "string"), selectize = FALSE),
                         shiny::selectInput(ns("term_unit"), "Unit", choices = units, selectize = FALSE))
        },
        component = shiny::selectInput(ns("term_parent"), "Part of", choices = with_blank(element_choices, "Top level"), selectize = FALSE)
      )
      shiny::showModal(shiny::modalDialog(
        title = sprintf("%s: %s", sub("…$", "", TERM_ACTION_LABELS[[action$action]]), action$term),
        fields,
        shiny::tags$p(class = "mb-caption", "Its value starts as [value] until you enter it."),
        footer = shiny::tagList(shiny::modalButton("Cancel"),
                                shiny::actionButton(ns("term_confirm"), "Add to the model", class = "mb-btn mb-btn-primary"))
      ))
    })

    shiny::observeEvent(input$term_confirm, {
      action <- pending_term()
      shiny::req(action)
      # The owner is the engineer's choice, so nothing is preselected.
      missing_owner <- (action$action == "property" && !nzchar(input$term_element)) ||
        (action$action == "field" && !nzchar(input$term_interface))
      if (missing_owner) return(refusal_toast("Choose where this term belongs first."))
      shiny::removeModal()
      parent <- if (is.null(input$term_parent) || !nzchar(input$term_parent)) NULL else input$term_parent
      written <- guarded_edit(switch(
        action$action,
        property = add_term_as_property(con(), action$term, input$term_element, input$term_unit),
        field = add_term_as_field(con(), action$term, input$term_interface, input$term_type, input$term_unit),
        component = add_term_as_component(con(), action$term, parent)
      ))
      pending_term(NULL)
      if (!is.null(written)) invalidate_db(state, "model")
    })

    invisible(list(gate = gate, commit_info = commit_info, editing = editing, review = current_review, failure = current_failure))
  })
}
