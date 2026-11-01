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

mod_write_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(
      class = "mb-tabbar",
      shiny::uiOutput(ns("mode")),
      shiny::tags$div(class = "mb-actions",
                      shiny::actionButton(ns("new"), "New requirement", class = "mb-btn"))
    ),
    shiny::tags$div(
      class = "mb-body",
      shiny::tags$aside(class = "mb-left",
                        shiny::tags$div(class = "mb-label", "Specification outline"),
                        shiny::uiOutput(ns("outline")),
                        shiny::uiOutput(ns("actors_note"))),
      shiny::tags$main(
        class = "mb-main",
        shiny::conditionalPanel(sprintf("output['%s'] == 'ears'", ns("mode_value")), mod_write_ears_ui(ns)),
        shiny::tags$div(class = "mb-draft",
                        shiny::textAreaInput(ns("text"), shiny::textOutput(ns("draft_label"), inline = TRUE), rows = 3, width = "100%")),
        shiny::tags$div(class = "mb-fields",
                        shiny::selectInput(ns("parent"), "Parent", choices = with_blank(character(0), "None")),
                        shiny::selectInput(ns("level"), "Level", choices = REQ_LEVELS),
                        shiny::selectInput(ns("type"), "Type", choices = with_blank(REQ_TYPES)),
                        shiny::selectInput(ns("verify"), "Verify by", choices = with_blank(VERIFY_METHODS)),
                        shiny::textInput(ns("title"), "Short title")),
        shiny::tags$section(class = "mb-panel",
                            shiny::tags$div(class = "mb-label", "Parsed sentence"),
                            shiny::uiOutput(ns("sentence"))),
        shiny::uiOutput(ns("gate")),
        shiny::uiOutput(ns("terms")),
        shiny::tags$div(style = "display:flex;flex-wrap:wrap;align-items:center;gap:8px 16px",
                        shiny::actionButton(ns("commit"), "Commit to specification", class = "mb-btn mb-btn-primary", disabled = TRUE,
                                            onclick = send_draft_js(ns("commit_request"), ns("text"))),
                        shiny::actionButton(ns("save_draft"), "Save draft", class = "mb-btn",
                                            onclick = send_draft_js(ns("save_request"), ns("text"))),
                        shiny::uiOutput(ns("commit_note"), class = "mb-caption"))
      ),
      shiny::tags$aside(class = "mb-right",
                        shiny::tags$section(class = "mb-panel",
                                            shiny::tags$b("Coach"),
                                            shiny::uiOutput(ns("coach_note"), class = "mb-caption")))
    )
  )
}

mod_write_ears_ui <- function(ns) {
  shiny::tags$section(
    class = "mb-panel",
    shiny::tags$div(class = "mb-label", "EARS builder"),
    shiny::tags$div(class = "mb-fields",
                    shiny::selectInput(ns("ears_pattern"), "Pattern", choices = EARS_PATTERNS),
                    shiny::textInput(ns("ears_trigger"), "Trigger, state or condition"),
                    shiny::selectInput(ns("ears_actor"), "Actor", choices = with_blank(character(0), "Choose an element"))),
    shiny::textInput(ns("ears_response"), "Response", width = "100%"),
    shiny::tags$div(class = "mb-fields",
                    shiny::selectInput(ns("ears_bound"), "Bound", choices = c("within", "at least", "no more than", "no less than")),
                    shiny::textInput(ns("ears_value"), "Value", placeholder = "[value]"),
                    shiny::selectInput(ns("ears_unit"), "Unit", choices = with_blank(EARS_UNIT_CHOICES, "None"))),
    shiny::tags$div(shiny::actionButton(ns("ears_use"), "Put the sentence in the draft", class = "mb-btn"))
  )
}

# Shiny already holds text input back 250 ms, so 50 more gives the 300 ms gate clock.
mod_write_server <- function(id, state, debounce_ms = 50) {
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

    commit_info <- shiny::reactive({
      # Checks the live text, so a stale gate result can't leave Commit on while typing.
      current <- stringr::str_squish(draft())
      result <- gate()
      if (!identical(result$text, current)) return(list(enabled = FALSE, reason = "Checking…"))
      commit_state(result, current, coach_configured(con()))
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
    output$coach_note <- shiny::renderUI({
      "No coach is configured yet. Commit runs on the gate alone and marks the requirement uncoached."
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
      written <- guarded_edit(commit_requirement(con(), input$commit_request$text, editing(), form_fields()))
      if (is.null(written)) return()
      last_commit(written$id)
      invalidate_db(state, written$area)
      clear_form()
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
        property = shiny::tagList(shiny::selectInput(ns("term_element"), "Property of", choices = with_blank(element_choices, "Choose an element")),
                                  shiny::selectInput(ns("term_unit"), "Unit", choices = units)),
        field = {
          interfaces <- DBI::dbGetQuery(con(), "SELECT id, name FROM interface WHERE layer = 'physical' ORDER BY name")
          shiny::tagList(shiny::selectInput(ns("term_interface"), "Field of", choices = with_blank(stats::setNames(interfaces$id, interfaces$name), "Choose an interface")),
                         shiny::selectInput(ns("term_type"), "Type", choices = c("float32", "float64", "int32", "uint32", "uint64", "bool", "string")),
                         shiny::selectInput(ns("term_unit"), "Unit", choices = units))
        },
        component = shiny::selectInput(ns("term_parent"), "Part of", choices = with_blank(element_choices, "Top level"))
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

    invisible(list(gate = gate, commit_info = commit_info, editing = editing))
  })
}
