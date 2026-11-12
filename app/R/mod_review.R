# Analyze › Review queue: structural gaps from code and AI proposals (handoff §5.3).

mod_review_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, actions = shiny::tagList(shiny::uiOutput(ns("run_status"), inline = TRUE),
                                              shiny::actionButton(ns("run"), "Ask AI about gaps", class = "mb-btn mb-btn-ai"))),
    shiny::tags$div(
      class = "mb-scroll",
      shiny::tags$div(
        class = "mb-reading mb-queue-page",
        shiny::tags$div(class = "mb-head", shiny::tags$h1(style = "margin:0;font-size:20px;font-weight:600", "Review queue"),
                        shiny::tags$span(class = "mb-caption", "The AI asks; it may sketch structure as unreviewed drafts. Values and links stay yours, and nothing reaches a baseline without your note.")),
        # Filters in one row above the list (UX review §2.25).
        shiny::tags$div(class = "mb-filterbar",
                        shiny::checkboxGroupInput(ns("sources"), "Source", choices = c("Structural check" = "structural", "AI-inferred" = "ai"), selected = c("structural", "ai"), inline = TRUE),
                        shiny::radioButtons(ns("status"), "Status", choices = c("Open" = "open", "Kept" = "kept", "Dismissed" = "dismissed"), inline = TRUE),
                        shiny::tags$span(class = "mb-caption mb-push", "Structural checks run by code, with no AI. Each kept item keeps its model, prompt version and your note.")),
        shiny::uiOutput(ns("cards"))
      )
    )
  )
}

# Structural gaps of one kind, one compact row each, under a single heading.
render_gap_group <- function(rows, title, ns) {
  shiny::tags$section(
    class = "mb-panel mb-gap-group",
    shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-tag", "STRUCTURAL"), shiny::tags$b(sprintf("%s (%d)", title, nrow(rows)))),
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      open <- row$status == "open"
      shiny::tags$div(
        class = "mb-gap-row-item",
        shiny::tags$span(class = "mb-gap-title", row$title),
        if (open) {
          shiny::tags$span(class = "mb-term-actions",
                           shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("show"), list(kind = row$target_kind, id = row$target_id, title = row$title)), "Show on diagram"),
                           shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("ask"), row$title), "Ask AI to draft…"),
                           shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("dismiss"), row$id), "Dismiss with reason"))
        } else {
          shiny::tags$span(class = "mb-caption", if (!is.na(row$dismiss_reason)) sprintf("Reason: %s", row$dismiss_reason) else if (!is.na(row$engineer_note)) sprintf("Your note: %s", row$engineer_note))
        }
      )
    })
  )
}

GAP_GROUP_TITLES <- c(element = "No linked requirements", port = "Ports with no connector")

# One button per candidate, off until the rationale has text (handoff §5.3).
render_candidate_choices <- function(row, ns, dismiss = NULL) {
  candidates <- jsonlite::fromJSON(row$candidates_json, simplifyVector = FALSE)
  note <- ns(paste0("rationale_", row$id))
  group <- ns(paste0("links_", row$id))
  shiny::tagList(
    lapply(candidates, function(candidate) {
      if (is.null(candidate$why) || !nzchar(candidate$why)) return(NULL)
      shiny::tags$div(class = "mb-caption", shiny::tags$b(candidate$name), ": ", candidate$why)
    }),
    shiny::tags$label(style = "display:flex;flex-direction:column;gap:4px;font-size:13px", "Your rationale, required to link",
                      shiny::tags$input(type = "text", id = note, class = "mb-table-input", placeholder = "In your own words",
                                        oninput = sprintf("document.querySelectorAll('#%s button.mb-candidate').forEach(b => b.disabled = !this.value.trim())", group))),
    shiny::tags$div(id = group, class = "mb-term-actions",
                    lapply(candidates, function(candidate) {
                      shiny::tags$button(type = "button", class = "mb-btn mb-candidate", disabled = NA,
                                         onclick = sprintf("Shiny.setInputValue('%s', {id: %s, handle: %s, note: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})",
                                                           ns("keep_link"), jsonlite::toJSON(row$id, auto_unbox = TRUE), jsonlite::toJSON(candidate$n, auto_unbox = TRUE), note),
                                         sprintf("Link to %s", candidate$name))
                    }),
                    dismiss)
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
    if (row$proposal_kind %in% c("sketch_element", "sketch_function", "sketch_failure")) {
      box <- if (row$proposal_kind == "sketch_failure") {
        effects <- c(payload$local_effect, payload$system_effect)
        shiny::tags$div(class = "mb-sketch-box",
                        shiny::tags$div(class = "mono mb-caption", "Suggested failure · the response stays yours"),
                        shiny::tags$b(payload$subject_label %||% payload$subject_id), " · ", FAILURE_CAUSES[[payload$cause_kind]],
                        if (length(effects) > 0L) shiny::tags$div(style = "font-size:13px", paste(effects[nzchar(effects)], collapse = " → ")))
      } else {
        ports <- vapply(payload$ports, function(port) sprintf("%s %s", port$direction, port$interface), "")
        shiny::tags$div(class = "mb-sketch-box",
                        shiny::tags$div(class = "mono mb-caption", "Sketch · structure only, no values"),
                        shiny::tags$b(payload$name), if (nzchar(payload$stereotype %||% "")) sprintf(" «%s»", payload$stereotype),
                        if (length(ports) > 0L) paste(" · ports:", paste(ports, collapse = ", ")))
      }
      shiny::tagList(
        box,
        if (open) {
          note <- ns(paste0("note_", row$id))
          confirm <- ns("keep")
          shiny::tagList(
            shiny::tags$label(style = "display:flex;flex-direction:column;gap:4px;font-size:13px",
                              if (row$proposal_kind == "sketch_failure") "Your note: why this failure matters (required)" else "Your note: why the system needs this (required)",
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
    } else if (row$proposal_kind == "candidate_link" && !is.na(row$candidates_json)) {
      if (open) render_candidate_choices(row, ns, dismiss)
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

# "See the prompt": exactly what was sent, and the schema the answer had to fit.
show_llm_call <- function(con, call_id) {
  call <- DBI::dbGetQuery(con, "SELECT system_prompt, user_message, schema_json FROM llm_call WHERE id = ?", params = list(call_id))
  shiny::req(nrow(call) == 1L)
  shiny::showModal(shiny::modalDialog(title = "See the prompt", size = "l", easyClose = TRUE,
                                      shiny::tags$pre(style = "white-space:pre-wrap;font-size:12px",
                                                      paste0("SYSTEM\n", call$system_prompt, "\n\nMESSAGE\n", call$user_message, "\n\nANSWER SCHEMA\n", call$schema_json)),
                                      footer = shiny::modalButton("Close")))
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
      structural <- rows[rows$source == "structural", , drop = FALSE]
      others <- rows[rows$source != "structural", , drop = FALSE]
      kinds <- unique(structural$target_kind)
      shiny::tagList(
        lapply(kinds, function(kind) {
          title <- if (kind %in% names(GAP_GROUP_TITLES)) GAP_GROUP_TITLES[[kind]] else term_to_title(kind)
          render_gap_group(structural[structural$target_kind == kind, , drop = FALSE], title, ns)
        }),
        lapply(seq_len(nrow(others)), function(i) render_proposal_card(others[i, ], ns))
      )
    })

    shiny::observeEvent(input$keep, {
      written <- guarded_edit(keep_sketch(con(), input$keep$id, input$keep$note))
      if (is.null(written)) return()
      state$commands(push_command(state$commands(), change_command(written$change_set, "add sketch")))
      invalidate_db(state, c("ai", "model"))
    })
    shiny::observeEvent(input$keep_link, {
      written <- model_edit(state, keep_candidate(con(), input$keep_link$id, input$keep_link$handle, input$keep_link$note), "link")
      if (!is.null(written)) invalidate_db(state, "ai")
    })
    shiny::observeEvent(input$start, {
      draft <- guarded_edit(start_from_sketch(con(), input$start))
      if (is.null(draft)) return()
      invalidate_db(state, "ai")
      state$open_in_write(list(text = draft, proposal = input$start))
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
      # The diagram screen offers the way back to this item (§2.26).
      state$return_to(list(label = if (is.null(target$title)) element else target$title,
                           from = list(workspace = "Analyze", tab = "Review queue"), at = list(workspace = "Model", tab = "Structure")))
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
    shiny::observeEvent(input$see_call, show_llm_call(con(), input$see_call))
    invisible(list(proposals = proposals, task = gaps_task))
  })
}
