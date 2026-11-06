# Behavior › Failure modes: what can fail, its effects and the system's response, with the
# checks that find the cases nobody has recorded (docs/VIEWPOINTS_AND_FAILURES.md §3).

mod_failures_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, actions = shiny::uiOutput(ns("add"), inline = TRUE)),
    shiny::tags$div(class = "mb-body",
                    side_column("left", "failures.list", "Failure modes", width = 300, shiny::uiOutput(ns("list"))),
                    shiny::tags$main(class = "mb-main", shiny::uiOutput(ns("editor"))),
                    side_column("right", "failures.checks", "Checks", width = 340, shiny::uiOutput(ns("checks"))))
  )
}

response_chip <- function(state) {
  shiny::tags$span(class = paste("mb-chip", if (state %in% c("placeholder", "unhandled")) "mb-gap"), FAILURE_RESPONSES[[state]])
}

render_failure_list <- function(rows, select_input, selected) {
  if (nrow(rows) == 0L) return(shiny::tags$p(class = "mb-caption", "No failure modes yet. Add one above, or from a check on the right."))
  groups <- list("Response open" = rows$response_state %in% c("placeholder", "unhandled"),
                 "Handled or accepted" = rows$response_state %in% c("handled", "accepted"))
  lapply(names(groups), function(label) {
    group <- rows[groups[[label]], , drop = FALSE]
    if (nrow(group) == 0L) return(NULL)
    shiny::tags$div(
      class = "mb-list-group",
      shiny::tags$div(class = "mb-label", sprintf("%s (%d)", label, nrow(group))),
      lapply(seq_len(nrow(group)), function(i) {
        row <- group[i, ]
        shiny::tags$button(type = "button", class = "mb-card-select", `aria-current` = if (identical(row$id, selected)) "true",
                           onclick = event_js(select_input, row$id),
                           shiny::tags$span(class = "mono", style = "font-size:11px;color:var(--ink-3)",
                                            sprintf("%s · %s", row$id, toupper(FAILURE_CAUSES[[row$cause_kind]]))),
                           shiny::tags$b(row$subject),
                           if (!is.na(row$system_effect)) shiny::tags$span(style = "font-size:13px", row$system_effect),
                           shiny::tags$span(response_chip(row$response_state),
                                            if (row$review_state == "unreviewed") shiny::tags$span(class = "mb-badge-ai", "AI")))
      })
    )
  })
}

render_failure_links <- function(links, type, choices, ns) {
  mine <- links[links$link_type == type, , drop = FALSE]
  label <- if (type == "detected_by") "Detected by" else "Handled by"
  shiny::tagList(
    shiny::tags$div(class = "mb-label", label),
    if (nrow(mine) == 0L) shiny::tags$span(class = "mb-caption", "Nothing linked."),
    lapply(seq_len(nrow(mine)), function(i) {
      shiny::tags$div(class = "mb-prop-grid", style = "grid-template-columns: minmax(0,1fr) auto",
                      shiny::tags$span(mine$label[i]),
                      shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Remove", mine$label[i]),
                                         onclick = event_js(ns("unlink"), mine$id[i]), "Remove"))
    }),
    labelled(paste(label, "…"), select_tag(stats::setNames(choices$value, choices$label), "", change_js(ns("link"), list(type = type)),
                                            "Add a link…", label = paste("Add to", tolower(label))))
  )
}

render_failure_editor <- function(row, subjects, fields, links, detect_choices, handle_choices, ns) {
  text_or_blank <- function(x) if (is.null(x) || is.na(x)) "" else x
  field <- function(name) change_js(ns("change"), list(field = name))
  note_label <- switch(row$response_state, accepted = "Why it's accepted", handled = "How it's handled", "Note")
  shiny::tags$section(
    class = "mb-inspector", style = "max-width:720px",
    shiny::tags$div(class = "mono mb-caption", row$id),
    if (row$review_state == "unreviewed") {
      shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b("Unreviewed AI suggestion"))
    },
    labelled("What fails", select_tag(stats::setNames(subjects$value, subjects$label), paste(row$subject_kind, row$subject_id, sep = "|"),
                                      field("subject"), label = "What fails")),
    if (row$subject_kind == "interface" && length(fields) > 0L) {
      labelled("Field", select_tag(stats::setNames(fields, fields), text_or_blank(row$field_name), field("field_name"), "The whole interface", label = "Field"))
    },
    labelled("Cause", select_tag(stats::setNames(names(FAILURE_CAUSES), FAILURE_CAUSES), row$cause_kind, field("cause_kind"), label = "Cause")),
    labelled("In words", shiny::tags$textarea(rows = 2, onchange = field("cause"), `aria-label` = "Cause in words", text_or_blank(row$cause))),
    labelled("Effect nearby", shiny::tags$textarea(rows = 2, onchange = field("local_effect"), `aria-label` = "Effect nearby", text_or_blank(row$local_effect))),
    labelled("Effect on the system", shiny::tags$textarea(rows = 2, onchange = field("system_effect"), `aria-label` = "Effect on the system", text_or_blank(row$system_effect))),
    labelled("Response", select_tag(stats::setNames(names(FAILURE_RESPONSES), FAILURE_RESPONSES), row$response_state, field("response_state"), label = "Response")),
    labelled(note_label, shiny::tags$textarea(rows = 2, onchange = field("response_note"), `aria-label` = note_label, text_or_blank(row$response_note))),
    render_failure_links(links, "detected_by", detect_choices, ns),
    render_failure_links(links, "handled_by", handle_choices, ns),
    danger_zone(confirm_button("Delete failure mode…", ns("delete"), row$id, sprintf("Delete %s?", row$id),
                               "It goes from the model with its links. Undo brings it back.", confirm = "Delete failure mode"))
  )
}

# Sends a check to set aside, with the reason typed beside it.
waive_js <- function(input_id, row, note_id) {
  sprintf("Shiny.setInputValue('%s', {rule: '%s', kind: '%s', id: '%s', note: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})",
          input_id, row$rule, row$subject_kind, row$subject_id, note_id)
}

render_failure_checks <- function(findings, waivers, ns) {
  set_aside <- if (nrow(waivers) > 0L) {
    shiny::tags$div(
      class = "mb-list-group",
      shiny::tags$div(class = "mb-label", sprintf("Set aside (%d)", nrow(waivers))),
      lapply(seq_len(nrow(waivers)), function(i) {
        row <- waivers[i, ]
        shiny::tags$div(class = "mb-prop-grid", style = "grid-template-columns: minmax(0,1fr) auto",
                        shiny::tags$span(style = "font-size:13px", sprintf("%s · %s: “%s”", FAILURE_RULES[[row$rule]], row$subject, row$rationale)),
                        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Check again:", row$subject),
                                           onclick = event_js(ns("unwaive"), paste(row$rule, row$subject_kind, row$subject_id, sep = "|")), "Check again"))
      }))
  }
  if (nrow(findings) == 0L) return(shiny::tagList(shiny::tags$p(class = "mb-caption", "Every check passes."), set_aside))
  shiny::tagList(lapply(names(FAILURE_RULES), function(rule) {
    group <- findings[findings$rule == rule, , drop = FALSE]
    if (nrow(group) == 0L) return(NULL)
    shiny::tags$div(
      class = "mb-list-group",
      shiny::tags$div(class = "mb-label", sprintf("%s (%d)", FAILURE_RULES[[rule]], nrow(group))),
      lapply(seq_len(nrow(group)), function(i) {
        row <- group[i, ]
        key <- paste(row$rule, row$subject_kind, row$subject_id, sep = "|")
        note_id <- ns(paste0("waive_", gsub("[^A-Za-z0-9]", "_", key)))
        shiny::tags$div(
          class = "mb-card", style = "padding:8px 10px;display:flex;flex-direction:column;gap:6px",
          shiny::tags$span(class = "mb-gap", style = "font-size:13px", row$text),
          if (rule %in% WAIVABLE_RULES) {
            shiny::tags$div(
              style = "display:flex;flex-wrap:wrap;gap:6px;align-items:center",
              shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("add_suggested"), list(subject = row$suggest_subject, cause = row$suggest_cause,
                                                                                                                         field = if (is.na(row$suggest_field)) "" else row$suggest_field)),
                                 sprintf("Add %s failure", tolower(FAILURE_CAUSES[[row$suggest_cause]]))),
              shiny::tags$details(shiny::tags$summary(class = "mb-linkish", "Doesn't apply…"),
                                  shiny::tags$div(style = "display:flex;gap:6px;margin-top:6px",
                                                  shiny::tags$input(type = "text", id = note_id, placeholder = "Why it doesn't apply", `aria-label` = "Why it doesn't apply"),
                                                  shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = waive_js(ns("waive"), row, note_id), "Set aside")))
            )
          } else {
            shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", style = "align-self:flex-start", onclick = event_js(ns("select"), row$failure_id), "Open")
          }
        )
      })
    )
  }), set_aside)
}

mod_failures_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    selected <- state$selected_failure
    refresh <- function() for (area in c("model", "spec", "behavior")) db_dep(state, area)

    rows <- shiny::reactive({
      refresh()
      failure_mode_list(con())
    })
    subjects <- shiny::reactive({
      refresh()
      failure_subjects(con())
    })
    findings <- shiny::reactive({
      refresh()
      failure_findings(con())
    })

    output$add <- shiny::renderUI({
      choices <- subjects()
      select_tag(stats::setNames(choices$value, choices$label), "", change_js(ns("add_new"), list()), "+ Failure mode for…", label = "Add a failure mode for")
    })
    output$list <- shiny::renderUI(render_failure_list(rows(), ns("select"), selected()))
    output$checks <- shiny::renderUI({
      waivers <- failure_waivers(con())
      waivers$subject <- vapply(seq_len(nrow(waivers)), function(i) {
        kind <- waivers$subject_kind[i]
        id <- waivers$subject_id[i]
        if (kind == "interface_field") {
          field <- DBI::dbGetQuery(con(), "SELECT f.name, i.name AS interface FROM interface_field f JOIN interface i ON i.id = f.interface_id WHERE f.id = ?", params = list(id))
          if (nrow(field) == 1L) paste0(field$interface, ".", field$name) else id
        } else {
          failure_subject_label(con(), kind, id)
        }
      }, "")
      render_failure_checks(findings(), waivers, ns)
    })
    output$editor <- shiny::renderUI({
      id <- selected()
      all <- rows()
      if (is.null(id) || !id %in% all$id) {
        return(shiny::tags$p(class = "mb-caption", "Choose a failure mode on the left, or add one. Each records what fails, why, what happens next, and how the system responds."))
      }
      row <- all[all$id == id, , drop = FALSE]
      fields <- if (row$subject_kind == "interface") interface_fields(con(), row$subject_id)$name else character(0)
      render_failure_editor(row, subjects(), fields, failure_links(con(), id),
                            failure_link_choices(con(), "detected_by"), failure_link_choices(con(), "handled_by"), ns)
    })

    shiny::observeEvent(input$select, selected(input$select))
    created <- function(written) if (!is.null(written)) selected(written$id)
    shiny::observeEvent(input$add_new, {
      if (nzchar(input$add_new$value)) created(model_edit(state, add_failure_mode(con(), input$add_new$value), "add failure mode"))
    })
    shiny::observeEvent(input$add_suggested, {
      suggestion <- input$add_suggested
      created(model_edit(state, add_failure_mode(con(), suggestion$subject, suggestion$cause, suggestion$field), "add failure mode"))
    })
    shiny::observeEvent(input$change, {
      model_edit(state, update_failure_mode(con(), selected(), input$change$field, input$change$value), "edit failure mode")
    })
    shiny::observeEvent(input$link, {
      if (nzchar(input$link$value)) model_edit(state, link_failure(con(), selected(), input$link$type, input$link$value), "link failure mode")
    })
    shiny::observeEvent(input$unlink, model_edit(state, delete_entity(con(), "link", input$unlink), "unlink failure mode"))
    shiny::observeEvent(input$delete, {
      if (!is.null(model_edit(state, delete_entity(con(), "failure_mode", input$delete), "delete failure mode"))) selected(NULL)
    })
    shiny::observeEvent(input$waive, {
      waive <- input$waive
      model_edit(state, waive_failure_check(con(), waive$rule, waive$kind, waive$id, waive$note), "set check aside")
    })
    shiny::observeEvent(input$unwaive, model_edit(state, delete_entity(con(), "failure_waiver", input$unwaive), "check again"))
    invisible(list(selected = selected, findings = findings))
  })
}
