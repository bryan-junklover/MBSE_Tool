# Pure renderers: data in, HTML out, so they can be tested without a session.

render_parsed_sentence <- function(parsed) {
  if (nrow(parsed) == 0L) return(shiny::tags$div(class = "mb-caption", "The parsed sentence appears as you type."))
  pieces <- lapply(seq_len(nrow(parsed)), function(i) {
    if (parsed$part[i] == "plain") parsed$text[i] else shiny::tags$span(class = paste0("part-", parsed$part[i]), .noWS = "outside", parsed$text[i])
  })
  shiny::tagList(
    shiny::tags$div(class = "mb-sentence", pieces),
    shiny::tags$div(class = "mb-legend",
                    shiny::tags$span("dotted = condition"), shiny::tags$span("thick = actor"),
                    shiny::tags$span("bold = shall"), shiny::tags$span("thin = response"),
                    shiny::tags$span("dashed = constraint"))
  )
}

ledger_label <- function(item, status) {
  switch(item,
         "Mandatory 'shall'" = if (status) 'contains "shall"' else 'no "shall"',
         "Condition Clause" = if (status) "condition clause" else "no condition clause (optional)",
         "Constraint or Bound" = if (status) "constraint or bound" else "no constraint or bound",
         "System-Centric Actor" = if (status) "the actor is a system" else "actor missing, or a person",
         "Named Actor" = if (status) "the actor is named" else "the actor is a pronoun",
         "Singular Concept" = if (status) "one concept" else "more than one concept",
         "Sufficient Detail" = if (status) "enough detail" else "too short",
         "Active Voice" = if (status) "active voice" else "passive voice",
         "Actor in the model" = if (status) "the actor is in the model" else "the actor isn't in the model",
         item)
}

format_points <- function(points) if (points > 0) sprintf("+%d", points) else as.character(points)

render_gate <- function(gate) {
  if (!nzchar(gate$text)) {
    return(shiny::tags$section(class = "mb-panel mb-gate",
                               shiny::tags$span(class = "mb-label", "Gate · every edit · no AI"),
                               shiny::tags$span(class = "mb-caption", "Nothing to check yet.")))
  }
  blockers <- gate_blockers(gate)
  rows <- unlist(lapply(seq_len(nrow(gate$ledger)), function(i) {
    list(shiny::tags$span(format_points(as.integer(gate$ledger$Impact[i]))),
         shiny::tags$span(ledger_label(gate$ledger$Item[i], gate$ledger$Status[i])))
  }), recursive = FALSE)
  banned <- lapply(gate$banned_words_found, function(word) {
    list(shiny::tags$span("-10"), shiny::tags$span(sprintf('banned word "%s"', word)))
  })
  shiny::tags$section(
    class = "mb-panel mb-gate",
    shiny::tags$div(class = "mb-head",
                    shiny::tags$span(class = "mb-label", "Gate · every edit · no AI"),
                    shiny::tags$b(class = "mb-push",
                                  sprintf("score %d · %s", as.integer(gate$score), if (gate$passes_gate) "PASS" else "FAIL"))),
    lapply(blockers, function(b) shiny::tags$div(class = "mb-hard", b)),
    shiny::tags$div(class = "mb-ledger", rows, unlist(banned, recursive = FALSE))
  )
}

TERM_ACTION_LABELS <- c(property = "Add as property…", field = "Add interface field…",
                        component = "Add component…", not_model = "Not a model term")

term_question <- function(role) {
  switch(role,
         actor = "Not in the model. Is it a component?",
         quantity = "Not in the model. Is it a property of a component?",
         "Not in the model. A property, an interface field, or neither?")
}

render_terms <- function(terms, action_input) {
  if (nrow(terms) == 0L) {
    body <- shiny::tags$span(class = "mb-caption", "No terms found yet.")
  } else {
    body <- shiny::tags$div(class = "mb-terms", unlist(lapply(seq_len(nrow(terms)), function(i) {
      term <- terms$term[i]
      if (terms$resolution[i] == "model") {
        stereotype <- if (is.na(terms$stereotype[i])) "" else sprintf(" «%s»", terms$stereotype[i])
        note <- if (identical(terms$source[i], "property")) {
          sprintf("Property of %s%s · in the model", terms$element_name[i], stereotype)
        } else {
          sprintf("%s%s · in the model", terms$element_name[i], stereotype)
        }
        return(list(shiny::tags$span(class = "mono", term), shiny::tags$span(note)))
      }
      if (terms$resolution[i] == "not_model") {
        return(list(shiny::tags$span(class = "mono", term), shiny::tags$span(class = "mb-caption", "Not a model term")))
      }
      buttons <- lapply(term_actions(terms$role[i]), function(action) {
        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small",
                           onclick = event_js(action_input, list(term = term, role = terms$role[i], action = action)),
                           unname(TERM_ACTION_LABELS[action]))
      })
      list(shiny::tags$span(class = "mono mb-gap", term),
           shiny::tags$div(style = "display:flex;flex-direction:column;gap:6px",
                           shiny::tags$span(class = "mb-gap", term_question(terms$role[i])),
                           shiny::tags$div(class = "mb-term-actions", buttons)))
    }), recursive = FALSE))
  }
  shiny::tags$section(
    class = "mb-panel", style = "font-size:13px",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Terms in this draft"),
                    shiny::tags$span(class = "mb-caption", "matched against the model by code")),
    body,
    shiny::tags$div(class = "mb-caption", "Anything added here starts with its value as [value] until you enter it.")
  )
}

render_outline <- function(outline, select_input, selected = NULL) {
  shiny::tags$div(class = "mb-outline", lapply(seq_len(nrow(outline)), function(i) {
    row <- outline[i, ]
    failing <- row$gate_pass == 0L && row$status == "draft"
    title <- if (is.na(row$short_title)) "" else row$short_title
    shiny::tags$button(
      type = "button",
      style = sprintf("padding-left:%dpx", 6L + 14L * row$depth),
      `aria-current` = if (identical(row$id, selected)) "true" else NULL,
      class = if (failing) "mb-gap" else NULL,
      onclick = event_js(select_input, row$id),
      shiny::tags$span(class = "mono", row$id), " ", title,
      if (failing && !is.na(row$gate_score)) sprintf(" · %d", row$gate_score)
    )
  }))
}

render_status_strip <- function(counts, nav_input) {
  shiny::tags$div(
    role = "status", `aria-label` = "Session status", class = "mb-status",
    shiny::tags$button(type = "button", onclick = event_js(nav_input, list(workspace = "Analyze")),
                       sprintf("%d unreviewed AI drafts · %d placeholders", counts$unreviewed, counts$placeholders))
  )
}

render_top_nav <- function(active, nav_input) {
  shiny::tags$nav(`aria-label` = "Workspaces", class = "mb-nav", lapply(WORKSPACES, function(workspace) {
    shiny::tags$button(type = "button", `aria-current` = if (identical(workspace, active)) "page" else NULL,
                       onclick = event_js(nav_input, list(workspace = workspace)), workspace)
  }))
}

render_findings <- function(findings) {
  shiny::tags$div(
    class = "mb-panel",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Quality findings"),
                    shiny::tags$span(class = "mono mb-push mb-caption", sprintf("score %d / 100", findings$score))),
    if (length(findings$findings) == 0L) {
      shiny::tags$span(class = "mb-caption", "No findings from the rules.")
    } else {
      shiny::tags$div(class = "mb-findings", unlist(lapply(findings$findings, function(f) {
        list(shiny::tags$span(class = "mono", style = "font-size:11px", "RULE"), shiny::tags$span(f))
      }), recursive = FALSE))
    },
    shiny::tags$div(class = "mb-caption", "RULE = deterministic check")
  )
}
