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

# The gate ledger. Each row says what it found in words as well as points, and the hard
# rules that block a commit come first, in the ledger as well as above it (UX review §2.2).
render_gate <- function(gate) {
  if (!nzchar(gate$text)) {
    return(shiny::tags$section(class = "mb-panel mb-gate",
                               shiny::tags$span(class = "mb-label", "Gate · every edit · no AI"),
                               shiny::tags$span(class = "mb-caption", "Nothing to check yet.")))
  }
  blockers <- gate_blockers(gate)
  row <- function(points, mark, text, class = NULL) {
    list(shiny::tags$span(points), shiny::tags$span(class = paste("mb-mark", class), mark), shiny::tags$span(class = class, text))
  }
  hard <- lapply(blockers, function(b) row("—", "blocks", b, "mb-gap"))
  rows <- lapply(seq_len(nrow(gate$ledger)), function(i) {
    impact <- as.integer(gate$ledger$Impact[i])
    status <- isTRUE(gate$ledger$Status[i])
    mark <- if (status) "pass" else if (impact < 0L) "fail" else "n/a"
    row(format_points(impact), mark, ledger_label(gate$ledger$Item[i], status), if (mark == "fail") "mb-gap")
  })
  banned <- lapply(gate$banned_words_found, function(word) row("-10", "fail", sprintf('banned word "%s"', word), "mb-gap"))
  score <- as.integer(gate$score)
  verdict <- if (gate$passes_gate) sprintf("score %d · PASS", score) else if (length(blockers) > 0L) {
    sprintf("score %d · FAIL (blocked by %d rule%s)", score, length(blockers), if (length(blockers) == 1L) "" else "s")
  } else {
    sprintf("score %d · FAIL", score)
  }
  shiny::tags$section(
    class = "mb-panel mb-gate",
    shiny::tags$div(class = "mb-head",
                    shiny::tags$span(class = "mb-label", "Gate · every edit · no AI"),
                    shiny::tags$b(class = paste("mb-push", if (!gate$passes_gate) "mb-gap"), verdict)),
    shiny::tags$div(class = "mb-ledger", unlist(c(hard, rows, banned), recursive = FALSE))
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

# One line per requirement: the id in a fixed gutter, the title cut short with the whole
# of it in the tooltip, and a failing score in its own column (UX review §2.3).
render_outline <- function(outline, select_input, selected = NULL) {
  shiny::tags$div(class = "mb-outline", lapply(seq_len(nrow(outline)), function(i) {
    row <- outline[i, ]
    failing <- row$gate_pass == 0L && row$status == "draft"
    title <- if (is.na(row$short_title)) "" else row$short_title
    shiny::tags$button(
      type = "button", class = paste("mb-outline-row", if (failing) "mb-gap"),
      style = sprintf("padding-left:%dpx", 6L + 8L * row$depth),
      title = trimws(paste(row$id, title)),
      `aria-current` = if (identical(row$id, selected)) "true" else NULL,
      onclick = event_js(select_input, row$id),
      shiny::tags$span(class = "mono mb-oid", row$id), shiny::tags$span(class = "mb-otitle", title),
      shiny::tags$span(class = "mono mb-oscore", if (failing && !is.na(row$gate_score)) row$gate_score)
    )
  }))
}

render_status_strip <- function(counts, nav_input, coach = NULL, usage = NULL, policy = "tiered", stack = NULL) {
  last <- function(commands) if (length(commands) == 0L) NULL else commands[[length(commands)]]$label
  shiny::tags$div(
    role = "status", `aria-label` = "Session status", class = "mb-status",
    if (!is.null(coach)) {
      shiny::tags$span(class = NULL, "Coach: ", shiny::tags$span(class = if (!coach$local) "mb-amber-dark", if (coach$local) "local ready" else "local down"), " · ",
                       shiny::tags$span(if (coach$cloud) "cloud ready" else "cloud off"))
    },
    shiny::tags$span(sprintf("Policy: %s", policy)),
    shiny::tags$button(type = "button", onclick = event_js(nav_input, list(workspace = "Analyze", tab = "Health")),
                       sprintf("%d unreviewed AI drafts · %d placeholders", counts$unreviewed, counts$placeholders)),
    if (!is.null(usage)) shiny::tags$span(style = "margin-left:auto", sprintf("Today: %d calls · $%.2f", usage$calls, usage$cost)),
    # One Undo for the whole session, saying what it would undo (review D1).
    if (!is.null(stack)) {
      shiny::tagList(
        shiny::tags$button(type = "button", disabled = if (length(stack$undo) == 0L) NA, onclick = event_js("global_undo", "undo"),
                           `aria-label` = if (length(stack$undo) > 0L) paste("Undo", last(stack$undo)) else "Nothing to undo",
                           if (length(stack$undo) > 0L) sprintf("Undo %s", last(stack$undo)) else "Undo"),
        shiny::tags$button(type = "button", disabled = if (length(stack$redo) == 0L) NA, onclick = event_js("global_redo", "redo"),
                           `aria-label` = if (length(stack$redo) > 0L) paste("Redo", last(stack$redo)) else "Nothing to redo",
                           if (length(stack$redo) > 0L) sprintf("Redo %s", last(stack$redo)) else "Redo")
      )
    }
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

# --- AI panels (handoff §5.2) ---

ai_header <- function(title, source) {
  shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b(title),
                  shiny::tags$span(class = "mb-caption mb-push", source))
}

render_word_diff <- function(diff) {
  shiny::tags$div(class = "mb-diff", lapply(seq_len(nrow(diff)), function(i) {
    word <- diff$word[i]
    shiny::tagList(switch(diff$op[i], del = shiny::tags$del(word), ins = shiny::tags$ins(word), word), " ")
  }))
}

COACH_CRITERIA <- c(singular = "Singular", verifiable = "Verifiable", design_independent = "Design-independent",
                    conflicts = "Conflicts", fits_scope = "Fits the scope")

render_coach_panel <- function(review, failure, running, configured, ns) {
  if (!is.null(failure)) {
    return(shiny::tags$section(class = "mb-sketch-panel", ai_header("Coach", "couldn't run"), shiny::tags$span(failure$error),
                               shiny::tags$span(class = "mb-caption", "You can still commit on the gate alone; it's marked uncoached.")))
  }
  if (is.null(review)) {
    note <- if (running) "Checking…" else if (configured) "The coach reads a draft after it passes the gate." else
      "No coach is configured. Commit runs on the gate alone and marks the requirement uncoached. Pick a local model in Project to turn it on."
    return(shiny::tags$section(class = "mb-sketch-panel", ai_header("Coach", ""), shiny::tags$span(class = "mb-caption", note),
                               shiny::tags$div(shiny::actionButton(ns("deep_check"), "Deep check (cloud)", class = "mb-btn"))))
  }
  answer <- review$answer
  task <- if (grepl("deep", review$prompt_version %||% "", fixed = TRUE)) "req_deep_check" else "req_light_check"
  shiny::tags$section(
    class = "mb-sketch-panel",
    ai_header("Coach", if (task == "req_deep_check") "cloud · on demand" else "local · after the gate passed"),
    shiny::tags$div(class = "mb-card-rows", style = "font-size:12px", unlist(lapply(names(COACH_CRITERIA), function(name) {
      value <- answer$criteria[[name]]
      list(shiny::tags$span(COACH_CRITERIA[[name]]), shiny::tags$span(style = if (identical(value, "question")) "font-weight:600", value))
    }), recursive = FALSE)),
    lapply(answer$questions, function(question) {
      shiny::tags$div(class = "mb-question", shiny::tags$span(style = "font-size:15px", question$text),
                      if (nzchar(question$context)) shiny::tags$span(class = "mb-caption", question$context))
    }),
    lapply(answer$conflicts, function(conflict) {
      shiny::tags$div(class = "mb-question",
                      shiny::tags$span(if (is.na(conflict$requirement_id)) "Possible conflict" else sprintf("Conflict with %s", conflict$requirement_id)),
                      shiny::tags$span(class = "mb-caption", conflict$explanation))
    }),
    if (identical(answer$example$status, "ok")) {
      shiny::tags$details(shiny::tags$summary(style = "font-weight:500", "Show an example rewrite (word diff)"),
                          render_word_diff(answer$example$diff),
                          shiny::tags$div(class = "mb-caption", "An example only."))
    } else if (identical(answer$example$status, "failed")) {
      shiny::tags$div(class = "mb-caption", sprintf("The example didn't pass the gate: %s", paste(answer$example$reasons, collapse = " ")))
    },
    shiny::tags$div(class = "mb-term-actions",
                    shiny::actionButton(ns("deep_check"), "Deep check (cloud)", class = "mb-btn"),
                    shiny::actionButton(ns("coach_dismiss"), "Dismiss with reason", class = "mb-btn")),
    shiny::tags$div(class = "mono mb-caption", style = "font-size:11px",
                    sprintf("%s · %s · %s%s", review$provider_id %||% "", review$prompt_version %||% "",
                            if (is.na(review$duration_ms)) "" else sprintf("%.1f s", review$duration_ms / 1000),
                            if (isTRUE(review$cached)) " · cached by text" else ""), " ",
                    shiny::tags$button(type = "button", class = "mb-linkish", onclick = event_js(ns("see_prompt"), task), "See the prompt"))
  )
}

render_scope_gaps <- function(proposals, ns, running) {
  gaps <- proposals[proposals$task == "scope_gaps", , drop = FALSE]
  if (nrow(gaps) == 0L && !running) return(NULL)
  shiny::tags$section(
    class = "mb-panel",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Gaps the scope implies"), shiny::tags$span(class = "mb-caption", if (running) "Checking…" else "after your last commit")),
    lapply(seq_len(nrow(gaps)), function(i) {
      row <- gaps[i, ]
      shiny::tags$div(style = "display:flex;flex-direction:column;gap:6px", shiny::tags$span(row$title),
                      shiny::tags$div(class = "mb-term-actions",
                                      if (row$proposal_kind == "sketch_requirement") shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("gap_draft"), row$id), "Start from this in Write"),
                                      shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("gap_dismiss"), row$id), "Dismiss with reason")))
    })
  )
}
