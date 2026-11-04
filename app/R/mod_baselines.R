# Analyze › Baselines & impact (handoff §5.9).

mod_baselines_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(class = "mb-tabbar",
                    shiny::selectInput(ns("from"), "Compare", choices = character(0), width = "220px"),
                    shiny::tags$span(`aria-hidden` = "true", "→"),
                    shiny::selectInput(ns("to"), "with", choices = c("Working copy" = "working"), width = "220px"),
                    shiny::tags$div(class = "mb-actions",
                                    shiny::actionButton(ns("rollback"), "Roll back…", class = "mb-btn"),
                                    shiny::uiOutput(ns("create_button"), inline = TRUE))),
    shiny::tags$div(class = "mb-body",
                    shiny::tags$aside(class = "mb-left", style = "max-width:340px;flex-basis:300px", shiny::uiOutput(ns("cards"))),
                    shiny::tags$main(class = "mb-main", shiny::uiOutput(ns("impact")), shiny::uiOutput(ns("narrative"))))
  )
}

render_change_cards <- function(cards, counts, selected, input_id, baseline_label) {
  shiny::tagList(
    shiny::tags$div(class = "mb-label", sprintf("%d change%s since %s", length(cards), if (length(cards) == 1L) "" else "s", baseline_label)),
    if (length(cards) == 0L) shiny::tags$span(class = "mb-caption", "Nothing has changed."),
    lapply(seq_along(cards), function(i) {
      card <- cards[[i]]
      shiny::tags$button(
        type = "button", class = "mb-card-select", `aria-current` = if (identical(card$change_set, selected)) "true",
        onclick = event_js(input_id, card$change_set),
        shiny::tags$span(class = "mono", style = "font-size:11px;color:var(--ink-3)", sprintf("%s · %s", toupper(card$status), gsub("_", " ", card$kind))),
        shiny::tags$b(card$title),
        shiny::tags$span(class = "mono", style = "font-size:12px", card$change_text),
        if (length(card$entities) > 1L) shiny::tags$span(class = "mb-caption", paste("and", paste(vapply(card$entities[-1], function(e) sprintf("%s: %s", e$title, e$change_text), ""), collapse = "; "))),
        if (!is.null(counts[[card$change_set]])) shiny::tags$span(class = "mb-caption", counts[[card$change_set]]),
        if (!is.na(card$note)) shiny::tags$span(class = "mb-caption", sprintf("“%s”", card$note))
      )
    })
  )
}

render_impact_table <- function(items) {
  shiny::tags$div(class = "mb-table", shiny::tags$table(
    class = "mb-simple",
    shiny::tags$thead(shiny::tags$tr(lapply(c("Impacted element", "Path", "Hops", "Significance"), shiny::tags$th))),
    shiny::tags$tbody(lapply(seq_len(nrow(items)), function(i) {
      row <- items[i, ]
      significance <- if ("significance" %in% names(row)) row$significance else NA_character_
      shiny::tags$tr(
        shiny::tags$td(row$label, if (!is.na(row$stale)) shiny::tags$div(class = "mb-gap", style = "font-size:12px", sprintf("Evidence no longer current: %s", row$stale))),
        shiny::tags$td(class = "mono", style = "font-size:12px", row$path),
        shiny::tags$td(class = "mono", row$hops),
        shiny::tags$td(class = if (identical(significance, "High")) "mb-sig-high" else if (identical(significance, "Medium")) "mb-sig-medium",
                       title = if ("reason" %in% names(row) && !is.na(row$reason)) row$reason,
                       if (is.na(significance)) shiny::tags$span(class = "mb-caption", "not rated") else significance)
      )
    }))
  ))
}

render_narrative <- function(narrative, audience, set_input, source_line) {
  button <- function(value, label) {
    on <- identical(audience, value)
    shiny::tags$button(type = "button", role = "tab", `aria-pressed` = tolower(as.character(on)), `aria-selected` = tolower(as.character(on)),
                       onclick = event_js(set_input, value), label)
  }
  shiny::tags$section(
    class = "mb-sketch-panel mb-narrative",
    shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b("Impact summary"),
                    shiny::tags$div(role = "tablist", `aria-label` = "Audience", class = "mb-segmented mb-push", button("technical", "Technical"), button("stakeholder", "Stakeholder"))),
    if (identical(audience, "technical")) {
      shiny::tags$p(narrative$technical)
    } else {
      shiny::tagList(shiny::tags$p(narrative$stakeholder), if (!is.na(narrative$decision)) shiny::tags$p(shiny::tags$b("Decision needed: "), narrative$decision))
    },
    shiny::tags$div(class = "mono mb-caption", style = "font-size:11px", source_line, " · numbers from deterministic roll-up, not the model")
  )
}

mod_baselines_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    selected <- shiny::reactiveVal(NULL)
    audience <- shiny::reactiveVal("technical")
    ai_results <- shiny::reactiveVal(list())
    ai_error <- shiny::reactiveVal(NULL)

    baselines <- shiny::reactive({
      db_dep(state, "baseline")
      baseline_list(con())
    })
    shiny::observe({
      rows <- baselines()
      labels <- ifelse(rows$is_safety == 1L, sprintf("%s · safety", rows$id), sprintf("%s · %s", rows$id, rows$name))
      latest <- latest_baseline(con())
      shiny::updateSelectInput(session, "from", choices = stats::setNames(rows$id, labels), selected = if (is.na(latest)) NULL else latest)
      shiny::updateSelectInput(session, "to", choices = c("Working copy" = "working", stats::setNames(rows$id, labels)), selected = "working")
    })

    from_id <- shiny::reactive({
      value <- input$from
      if (is.null(value) || !nzchar(value)) latest_baseline(con()) else value
    })
    to_id <- shiny::reactive(if (is.null(input$to)) "working" else input$to)

    cards <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      db_dep(state, "baseline")
      from <- from_id()
      if (is.na(from)) return(list())
      compare_baselines(con(), from, to_id())
    })
    current_card <- shiny::reactive({
      all <- cards()
      if (length(all) == 0L) return(NULL)
      hit <- Filter(function(card) identical(card$change_set, selected()), all)
      if (length(hit) == 1L) hit[[1]] else all[[1]]
    })
    impact <- shiny::reactive({
      card <- current_card()
      if (is.null(card)) return(NULL)
      items <- card_impact(con(), card)
      known <- ai_results()[[card$change_set]]
      if (is.null(known)) known <- impact_ai_from_cache(con(), card, items)
      if (!is.null(known$items)) {
        items$significance <- known$items$significance[match(items$id, known$items$id)]
        items$reason <- known$items$reason[match(items$id, known$items$id)]
      }
      list(items = items, narrative = known$narrative, provider_id = known$provider_id, model = known$model)
    })

    output$cards <- shiny::renderUI({
      all <- cards()
      current <- current_card()
      counts <- lapply(all, function(card) {
        items <- card_impact(con(), card)
        known <- ai_results()[[card$change_set]]
        high <- if (!is.null(known$items)) sum(known$items$significance %in% "High") else NA
        sprintf("%d elements impacted%s", nrow(items), if (is.na(high)) "" else sprintf(" · %d high", high))
      })
      names(counts) <- vapply(all, function(card) card$change_set, "")
      render_change_cards(all, counts, if (is.null(current)) NULL else current$change_set, ns("select"), from_id())
    })
    shiny::observeEvent(input$select, selected(input$select))

    task <- shiny::ExtendedTask$new(function(connection, card, items) run_impact_ai_async(connection, card, items))
    shiny::observeEvent(input$rate, {
      ai_error(NULL)
      task$invoke(con(), current_card(), shiny::isolate(impact())$items[c("kind", "id", "label", "hops", "path", "stale")])
    })
    shiny::observeEvent(task$result(), {
      result <- task$result()
      if (isTRUE(result$ok) && !is.null(result$change_set)) {
        known <- ai_results()
        known[[result$change_set]] <- result
        ai_results(known)
      }
      if (!isTRUE(result$ok) || !is.null(result$error)) ai_error(result$error)
      invalidate_db(state, "ai")
    })

    output$impact <- shiny::renderUI({
      data <- impact()
      card <- current_card()
      if (is.null(data)) return(shiny::tags$p(class = "mb-caption", "There's nothing to compare yet."))
      scenarios <- impacted_scenarios(data$items)
      shiny::tags$section(
        class = "mb-inspector",
        shiny::tags$div(class = "mb-head", shiny::tags$h1(style = "margin:0;font-size:18px;font-weight:600", sprintf("Impact of %s", card$title)),
                        shiny::tags$span(class = "mb-caption", "Impact set found by graph traversal (complete) · significance rated by AI")),
        render_impact_table(data$items),
        shiny::tags$div(class = "mb-head",
                        if (nrow(scenarios) > 0L) shiny::tags$button(type = "button", class = "mb-linkish", onclick = event_js(ns("to_behavior"), card$change_set), sprintf("Behavioral impact: %d scenarios affected", nrow(scenarios))),
                        shiny::actionButton(ns("rate"), "Rate and explain with AI", class = "mb-btn mb-btn-ai"),
                        if (task$status() == "running") shiny::tags$span(class = "mb-caption", "Checking…"),
                        if (!is.null(ai_error())) shiny::tags$span(ai_error()))
      )
    })
    output$narrative <- shiny::renderUI({
      data <- impact()
      if (is.null(data) || is.null(data$narrative)) return(NULL)
      render_narrative(data$narrative, audience(), ns("set_audience"), sprintf("via %s", paste(c(data$provider_id, data$model), collapse = " · ")))
    })
    shiny::observeEvent(input$set_audience, audience(input$set_audience))
    shiny::observeEvent(input$to_behavior, {
      state$behavior_focus(input$to_behavior)
      go_to(state, "Behavior", "Behavioral impact")
    })

    # --- Create a baseline ---
    output$create_button <- shiny::renderUI({
      db_dep(state, "baseline")
      shiny::actionButton(ns("create"), sprintf("Create baseline %s…", next_baseline_id(con())), class = "mb-btn mb-btn-primary")
    })
    shiny::observeEvent(input$create, {
      blockers <- baseline_blockers(con())
      ready <- nrow(blockers) == 0L
      group <- function(key, title) {
        rows <- blockers[blockers$group == key, , drop = FALSE]
        if (nrow(rows) == 0L) return(NULL)
        shiny::tags$div(shiny::tags$b(sprintf("%d %s", nrow(rows), title)), shiny::tags$ul(lapply(rows$text, shiny::tags$li)))
      }
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Create baseline %s", next_baseline_id(con())),
        if (!ready) {
          shiny::tagList(shiny::tags$p(class = "mb-gap", sprintf("It can't be created yet: %d blockers.", nrow(blockers))),
                         group("placeholder", "placeholders"), group("unreviewed", "unreviewed AI drafts"), group("stale", "stale evidence"))
        },
        if (ready) shiny::textInput(ns("baseline_name"), "Name"),
        if (ready) shiny::textAreaInput(ns("baseline_note"), "Note"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("create_confirm"), "Create baseline", class = "mb-btn mb-btn-primary", disabled = !ready))
      ))
    })
    shiny::observeEvent(input$create_confirm, {
      name <- trimws(if (is.null(input$baseline_name)) "" else input$baseline_name)
      if (!nzchar(name)) return(refusal_toast("A baseline needs a name."))
      result <- guarded_edit(create_baseline(con(), name, input$baseline_note))
      if (is.null(result)) return()
      if (!isTRUE(result$ok)) return(refusal_toast("New blockers appeared, so it wasn't created."))
      shiny::removeModal()
      invalidate_db(state, "baseline")
    })

    # --- Roll back ---
    shiny::observeEvent(input$rollback, {
      from <- from_id()
      if (is.na(from)) return(refusal_toast("There's no baseline to roll back to."))
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Roll back to %s?", from),
        shiny::tags$p(sprintf("The model goes back to how it was at %s, as new revisions: history isn't rewritten. Draft requirements stay as they are.", from)),
        shiny::tags$p(sprintf("It first saves the current state as safety baseline %s. Roll back to that to undo this.", next_baseline_id(con()))),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("rollback_confirm"), sprintf("Roll back to %s", from), class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$rollback_confirm, {
      shiny::removeModal()
      result <- guarded_edit(rollback_to_baseline(con(), from_id()))
      if (is.null(result)) return()
      # Undo can't step back across this: rolling back to the safety baseline is the way.
      state$commands(new_command_stack())
      selected(NULL)
      ai_results(list())
      invalidate_db(state, c("baseline", "model", "spec", "behavior"))
    })

    invisible(list(cards = cards, impact = impact, card = current_card, task = task))
  })
}
