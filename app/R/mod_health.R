# Analyze › Health: readiness and model health, computed with no AI calls (handoff §5.10).

mod_health_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, shiny::tags$span(class = "mb-caption", "Computed from the model. This page makes no AI calls."),
               actions = shiny::actionButton(ns("recompute"), "Recompute", class = "mb-btn")),
    shiny::tags$div(class = "mb-scroll", shiny::uiOutput(ns("page")))
  )
}

health_link <- function(label, target, input_id, gap = FALSE) {
  shiny::tags$button(type = "button", class = paste("mb-linkish", if (gap) "mb-gap"), onclick = event_js(input_id, target), label)
}

# Every count above zero links to where it's worked on. A problem count is amber; a plain
# total is an ordinary link (UX review §2.24).
health_row <- function(label, value, target = NULL, input_id = NULL, problem = TRUE) {
  linked <- !is.null(target) && value > 0L
  amber <- linked && problem
  list(if (linked) health_link(label, target, input_id, amber) else shiny::tags$span(label),
       shiny::tags$span(class = paste("mono", if (amber) "mb-gap"), value))
}

health_card <- function(title, rows) {
  shiny::tags$section(class = "mb-card", shiny::tags$div(class = "mb-label", title),
                      shiny::tags$div(class = "mb-card-rows", rows))
}

# Where each kind of blocker gets fixed.
blocker_target <- function(kind, id, owner_kind = NA, owner_id = NA) {
  if (kind == "interface_field" || identical(owner_kind, "interface")) return(list(place = "interface", id = if (is.na(owner_id)) id else owner_id))
  if (kind == "property_value" && identical(owner_kind, "element")) return(list(place = "element", id = owner_id))
  if (kind == "element") return(list(place = "element", id = id))
  list(place = "behavior", id = id)
}

# The blocker groups, each a list of links to the fix. Health and the Create baseline
# dialog both show them (UX review §2.28).
render_blocker_groups <- function(blockers, input_id) {
  place_links <- lapply(seq_len(nrow(blockers$placeholders)), function(i) {
    row <- blockers$placeholders[i, ]
    health_link(row$text, blocker_target(row$entity_kind, row$entity_id, row$owner_kind, row$owner_id), input_id)
  })
  draft_links <- lapply(seq_len(nrow(blockers$unreviewed)), function(i) {
    row <- blockers$unreviewed[i, ]
    health_link(sprintf("%s · %s", row$label, gsub("_", " ", row$entity_kind)), blocker_target(row$entity_kind, row$entity_id), input_id)
  })
  stale_links <- lapply(seq_len(nrow(blockers$stale)), function(i) {
    row <- blockers$stale[i, ]
    health_link(sprintf("%s · %s", if (is.na(row$test_case_id)) row$id else row$test_case_id, row$text), list(place = "requirement", id = row$requirement_id), input_id)
  })
  group <- function(title, links, note = NULL) {
    shiny::tags$div(style = "display:flex;flex-direction:column;gap:6px", shiny::tags$b(title), links,
                    if (!is.null(note)) shiny::tags$span(class = "mb-caption", note))
  }
  draft_note <- sprintf("%d requirement drafts also contain placeholders. Drafts can't be committed and aren't part of a baseline.",
                        blockers$draft_placeholders)
  shiny::tags$div(class = "mb-banner-groups",
                  if (length(place_links) > 0L) group(sprintf("%d placeholders", length(place_links)), place_links, draft_note),
                  if (length(draft_links) > 0L) group(sprintf("%d unreviewed AI drafts, each needs your note", length(draft_links)), draft_links),
                  if (length(stale_links) > 0L) group(sprintf("%d piece%s of stale evidence", length(stale_links), if (length(stale_links) == 1L) "" else "s"), stale_links))
}

render_health <- function(health, input_id, next_id = "The next baseline") {
  blockers <- health$blockers
  count <- nrow(blockers$placeholders) + nrow(blockers$unreviewed) + nrow(blockers$stale)
  spec <- list(place = "spec")
  structure <- list(place = "structure")
  shiny::tags$div(
    class = "mb-health",
    if (count == 0L) {
      shiny::tags$section(class = "mb-banner mb-ready", shiny::tags$h1("Ready to baseline"))
    } else {
      shiny::tags$section(
        class = "mb-banner",
        shiny::tags$div(class = "mb-head", shiny::tags$h1(sprintf("%s can't be created yet", next_id)),
                        shiny::tags$span(class = "mb-gap", style = "font-weight:500", sprintf("%d blockers", count)),
                        shiny::tags$span(class = "mb-caption", "A baseline only holds what an engineer decided.")),
        render_blocker_groups(blockers, input_id)
      )
    },
    shiny::tags$div(
      class = "mb-cards",
      health_card("Requirements", c(
        health_row("Committed", health$requirements$committed, spec, input_id, problem = FALSE),
        health_row("Pass the gate", health$requirements$passing, spec, input_id, problem = FALSE),
        health_row("Fail the gate", health$requirements$failing, spec, input_id),
        health_row("Committed without a coach review", health$requirements$uncoached, spec, input_id),
        health_row("Committed but unallocated", health$requirements$unallocated, list(place = "trace"), input_id),
        health_row("No verification method", health$requirements$no_verification, spec, input_id)
      )),
      health_card("Architecture", c(
        health_row("Components", health$architecture$components, structure, input_id, problem = FALSE),
        health_row("Unconnected ports", health$architecture$unconnected_ports, structure, input_id),
        health_row("Components with no requirements", health$architecture$no_requirements, structure, input_id),
        health_row("Software with nothing to run on", health$architecture$unhosted, structure, input_id),
        health_row("Interfaces", health$architecture$interfaces, list(place = "interfaces"), input_id, problem = FALSE),
        health_row("Gate findings", health$architecture$gate_findings, structure, input_id)
      )),
      health_card("Functional", c(
        health_row("Functions", health$functional$functions, list(place = "functional"), input_id, problem = FALSE),
        health_row("Unallocated", health$functional$unallocated, list(place = "functional"), input_id),
        list(shiny::tags$span(class = if (health$functional$flows_realised < health$functional$flows) "mb-gap", "Flows with a physical path"),
             shiny::tags$span(class = paste("mono", if (health$functional$flows_realised < health$functional$flows) "mb-gap"),
                              sprintf("%d / %d", health$functional$flows_realised, health$functional$flows)))
      )),
      health_card("Behavior", c(
        health_row("Scenarios", health$behavior$scenarios, list(place = "scenarios"), input_id, problem = FALSE),
        health_row("Messages using a missing field", health$behavior$missing_fields, list(place = "scenarios"), input_id),
        health_row("Unhandled event/state pairs", health$behavior$unhandled, list(place = "machines"), input_id)
      )),
      health_card("Budgets (roll-up)", c(
        unlist(lapply(seq_len(nrow(health$budgets)), function(i) {
          budget <- health$budgets[i, ]
          list(shiny::tags$span(budget$label),
               shiny::tags$span(class = paste("mono", if (!budget$pass) "mb-gap"), sprintf("%s%s", budget$text, if (budget$pass) "" else " · FAIL")))
        }), recursive = FALSE),
        unlist(lapply(seq_len(nrow(health$analyses)), function(i) {
          row <- health$analyses[i, ]
          list(shiny::tags$span(class = if (!row$pass) "mb-gap", term_to_title(row$analysis)),
               shiny::tags$span(class = paste("mono", if (!row$pass) "mb-gap"), sprintf("%s%s", row$text, if (row$pass) "" else " · FAIL")))
        }), recursive = FALSE)
      )),
      health_card("AI contributions", c(
        health_row("Kept, with your note", health$ai$kept, list(place = "review"), input_id, problem = FALSE),
        health_row("Dismissed, with reason", health$ai$dismissed, list(place = "review"), input_id, problem = FALSE),
        health_row("Unreviewed", health$ai$unreviewed, list(place = "review"), input_id),
        health_row("Values the model tried to invent, blocked", health$ai$invented)
      ))
    )
  )
}

mod_health_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    health <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      db_dep(state, "behavior")
      db_dep(state, "ai")
      input$recompute
      model_health(con())
    })
    output$page <- shiny::renderUI(render_health(health(), ns("go"), next_baseline_id(con())))
    shiny::observeEvent(input$go, follow_blocker(state, input$go))
    invisible(list(health = health))
  })
}

# Goes to where a Health link's item is worked on.
follow_blocker <- function(state, target) {
  switch(
    target$place,
    spec = go_to(state, "Write", "Specification"),
    functional = go_to(state, "Model", "Functional"),
    trace = go_to(state, "Write", "Traceability"),
    requirement = {
      state$selected_requirement(target$id)
      go_to(state, "Write", "Specification")
    },
    structure = go_to(state, "Model", "Structure"),
    element = {
      state$selected_entity(list(kind = "element", ids = target$id))
      go_to(state, "Model", "Structure")
    },
    interface = {
      state$selected_interface(target$id)
      go_to(state, "Model", "Interfaces")
    },
    interfaces = go_to(state, "Model", "Interfaces"),
    scenarios = go_to(state, "Behavior", "Scenarios"),
    machines = go_to(state, "Behavior", "State machines"),
    review = go_to(state, "Analyze", "Review queue"),
    go_to(state, "Behavior")
  )
}
