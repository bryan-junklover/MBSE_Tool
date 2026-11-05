# Behavior › Scenarios, State machines and Behavioral impact (handoff §5.7, §5.8).

STEP_KINDS <- c("Message" = "message", "Self call" = "self", "Note" = "note",
                "Fragment start" = "fragment_open", "Else" = "fragment_else", "Fragment end" = "fragment_close")
FRAGMENT_KINDS <- c("alt" = "alt", "opt" = "opt", "loop" = "loop", "par" = "par")

blank_of <- function(x) if (is.null(x) || length(x) == 0L || is.na(x)) "" else as.character(x)

review_block <- function(ns, input_name, note_id) {
  shiny::tags$div(
    class = "mb-sketch-panel",
    shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$span("This came from the AI. Say in your own words why it belongs.")),
    shiny::tags$textarea(id = ns(note_id), rows = 2, `aria-label` = "Why it belongs", style = "width:100%"),
    shiny::tags$button(type = "button", class = "mb-btn",
                       onclick = sprintf("Shiny.setInputValue('%s', {note: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})", ns(input_name), ns(note_id)),
                       "Review, with my note")
  )
}

# Positions from a diagram event, as a data frame.
event_positions <- function(event) do.call(rbind, lapply(event$positions, function(p) data.frame(id = p$id, x = p$x, y = p$y)))

record_diagram_layout <- function(state, view_id, positions, kind, undoable) {
  con <- shiny::isolate(state$con())
  before <- layout_rows(con, view_id, positions$id)
  save_layout(con, view_id, layout_items(positions, item_kind = kind))
  if (undoable) state$commands(push_command(shiny::isolate(state$commands()), layout_command(view_id, before, layout_rows(con, view_id, positions$id), "layout")))
}

# The step table holds what a row needs at a glance: kind, ends, label and timing. The
# interface, field and guard are edited in the step inspector (UX review §2.17).
render_step_table <- function(steps, lifelines, selected, ns) {
  lifeline_choices <- stats::setNames(lifelines$id, lifelines$name)
  shiny::tags$div(class = "mb-table", shiny::tags$table(
    class = "mb-simple mb-seq-table",
    shiny::tags$thead(shiny::tags$tr(lapply(c("", "Kind", "From → To", "Label", "Timing (s)", ""), shiny::tags$th))),
    shiny::tags$tbody(`data-reorder-input` = ns("reorder"), lapply(seq_len(nrow(steps)), function(i) {
      step <- steps[i, ]
      field <- function(name) change_js(ns("step_field"), list(id = step$id, field = name))
      is_message <- step$step_kind %in% c("message", "self")
      has_ends <- step$step_kind %in% c("message", "self", "note")
      classes <- paste(if (step$review_state == "unreviewed") "mb-unreviewed", if (identical(step$id, selected)) "mb-row-selected", if (isTRUE(step$field_missing)) "mb-field-missing-row")
      shiny::tags$tr(
        `data-row-id` = step$id, draggable = "true", class = classes,
        onclick = sprintf("if (event.target.tagName === 'TD') { %s }", event_js(ns("select_step"), step$id)),
        shiny::tags$td(title = "Drag to reorder", "⋮⋮"),
        shiny::tags$td(select_tag(STEP_KINDS, step$step_kind, field("step_kind"), label = "Kind")),
        shiny::tags$td(
          class = "mb-ends",
          if (has_ends) select_tag(lifeline_choices, blank_of(step$from_lifeline_id), field("from_lifeline_id"), "—", label = "From")
          else if (step$step_kind == "fragment_open") select_tag(FRAGMENT_KINDS, blank_of(step$fragment_kind), field("fragment_kind"), label = "Fragment kind"),
          if (step$step_kind == "message") shiny::tags$span(`aria-hidden` = "true", "→"),
          if (step$step_kind == "message") select_tag(lifeline_choices, blank_of(step$to_lifeline_id), field("to_lifeline_id"), "—", label = "To")
        ),
        shiny::tags$td(shiny::tags$input(type = "text", value = step$label, `aria-label` = "Label", onchange = field("label")),
                       if (isTRUE(step$field_missing)) shiny::tags$div(class = "mb-gap", style = "font-size:12px", sprintf("Field %s isn't on its interface", blank_of(step$field_name)))),
        shiny::tags$td(if (is_message) shiny::tags$input(type = "text", placeholder = "[value]", `aria-label` = "Timing in seconds", onchange = field("timing"), class = "mb-timing",
                                                         value = switch(step$timing_state, set = format(step$timing_s, trim = TRUE), placeholder = "[value]", ""))),
        shiny::tags$td(shiny::tags$details(
          class = "mb-menu mb-menu-end",
          shiny::tags$summary(class = "mb-btn mb-btn-small", `aria-label` = paste("Step actions:", step$label), "⋯"),
          shiny::tags$div(class = "mb-menu-list",
                          shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("move_step"), list(id = step$id, by = -1L)), "Move up"),
                          shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("move_step"), list(id = step$id, by = 1L)), "Move down"),
                          confirm_button("Delete…", ns("delete_step"), step$id, "Delete this step?", sprintf("“%s” goes from the scenario. Undo brings it back.", step$label), confirm = "Delete step"))
        ))
      )
    }))
  ))
}

# Everything about one step, in the right-hand column.
render_step_inspector <- function(step, lifelines, interfaces, ns) {
  if (is.null(step) || nrow(step) != 1L) return(shiny::tags$p(class = "mb-caption", "Select a step in the diagram or the table to edit it."))
  field <- function(name) change_js(ns("step_field"), list(id = step$id, field = name))
  lifeline_choices <- stats::setNames(lifelines$id, lifelines$name)
  is_message <- step$step_kind %in% c("message", "self")
  shiny::tags$section(
    class = "mb-inspector",
    shiny::tags$div(class = "mb-head", shiny::tags$b(names(STEP_KINDS)[match(step$step_kind, STEP_KINDS)]), shiny::tags$span(class = "mono mb-caption", step$id)),
    if (step$review_state == "unreviewed") review_block(ns, "review_step", "step_note"),
    labelled("Label", shiny::tags$input(type = "text", value = step$label, onchange = field("label"))),
    labelled("Kind", select_tag(STEP_KINDS, step$step_kind, field("step_kind"))),
    if (step$step_kind %in% c("message", "self", "note")) labelled("From", select_tag(lifeline_choices, blank_of(step$from_lifeline_id), field("from_lifeline_id"), "—")),
    if (step$step_kind == "message") labelled("To", select_tag(lifeline_choices, blank_of(step$to_lifeline_id), field("to_lifeline_id"), "—")),
    if (step$step_kind == "fragment_open") labelled("Fragment kind", select_tag(FRAGMENT_KINDS, blank_of(step$fragment_kind), field("fragment_kind"))),
    if (is_message) labelled("Interface", select_tag(stats::setNames(interfaces$id, interfaces$name), blank_of(step$interface_id), field("interface_id"), "None")),
    if (is_message) labelled("Field", shiny::tags$input(type = "text", value = blank_of(step$field_name), class = if (isTRUE(step$field_missing)) "mb-field-missing", onchange = field("field_name"))),
    if (isTRUE(step$field_missing)) shiny::tags$span(class = "mb-gap", "That field isn't on the interface. The Consistency line offers to add it."),
    if (is_message || step$step_kind %in% c("fragment_open", "fragment_else")) labelled("Guard", shiny::tags$input(type = "text", class = "mono", value = blank_of(step$guard), onchange = field("guard"))),
    if (is_message) labelled("Timing (s)", shiny::tags$input(type = "text", placeholder = "[value]", onchange = field("timing"),
                                                            value = switch(step$timing_state, set = format(step$timing_s, trim = TRUE), placeholder = "[value]", ""))),
    shiny::tags$div(class = "mb-head",
                    shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("move_step"), list(id = step$id, by = -1L)), "Move up"),
                    shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("move_step"), list(id = step$id, by = 1L)), "Move down")),
    danger_zone(confirm_button("Delete step…", ns("delete_step"), step$id, "Delete this step?", sprintf("“%s” goes from the scenario. Undo brings it back.", step$label), confirm = "Delete step"))
  )
}

render_consistency <- function(check, ns) {
  shiny::tags$section(
    class = "mb-consistency",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Consistency"), shiny::tags$span(class = if (check$mapped < check$total) "mb-gap", sprintf("%d of %d messages map to defined interfaces", check$mapped, check$total)),
                    shiny::tags$span(class = "mono mb-caption", "code · every edit")),
    lapply(check$problems, function(problem) {
      shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-gap", problem$text),
                      if (problem$kind == "field") {
                        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("add_field"), list(interface = problem$interface_id, field = problem$field_name)), "Add field to interface")
                      })
    })
  )
}

mod_scenarios_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, actions = shiny::tagList(
      shiny::uiOutput(ns("sketch_status"), inline = TRUE),
      shiny::actionButton(ns("new"), "New scenario…", class = "mb-btn"),
      # The AI sketch has its own dialog, with room for a proper description (§2.18).
      shiny::actionButton(ns("sketch_open"), "Sketch with AI…", class = "mb-btn mb-btn-ai"))),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "scenarios.list", "Scenarios", width = 260, shiny::uiOutput(ns("list"))),
      shiny::tags$main(
        class = "mb-main",
        shiny::uiOutput(ns("header")),
        shiny::conditionalPanel(
          "output.view_kind == 'sequence'", ns = ns, class = "mb-fill",
          shiny::tags$div(class = "mb-head",
                          shiny::actionButton(ns("add_lifeline"), "+ Lifeline", class = "mb-btn mb-btn-small"),
                          shiny::actionButton(ns("add_message"), "+ Message", class = "mb-btn mb-btn-small"),
                          shiny::actionButton(ns("add_fragment"), "+ Fragment", class = "mb-btn mb-btn-small"),
                          shiny::actionButton(ns("add_note"), "+ Note", class = "mb-btn mb-btn-small"),
                          shiny::tags$span(class = "mb-caption", "Drag the bar below the diagram to share the space with the steps.")),
          # Diagram above, steps below, with a bar between them to share the height (§2.17).
          shiny::tags$div(class = "mb-pane-top", shiny::uiOutput(ns("sequence"))),
          shiny::tags$div(class = "mb-hsplit", `data-key` = "scenarios", role = "separator", `aria-orientation` = "horizontal", `aria-label` = "Resize the diagram and steps"),
          shiny::tags$div(class = "mb-pane-bottom", shiny::uiOutput(ns("steps")))
        ),
        shiny::conditionalPanel(
          "output.view_kind == 'activity'", ns = ns, class = "mb-fill",
          shiny::tags$div(class = "mb-head",
                          lapply(c("action", "decision", "initial", "final"), function(kind) shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("add_node"), kind), paste("+", tools::toTitleCase(kind)))),
                          shiny::actionButton(ns("auto_layout"), "Auto-layout", class = "mb-btn mb-btn-small"),
                          shiny::tags$span(class = "mb-caption", "Drag between a node's handles to connect it.")),
          shiny::tags$div(class = "mb-diagram", mbdiagramOutput(ns("activity")))
        )
      ),
      side_column("right", "scenarios.inspector", "Step", width = 340, auto = TRUE, shiny::uiOutput(ns("inspector")))
    )
  )
}

mod_scenarios_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    current <- shiny::reactiveVal(NULL)
    view <- shiny::reactiveVal(NULL)
    selected_step <- shiny::reactiveVal(NULL)
    selected_item <- shiny::reactiveVal(list(kind = "none", ids = character(0)))
    redraw <- shiny::reactiveVal(0L)

    scenarios <- shiny::reactive({
      db_dep(state, "behavior")
      scenario_list(con())
    })
    scenario <- shiny::reactive({
      rows <- scenarios()
      if (nrow(rows) == 0L) return(NULL)
      at <- match(current(), rows$id)
      rows[if (length(at) == 1L && !is.na(at)) at else 1L, , drop = FALSE]
    })
    view_kind <- shiny::reactive({
      row <- scenario()
      if (is.null(row)) return("none")
      available <- c(if (row$has_sequence == 1L) "sequence", if (row$has_activity == 1L) "activity")
      if (length(available) == 0L) return("none")
      if (!is.null(view()) && view() %in% available) view() else available[1]
    })
    output$view_kind <- shiny::reactive(view_kind())
    shiny::outputOptions(output, "view_kind", suspendWhenHidden = FALSE)

    output$list <- shiny::renderUI({
      rows <- scenarios()
      on <- scenario()
      shiny::tagList(shiny::tags$div(class = "mb-label", "Scenarios"), lapply(seq_len(nrow(rows)), function(i) {
        shiny::tags$button(type = "button", class = "mb-card-select", `aria-current` = if (identical(rows$id[i], on$id)) "true", onclick = event_js(ns("pick"), rows$id[i]),
                           shiny::tags$span(class = "mono mb-caption", sprintf("%s · %s", rows$id[i], if (rows$has_activity[i] == 1L) "activity" else "sequence")),
                           shiny::tags$b(rows$name[i]),
                           if (rows$review_state[i] == "unreviewed") shiny::tags$span(class = "mb-caption", "Unreviewed AI draft"))
      }))
    })
    shiny::observeEvent(input$pick, {
      current(input$pick)
      selected_step(NULL)
      selected_item(list(kind = "none", ids = character(0)))
    })
    output$header <- shiny::renderUI({
      row <- scenario()
      if (is.null(row)) return(shiny::tags$p(class = "mb-caption", "No scenarios yet."))
      both <- row$has_sequence == 1L && row$has_activity == 1L
      shiny::tagList(
        shiny::tags$div(class = "mb-head", shiny::tags$h1(style = "margin:0;font-size:18px;font-weight:600", row$name), shiny::tags$span(class = "mono mb-caption", row$id),
                        if (both) shiny::tags$div(class = "mb-segmented mb-push", lapply(c("sequence", "activity"), function(kind) {
                          shiny::tags$button(type = "button", `aria-pressed` = tolower(as.character(identical(view_kind(), kind))), onclick = event_js(ns("set_view"), kind), tools::toTitleCase(kind))
                        }))),
        if (!is.na(row$description)) shiny::tags$p(class = "mb-caption", row$description),
        if (row$review_state == "unreviewed") review_block(ns, "review_scenario", "scenario_note")
      )
    })
    shiny::observeEvent(input$set_view, view(input$set_view))
    shiny::observeEvent(input$review_scenario, behavior_edit(state, review_entity(con(), "scenario", scenario()$id, input$review_scenario$note), "review"))

    shiny::observeEvent(input$new, {
      shiny::showModal(shiny::modalDialog(
        title = "New scenario", shiny::textInput(ns("new_name"), "Name"), shiny::selectInput(ns("new_view"), "As", c("Sequence" = "sequence", "Activity" = "activity"), selectize = FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("new_confirm"), "Create", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$new_confirm, {
      activity <- identical(input$new_view, "activity")
      written <- behavior_edit(state, new_scenario(con(), input$new_name, has_sequence = !activity, has_activity = activity), "new scenario")
      if (is.null(written)) return()
      shiny::removeModal()
      current(written$id)
    })

    # --- Sequence ---
    steps <- shiny::reactive({
      db_dep(state, "behavior")
      db_dep(state, "model")
      row <- scenario()
      if (is.null(row)) return(NULL)
      scenario_steps(con(), row$id)
    })
    lifelines <- shiny::reactive({
      db_dep(state, "behavior")
      db_dep(state, "model")
      row <- scenario()
      if (is.null(row)) return(NULL)
      scenario_lifelines(con(), row$id)
    })
    output$sequence <- shiny::renderUI({
      row <- scenario()
      if (is.null(row) || view_kind() != "sequence") return(NULL)
      seqview(lifelines(), steps(), selected_step(), ns("select_step"))
    })
    output$steps <- shiny::renderUI({
      row <- scenario()
      if (is.null(row) || view_kind() != "sequence") return(NULL)
      shiny::tagList(
        render_consistency(scenario_consistency(con(), row$id), ns),
        render_step_table(steps(), lifelines(), selected_step(), ns)
      )
    })
    output$inspector <- shiny::renderUI({
      if (view_kind() == "activity") return(node_inspector())
      chosen <- selected_step()
      all <- steps()
      step <- if (is.null(chosen) || is.null(all)) NULL else all[all$id == chosen, , drop = FALSE]
      interfaces <- DBI::dbGetQuery(con(), "SELECT id, name FROM interface ORDER BY name")
      render_step_inspector(step, lifelines(), interfaces, ns)
    })
    shiny::observeEvent(input$select_step, selected_step(input$select_step))
    shiny::observe(reveal_column(session, "inspector", !is.null(selected_step()) || length(selected_item()$ids) > 0L))
    shiny::observeEvent(input$review_step, behavior_edit(state, review_entity(con(), "seq_step", selected_step(), input$review_step$note), "review"))
    shiny::observeEvent(input$step_field, behavior_edit(state, update_step_field(con(), input$step_field$id, input$step_field$field, input$step_field$value), "edit step"))
    shiny::observeEvent(input$add_field, behavior_edit(state, add_field_for_step(con(), input$add_field$interface, input$add_field$field), "add field"))
    shiny::observeEvent(input$add_message, behavior_edit(state, add_step(con(), scenario()$id, "message"), "add step"))
    shiny::observeEvent(input$add_note, behavior_edit(state, add_step(con(), scenario()$id, "note"), "add step"))
    shiny::observeEvent(input$add_fragment, behavior_edit(state, add_step(con(), scenario()$id, "fragment"), "add fragment"))
    shiny::observeEvent(input$delete_step, {
      behavior_edit(state, delete_entity(con(), "seq_step", input$delete_step), "delete step")
      selected_step(NULL)
    })
    apply_order <- function(ids) {
      written <- behavior_edit(state, reorder_steps(con(), scenario()$id, ids), "reorder")
      # A refused drag is still on screen until the table redraws.
      if (is.null(written)) invalidate_db(state, "behavior")
    }
    shiny::observeEvent(input$reorder, apply_order(as.character(unlist(input$reorder$ids))))
    shiny::observeEvent(input$move_step, {
      ids <- steps()$id
      at <- match(input$move_step$id, ids)
      to <- at + as.integer(input$move_step$by)
      if (is.na(at) || to < 1L || to > length(ids)) return()
      ids[c(at, to)] <- ids[c(to, at)]
      apply_order(ids)
    })
    shiny::observeEvent(input$add_lifeline, {
      taken <- scenario_lifelines(con(), scenario()$id)$element_id
      elements <- DBI::dbGetQuery(con(), "SELECT id, name FROM element WHERE kind IN ('component', 'external') ORDER BY name")
      elements <- elements[!elements$id %in% taken, , drop = FALSE]
      shiny::showModal(shiny::modalDialog(
        title = "Add a lifeline", shiny::selectInput(ns("lifeline_element"), "Element", stats::setNames(elements$id, elements$name), selectize = FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("lifeline_confirm"), "Add", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$lifeline_confirm, {
      shiny::removeModal()
      behavior_edit(state, add_lifeline(con(), scenario()$id, input$lifeline_element), "add lifeline")
    })

    # --- Activity ---
    activity_view <- shiny::reactive({
      db_dep(state, "behavior")
      redraw()
      row <- scenario()
      if (is.null(row) || view_kind() != "activity") return(NULL)
      activity_diagram(con(), row$id)
    })
    output$activity <- renderMbdiagram({
      shiny::req(activity_view())
      mbdiagram(c(activity_view(), list(selected = list())))
    })
    shiny::observeEvent(input$activity_event, {
      event <- input$activity_event
      view_id <- activity_view_id(scenario()$id)
      switch(
        event$type,
        undo = run_command(state, undo_command, redraw),
        redo = run_command(state, redo_command, redraw),
        select = selected_item(list(kind = event$kind, ids = as.character(unlist(event$ids)))),
        move = record_diagram_layout(state, view_id, data.frame(id = event$id, x = event$x, y = event$y), "act_node", TRUE),
        layout = {
          rows <- event_positions(event)
          if (!is.null(rows)) record_diagram_layout(state, view_id, rows, "act_node", isTRUE(event$user))
        },
        connect = {
          written <- behavior_edit(state, add_act_edge(con(), scenario()$id, event$source_id, blank_of(event$target_id)), "connect")
          if (is.null(written)) redraw(redraw() + 1L)
        }
      )
    })
    shiny::observeEvent(input$auto_layout, mbdiagram_send(session, "activity", "auto_layout"))
    shiny::observeEvent(input$add_node, behavior_edit(state, add_act_node(con(), scenario()$id, input$add_node), "add node"))

    node_inspector <- shiny::reactive({
      db_dep(state, "behavior")
      item <- selected_item()
      if (length(item$ids) != 1L) return(shiny::tags$p(class = "mb-caption", "Select a node or edge to edit it."))
      field <- function(name) change_js(ns("item_field"), list(id = item$ids, field = name, kind = item$kind))
      if (item$kind == "connector") {
        edge <- DBI::dbGetQuery(con(), "SELECT * FROM act_edge WHERE id = ?", params = list(item$ids))
        if (nrow(edge) == 0L) return(NULL)
        return(shiny::tags$section(class = "mb-inspector", shiny::tags$b("Edge"),
                                   shiny::tags$label("Guard", shiny::tags$input(type = "text", value = blank_of(edge$guard), onchange = field("guard"))),
                                   danger_zone(confirm_button("Delete edge…", ns("delete_item"), list(kind = "act_edge", id = item$ids), "Delete this edge?",
                                                              "The two nodes are no longer connected. Undo brings it back.", confirm = "Delete edge"))))
      }
      node <- activity_nodes(con(), scenario()$id)
      node <- node[node$id == item$ids, , drop = FALSE]
      if (nrow(node) == 0L) return(NULL)
      elements <- DBI::dbGetQuery(con(), "SELECT id, name FROM element WHERE kind IN ('component', 'external') ORDER BY name")
      reads <- node_reads(node$reads_json)
      readable <- readable_properties(con())
      read_keys <- paste(readable$owner_id, readable$property, sep = "|")
      shiny::tags$section(
        class = "mb-inspector", shiny::tags$div(class = "mb-head", shiny::tags$b(tools::toTitleCase(node$node_kind)), shiny::tags$span(class = "mono mb-caption", node$id)),
        if (node$review_state == "unreviewed") review_block(ns, "review_node", "node_note"),
        if (!node$node_kind %in% c("initial", "final")) shiny::tags$label("Label", shiny::tags$input(type = "text", value = blank_of(node$label), onchange = field("label"))),
        if (!node$node_kind %in% c("initial", "final")) shiny::tags$label("Performer", select_tag(stats::setNames(elements$id, elements$name), blank_of(node$performer_element_id), field("performer_element_id"), "None")),
        shiny::tags$div(class = "mb-label", "Reads"),
        lapply(reads, function(read) {
          shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mono", sprintf("%s.%s", readable$element[match(read$owner_id, readable$owner_id)], read$property)),
                          shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("remove_read"), list(id = node$id, owner = read$owner_id, property = read$property)), "Remove"))
        }),
        shiny::tags$label("Add what it reads", select_tag(stats::setNames(read_keys, sprintf("%s · %s", readable$element, readable$label)), "", change_js(ns("add_read"), list(id = node$id)), "Choose…")),
        danger_zone(confirm_button("Delete node…", ns("delete_item"), list(kind = "act_node", id = node$id), "Delete this node?",
                                   sprintf("“%s” and its edges go from the activity. Undo brings them back.", if (is.na(node$label)) node$node_kind else node$label), confirm = "Delete node"))
      )
    })
    shiny::observeEvent(input$item_field, {
      change <- input$item_field
      value <- trimws(change$value)
      fields <- stats::setNames(list(if (nzchar(value)) value else NA_character_), change$field)
      if (identical(change$kind, "connector")) {
        behavior_edit(state, write_act_edge(con(), change$id, fields, note = "Edited edge"), "edit edge")
      } else {
        behavior_edit(state, write_act_node(con(), change$id, fields, note = "Edited node"), "edit node")
      }
    })
    shiny::observeEvent(input$delete_item, {
      behavior_edit(state, delete_entity(con(), input$delete_item$kind, input$delete_item$id), "delete")
      selected_item(list(kind = "none", ids = character(0)))
    })
    shiny::observeEvent(input$review_node, behavior_edit(state, review_entity(con(), "act_node", selected_item()$ids, input$review_node$note), "review"))
    current_reads <- function(id) node_reads(DBI::dbGetQuery(con(), "SELECT reads_json FROM act_node WHERE id = ?", params = list(id))$reads_json)
    shiny::observeEvent(input$add_read, {
      parts <- strsplit(input$add_read$value, "|", fixed = TRUE)[[1]]
      if (length(parts) != 2L) return()
      reads <- c(current_reads(input$add_read$id), list(list(owner_kind = "element", owner_id = parts[1], property = parts[2])))
      behavior_edit(state, set_node_reads(con(), input$add_read$id, unique(reads)), "edit reads")
    })
    shiny::observeEvent(input$remove_read, {
      gone <- input$remove_read
      reads <- Filter(function(read) !(identical(read$owner_id, gone$owner) && identical(read$property, gone$property)), current_reads(gone$id))
      behavior_edit(state, set_node_reads(con(), gone$id, reads), "edit reads")
    })

    # --- AI sketch ---
    sketch_task <- shiny::ExtendedTask$new(function(connection, description, name, kind) run_behavior_sketch_async(connection, description, name, kind))
    sketch_message <- shiny::reactiveVal(NULL)
    sketch_kind <- shiny::reactiveVal("sequence")
    shiny::observeEvent(input$sketch_open, {
      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::tags$span(class = "mb-badge-ai", "AI"), " Sketch an unreviewed draft"), size = "l",
        shiny::tags$p(class = "mb-caption", "The AI drafts structure only. Every step comes in unreviewed, timings come in as [value], and anything the model doesn't have becomes a question in the Review queue."),
        shiny::textInput(ns("sketch_name"), "Name", width = "100%"),
        shiny::textAreaInput(ns("sketch_text"), "What happens, in your words", rows = 6, width = "100%"),
        shiny::selectInput(ns("sketch_view"), "As", c("Sequence" = "sequence", "Activity" = "activity"), selectize = FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("sketch"), "Sketch", class = "mb-btn mb-btn-ai"))
      ))
    })
    shiny::observeEvent(input$sketch, {
      text <- trimws(input$sketch_text)
      name <- trimws(input$sketch_name)
      if (!nzchar(text) || !nzchar(name)) return(refusal_toast("A sketch needs a name and a description."))
      shiny::removeModal()
      sketch_message(NULL)
      sketch_kind(input$sketch_view)
      sketch_task$invoke(con(), text, name, input$sketch_view)
    })
    shiny::observeEvent(sketch_task$result(), {
      result <- sketch_task$result()
      if (isTRUE(result$ok) && !is.null(result$sketch)) {
        current(result$sketch$id)
        view(sketch_kind())
        n <- length(result$sketch$questions)
        sketch_message(if (n > 0L) sprintf("Drafted. %d question%s went to the Review queue: it needs something the model doesn't have.", n, if (n == 1L) "" else "s") else "Drafted. Every step needs your review.")
        invalidate_db(state, c("behavior", "model", "ai"))
      } else {
        sketch_message(if (is.null(result$error)) "The sketch didn't come back." else result$error)
        invalidate_db(state, "ai")
      }
    })
    output$sketch_status <- shiny::renderUI({
      if (sketch_task$status() == "running") return(shiny::tags$span(class = "mb-caption", "Sketching…"))
      if (!is.null(sketch_message())) shiny::tags$span(class = "mb-caption", sketch_message())
    })

    invisible(list(scenario = scenario, steps = steps, view_kind = view_kind, sketch_task = sketch_task))
  })
}

# --- State machines ---

render_trace <- function(trace) {
  if (length(trace) == 0L) return(shiny::tags$p(class = "mb-caption", "Nothing has happened yet."))
  shiny::tags$ol(class = "mb-trace", `aria-label` = "Trace, newest first", lapply(rev(trace), function(row) {
    shiny::tags$li(class = if (row$level != "normal") row$level, sprintf("%.1f s  %s", row$t, row$note))
  }))
}

render_checks <- function(rows, ns) {
  shiny::tags$section(
    class = "mb-inspector",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Requirement checks"), shiny::actionButton(ns("run_all"), "Run and record", class = "mb-btn mb-btn-small mb-push")),
    if (nrow(rows) == 0L) shiny::tags$span(class = "mb-caption", "No assertions yet."),
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      recorded <- if (is.na(row$result)) "Not recorded yet" else if (row$current) sprintf("Recorded %s, this model", toupper(row$result)) else sprintf("Recorded %s on an earlier model", toupper(row$result))
      shiny::tags$div(
        shiny::tags$div(class = "mono", style = "font-size:12px", paste(c(row$id, if (!is.na(row$requirement_id)) row$requirement_id), collapse = " · ")),
        shiny::tags$div(row$description),
        shiny::tags$div(class = if (row$live_result == "pass") "mb-check-pass" else "mb-check-fail", sprintf("%s · %s", toupper(row$live_result), row$live_message)),
        shiny::tags$div(class = "mb-caption", recorded)
      )
    }),
    shiny::actionButton(ns("add_assertion"), "Add assertion…", class = "mb-btn mb-btn-small")
  )
}

render_completeness <- function(pairs, ns) {
  shiny::tags$section(
    class = "mb-inspector",
    shiny::tags$div(class = "mb-head", shiny::tags$b("Completeness"), shiny::tags$span(class = "mono mb-caption", "code · every edit")),
    if (nrow(pairs) == 0L) shiny::tags$span("Every in-scope pair is handled."),
    if (nrow(pairs) > 0L) shiny::tags$span(class = "mb-gap", sprintf("%d unhandled event/state pair%s", nrow(pairs), if (nrow(pairs) == 1L) "" else "s")),
    lapply(seq_len(nrow(pairs)), function(i) {
      pair <- pairs[i, ]
      value <- list(state = pair$state_id, event = pair$event_id)
      shiny::tags$div(
        shiny::tags$div(class = "mono", style = "font-size:12px", sprintf("%s × %s", pair$event_name, pair$state_name)),
        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("pair_transition"), value), "Add transition"), " ",
        shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", onclick = event_js(ns("pair_ignore"), value), "Ignore, with my note")
      )
    })
  )
}

MACHINE_SIDE_TABS <- c("Inspector" = "inspector", "Completeness" = "completeness", "Checks" = "checks")

# The left column picks and lists; the verification panels sit in tabs on the right, and
# the simulator's controls run as one strip under the canvas (UX review §2.20, §2.21).
mod_machines_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  side_panel <- function(value, ...) shiny::conditionalPanel(sprintf("output.side_tab == '%s'", value), ns = ns, class = "mb-side-panel", ...)
  shiny::tagList(
    screen_bar(tabs, actions = shiny::tagList(
      shiny::actionButton(ns("add_state"), "Add state", class = "mb-btn"),
      shiny::actionButton(ns("auto_layout"), "Auto-layout", class = "mb-btn"))),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "machines.list", "State machine", width = 260,
                  shiny::selectInput(ns("machine"), "State machine", choices = character(0), width = "100%", selectize = FALSE),
                  shiny::uiOutput(ns("states")),
                  shiny::uiOutput(ns("definitions"))),
      shiny::tags$main(
        class = "mb-main",
        shiny::tags$span(class = "mb-caption", "Drag between a state's handles to add a transition. Click a state or transition to edit it."),
        shiny::tags$div(class = "mb-diagram", mbdiagramOutput(ns("diagram"))),
        shiny::tags$section(
          class = "mb-sim", `aria-label` = "Simulator",
          shiny::uiOutput(ns("runbar")),
          shiny::uiOutput(ns("fire")),
          shiny::tags$details(class = "mb-trace-fold", shiny::tags$summary(shiny::textOutput(ns("trace_title"), inline = TRUE)), shiny::uiOutput(ns("trace")))
        )
      ),
      side_column("right", "machines.side", "Inspector", width = 360,
                  shiny::uiOutput(ns("side_tabs")),
                  side_panel("inspector", shiny::uiOutput(ns("inspector"))),
                  side_panel("completeness", shiny::uiOutput(ns("completeness")), shiny::uiOutput(ns("questions"))),
                  side_panel("checks", shiny::uiOutput(ns("checks"))))
    )
  )
}

# The script and expectation an assertion builder describes, as the JSON core stores.
# `sets` are the values each event sets, as the Fire buttons use them.
assertion_json <- function(machine, start, events_text, expect_state, within = "", sets = list()) {
  events <- trimws(strsplit(if (is.null(events_text)) "" else events_text, ",", fixed = TRUE)[[1]])
  events <- events[nzchar(events)]
  if (length(events) == 0L) stop("Name at least one event to fire.", call. = FALSE)
  unknown <- setdiff(events, machine$events$name)
  if (length(unknown) > 0L) stop(sprintf("%s %s an event of this state machine.", paste(unknown, collapse = ", "), if (length(unknown) == 1L) "isn't" else "aren't"), call. = FALSE)
  if (!start %in% machine$states$id || !expect_state %in% machine$states$id) stop("Choose a start state and an expected state.", call. = FALSE)
  script <- c(list(list(start = start)), lapply(events, function(name) if (is.null(sets[[name]])) list(event = name) else list(event = name, set = sets[[name]])))
  expect <- list(state = expect_state)
  within <- trimws(if (is.null(within)) "" else within)
  if (nzchar(within)) {
    seconds <- suppressWarnings(as.numeric(within))
    if (is.na(seconds) || seconds <= 0) stop("Within is a number of seconds.", call. = FALSE)
    expect$within_s <- seconds
  }
  list(script = as.character(jsonlite::toJSON(script, auto_unbox = TRUE)), expect = as.character(jsonlite::toJSON(expect, auto_unbox = TRUE)))
}

# Set values an assertion script gives an event, so firing it by hand matches the check.
event_default_sets <- function(con, sm_id) {
  rows <- DBI::dbGetQuery(con, "SELECT script_json FROM sm_assertion WHERE sm_id = ?", params = list(sm_id))
  out <- list()
  for (json in rows$script_json) {
    steps <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) list())
    for (step in steps) if (!is.null(step$event) && !is.null(step$set) && is.null(out[[step$event]])) out[[step$event]] <- step$set
  }
  out
}

mod_machines_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    selected <- shiny::reactiveVal(list(kind = "none", ids = character(0)))
    redraw <- shiny::reactiveVal(0L)
    sim <- shiny::reactiveVal(NULL)

    machines <- shiny::reactive({
      db_dep(state, "behavior")
      DBI::dbGetQuery(con(), "SELECT m.id, m.name, e.name AS owner FROM state_machine m JOIN element e ON e.id = m.owner_element_id ORDER BY m.id")
    })
    shiny::observe({
      rows <- machines()
      keep <- shiny::isolate(input$machine)
      shiny::updateSelectInput(session, "machine", choices = stats::setNames(rows$id, sprintf("%s · %s", rows$owner, rows$name)), selected = if (!is.null(keep) && keep %in% rows$id) keep)
    })
    sm_id <- shiny::reactive({
      value <- input$machine
      if (!is.null(value) && nzchar(value)) return(value)
      rows <- machines()
      if (nrow(rows) > 0L) rows$id[1] else NULL
    })
    machine <- shiny::reactive({
      db_dep(state, "behavior")
      db_dep(state, "model")
      shiny::req(sm_id())
      load_machine(con(), sm_id())
    })
    # A new machine, or an edit, restarts the run: the old trace no longer describes it.
    shiny::observeEvent(machine(), start_run())
    # Reset goes back to where this run started, which may not be the start state.
    start_run <- function(state_id = NULL) {
      running <- sim_start(machine(), state_id)
      running$started_in <- if (is.null(state_id)) machine()$initial_state_id else state_id
      sim(running)
    }
    shiny::observeEvent(input$reset, start_run(shiny::isolate(sim())$started_in))
    shiny::observeEvent(input$start_in, start_run(input$start_in$value))

    output$diagram <- renderMbdiagram({
      db_dep(state, "behavior")
      redraw()
      shiny::req(sm_id())
      data <- machine_diagram(con(), sm_id())
      data$current <- shiny::isolate(if (is.null(sim())) NA_character_ else sim()$current)
      mbdiagram(c(data, list(selected = list())))
    })
    shiny::observe({
      running <- sim()
      if (is.null(running)) return()
      last <- if (length(running$trace) > 0L) running$trace[[length(running$trace)]]$transition else NULL
      mbdiagram_send(session, "diagram", "set_current", list(id = running$current, via = if (length(last) == 1L && !is.na(last)) last))
    })

    output$runbar <- shiny::renderUI({
      running <- sim()
      shiny::req(running)
      m <- shiny::isolate(machine())
      shiny::tags$div(
        class = "mb-runbar",
        shiny::tags$span(sprintf("%.1f s", running$clock)),
        shiny::tags$b(state_name(m, running$current)),
        lapply(names(running$variables), function(name) {
          value <- running$variables[[name]]
          shiny::tags$label(name, " ", shiny::tags$input(type = "text", value = if (is.logical(value)) tolower(as.character(value)) else format(value, trim = TRUE),
                                                         `aria-label` = name, onchange = change_js(ns("set_variable"), list(name = name))))
        }),
        shiny::tags$label("Start in ", select_tag(stats::setNames(m$states$id, m$states$name), blank_of(running$started_in), change_js(ns("start_in"), list()))),
        shiny::actionButton(ns("reset"), "Reset", class = "mb-btn mb-btn-small"),
        if (running$status != "ok") shiny::tags$span(class = "mb-gap", running$error)
      )
    })
    shiny::observeEvent(input$set_variable, {
      running <- sim()
      name <- input$set_variable$name
      value <- trimws(input$set_variable$value)
      was <- running$variables[[name]]
      parsed <- if (is.logical(was)) {
        if (value %in% c("true", "false")) value == "true" else NA
      } else if (is.numeric(was)) {
        suppressWarnings(as.numeric(value))
      } else {
        value
      }
      if (length(parsed) != 1L || is.na(parsed)) return(refusal_toast(sprintf("%s can't be %s.", name, value)))
      running$variables[[name]] <- parsed
      sim(running)
    })
    output$fire <- shiny::renderUI({
      running <- sim()
      shiny::req(running)
      m <- shiny::isolate(machine())
      valid <- if (is.na(running$current)) character(0) else sim_valid_events(m, running)
      defaults <- event_default_sets(con(), m$id)
      button <- function(name) {
        sets <- defaults[[name]]
        shiny::tags$button(type = "button", class = if (name %in% valid) "valid" else "muted", onclick = event_js(ns("fire_event"), name),
                           title = if (length(sets) > 0L) paste("Sets", paste(sprintf("%s = %s", names(sets), vapply(sets, function(v) tolower(format(v)), "")), collapse = ", ")), name)
      }
      others <- setdiff(m$events$name, valid)
      # Said in words as well as by the dark fill: which events can fire from here.
      shiny::tags$div(class = "mb-fire-groups",
                      shiny::tags$div(class = "mb-fire", role = "group", `aria-label` = "Can fire now", shiny::tags$span(class = "mb-label", "Can fire now"),
                                      if (length(valid) == 0L) shiny::tags$span(class = "mb-caption", "nothing") else lapply(valid, button)),
                      if (length(others) > 0L) shiny::tags$div(class = "mb-fire", role = "group", `aria-label` = "Ignored here", shiny::tags$span(class = "mb-label", "Ignored here"), lapply(others, button)))
    })
    shiny::observeEvent(input$fire_event, {
      m <- machine()
      sets <- event_default_sets(con(), m$id)[[input$fire_event]]
      sim(sim_fire(m, sim(), input$fire_event, if (is.null(sets)) list() else sets))
    })
    output$trace <- shiny::renderUI({
      shiny::req(sim())
      render_trace(sim()$trace)
    })
    # Folded by default to leave the canvas its height; the count and last step stay in view.
    output$trace_title <- shiny::renderText({
      trace <- shiny::req(sim())$trace
      if (length(trace) == 0L) return("Trace · nothing yet")
      sprintf("Trace (%d) · last: %s", length(trace), trace[[length(trace)]]$note)
    })

    # --- Side tabs ---
    side_tab <- shiny::reactiveVal("inspector")
    output$side_tab <- shiny::reactive(side_tab())
    shiny::outputOptions(output, "side_tab", suspendWhenHidden = FALSE)
    output$side_tabs <- shiny::renderUI({
      pairs <- unhandled_pairs(con(), shiny::req(sm_id()))
      checks <- assertions()
      failing <- sum(checks$live_result != "pass")
      labels <- c(inspector = "Inspector", completeness = if (nrow(pairs) > 0L) sprintf("Completeness (%d)", nrow(pairs)) else "Completeness",
                  checks = if (failing > 0L) sprintf("Checks (%d failing)", failing) else "Checks")
      shiny::tags$div(role = "tablist", `aria-label` = "Panels", class = "mb-segmented mb-segmented-fill", lapply(names(labels), function(value) {
        on <- tolower(as.character(identical(side_tab(), value)))
        shiny::tags$button(type = "button", role = "tab", `aria-pressed` = on, `aria-selected` = on, onclick = event_js(ns("set_side_tab"), value), labels[[value]])
      }))
    })
    shiny::observeEvent(input$set_side_tab, side_tab(input$set_side_tab))
    output$states <- shiny::renderUI({
      m <- machine()
      on <- selected()$ids
      shiny::tagList(shiny::tags$div(class = "mb-label", "States"),
                     shiny::tags$div(class = "mb-tree", lapply(seq_len(nrow(m$states)), function(i) {
                       row <- m$states[i, ]
                       shiny::tags$button(type = "button", `aria-current` = if (row$id %in% on) "true", class = if (row$review_state == "unreviewed") "mb-sketch",
                                          onclick = event_js(ns("pick_state"), row$id),
                                          row$name, if (identical(row$id, m$initial_state_id)) shiny::tags$span(class = "mb-caption", " · start"),
                                          if (identical(row$id, sim()$current)) shiny::tags$span(class = "mb-caption", " · current"))
                     })))
    })
    shiny::observeEvent(input$pick_state, {
      selected(list(kind = "element", ids = input$pick_state))
      mbdiagram_send(session, "diagram", "set_selection", list(ids = list(input$pick_state)))
    })
    shiny::observeEvent(selected(), {
      if (length(selected()$ids) > 0L) side_tab("inspector")
      reveal_column(session, "inspector", length(selected()$ids) > 0L)
    }, ignoreInit = TRUE)

    # --- Panels ---
    output$completeness <- shiny::renderUI({
      db_dep(state, "behavior")
      shiny::req(sm_id())
      render_completeness(unhandled_pairs(con(), sm_id()), ns)
    })
    assertions <- shiny::reactive({
      db_dep(state, "behavior")
      db_dep(state, "model")
      runs()
      shiny::req(sm_id())
      live_assertions(con(), sm_id())
    })
    runs <- shiny::reactiveVal(0L)
    output$checks <- shiny::renderUI(render_checks(assertions(), ns))
    shiny::observeEvent(input$run_all, {
      for (id in assertions()$id) guarded_edit(run_assertion(con(), id))
      runs(runs() + 1L)
    })
    # A small builder: start here, fire these, expect that. JSON stays available (§2.22).
    shiny::observeEvent(input$add_assertion, {
      m <- machine()
      requirements <- DBI::dbGetQuery(con(), "SELECT id, coalesce(short_title, id) AS name FROM requirement WHERE status = 'committed' ORDER BY id")
      states <- stats::setNames(m$states$id, m$states$name)
      shiny::showModal(shiny::modalDialog(
        title = "Add an assertion", size = "l",
        shiny::textInput(ns("assert_description"), "What it checks", width = "100%"),
        shiny::selectInput(ns("assert_requirement"), "Requirement", c("None" = "", stats::setNames(requirements$id, sprintf("%s %s", requirements$id, requirements$name))), width = "100%", selectize = FALSE),
        shiny::tags$div(class = "mb-fields",
                        shiny::selectInput(ns("assert_start"), "Start in", states, selected = m$initial_state_id, selectize = FALSE),
                        shiny::textInput(ns("assert_events"), "Then fire, in order", placeholder = paste(utils::head(m$events$name, 3), collapse = ", ")),
                        shiny::selectInput(ns("assert_state"), "Expect it in", states, selectize = FALSE),
                        shiny::textInput(ns("assert_within"), "Within (s, optional)")),
        shiny::tags$span(class = "mb-caption", sprintf("Events: %s. Each sets the same values as its Fire button.", paste(m$events$name, collapse = ", "))),
        shiny::tags$details(class = "mb-fold",
                            shiny::tags$summary("Edit as JSON"),
                            shiny::checkboxInput(ns("assert_use_json"), "Use the JSON below instead of the builder"),
                            shiny::textAreaInput(ns("assert_script"), "Script (JSON)", rows = 3, width = "100%", value = '[{"start":""},{"event":""}]'),
                            shiny::textAreaInput(ns("assert_expect"), "Expected (JSON)", rows = 2, width = "100%", value = '{"state":""}')),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("assert_confirm"), "Add", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$assert_confirm, {
      requirement <- if (nzchar(input$assert_requirement)) input$assert_requirement else NA_character_
      json <- if (isTRUE(input$assert_use_json)) {
        list(script = input$assert_script, expect = input$assert_expect)
      } else {
        guarded_edit(assertion_json(machine(), input$assert_start, input$assert_events, input$assert_state, input$assert_within, event_default_sets(con(), sm_id())))
      }
      if (is.null(json)) return()
      written <- behavior_edit(state, add_assertion(con(), sm_id(), requirement, input$assert_description, json$script, json$expect), "add assertion")
      if (!is.null(written)) shiny::removeModal()
    })

    output$definitions <- shiny::renderUI({
      m <- machine()
      shiny::tags$section(
        class = "mb-inspector",
        shiny::tags$b("Events and variables"),
        shiny::tags$div(class = "mono", style = "font-size:12px", paste(m$events$name, collapse = ", ")),
        shiny::tags$div(class = "mono", style = "font-size:12px", paste(sprintf("%s: %s = %s", m$variables$name, m$variables$value_type, m$variables$initial_value), collapse = "; ")),
        shiny::tags$div(class = "mb-head",
                        shiny::actionButton(ns("add_event"), "Add event…", class = "mb-btn mb-btn-small"),
                        shiny::actionButton(ns("add_variable"), "Add variable…", class = "mb-btn mb-btn-small"))
      )
    })
    shiny::observeEvent(input$add_event, {
      shiny::showModal(shiny::modalDialog(
        title = "Add an event", shiny::textInput(ns("event_name"), "Name (snake_case)"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("event_confirm"), "Add", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$event_confirm, {
      name <- trimws(input$event_name)
      if (!grepl("^[a-z][a-z0-9_]*$", name)) return(refusal_toast("An event name is snake_case, like gnss_lost."))
      if (!is.null(behavior_edit(state, write_sm_event(con(), fields = list(sm_id = sm_id(), name = name), note = "Added event"), "add event"))) shiny::removeModal()
    })
    shiny::observeEvent(input$add_variable, {
      shiny::showModal(shiny::modalDialog(
        title = "Add a variable", shiny::textInput(ns("variable_name"), "Name"),
        shiny::selectInput(ns("variable_type"), "Type", c("number", "bool"), selectize = FALSE), shiny::textInput(ns("variable_value"), "Initial value"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("variable_confirm"), "Add", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$variable_confirm, {
      name <- trimws(input$variable_name)
      value <- trimws(input$variable_value)
      if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", name) || name %in% EXPR_KEYWORDS) return(refusal_toast("That can't be a variable name."))
      fits <- if (input$variable_type == "bool") value %in% c("true", "false") else !is.na(suppressWarnings(as.numeric(value)))
      if (!fits) return(refusal_toast(sprintf("The initial value has to be a %s.", input$variable_type)))
      written <- behavior_edit(state, write_sm_variable(con(), fields = list(sm_id = sm_id(), name = name, value_type = input$variable_type, initial_value = value), note = "Added variable"), "add variable")
      if (!is.null(written)) shiny::removeModal()
    })

    # --- Adding transitions and ignores ---
    pending <- shiny::reactiveVal(NULL)
    ask_transition <- function(from, to = NULL, event = NULL) {
      m <- machine()
      pending(list(from = from))
      shiny::showModal(shiny::modalDialog(
        title = sprintf("New transition from %s", state_name(m, from)),
        shiny::selectInput(ns("new_event"), "Event", stats::setNames(m$events$id, m$events$name), selected = event, selectize = FALSE),
        shiny::selectInput(ns("new_target"), "To", stats::setNames(m$states$id, m$states$name), selected = to, selectize = FALSE),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("transition_confirm"), "Add transition", class = "mb-btn mb-btn-primary"))
      ))
    }
    shiny::observeEvent(input$transition_confirm, {
      written <- behavior_edit(state, save_transition(con(), fields = list(sm_id = sm_id(), from_state_id = pending()$from, to_state_id = input$new_target, event_id = input$new_event), note = "Added transition"), "add transition")
      if (!is.null(written)) shiny::removeModal()
      redraw(redraw() + 1L)
    })
    shiny::observeEvent(input$pair_transition, ask_transition(input$pair_transition$state, event = input$pair_transition$event))
    shiny::observeEvent(input$pair_ignore, {
      pending(input$pair_ignore)
      m <- machine()
      shiny::showModal(shiny::modalDialog(
        title = sprintf("Ignore %s in %s", m$events$name[match(input$pair_ignore$event, m$events$id)], state_name(m, input$pair_ignore$state)),
        shiny::textAreaInput(ns("ignore_note"), "Why it's safe to ignore here", width = "100%"),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("ignore_confirm"), "Ignore, with my note", class = "mb-btn mb-btn-primary"))
      ))
    })
    shiny::observeEvent(input$ignore_confirm, {
      written <- behavior_edit(state, ignore_pair(con(), pending()$state, pending()$event, input$ignore_note), "ignore")
      if (!is.null(written)) shiny::removeModal()
    })

    # --- Diagram events ---
    adding <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$add_state, {
      adding(TRUE)
      mbdiagram_send(session, "diagram", "set_tool", list(tool = "component"))
    })
    shiny::observeEvent(input$auto_layout, mbdiagram_send(session, "diagram", "auto_layout"))
    shiny::observeEvent(input$diagram_event, {
      event <- input$diagram_event
      view_id <- machine_view_id(sm_id())
      switch(
        event$type,
        undo = run_command(state, undo_command, redraw),
        redo = run_command(state, redo_command, redraw),
        select = selected(list(kind = event$kind, ids = as.character(unlist(event$ids)))),
        move = record_diagram_layout(state, view_id, data.frame(id = event$id, x = event$x, y = event$y), "sm_state", TRUE),
        layout = {
          rows <- event_positions(event)
          if (!is.null(rows)) record_diagram_layout(state, view_id, rows, "sm_state", isTRUE(event$user))
        },
        connect = {
          redraw(redraw() + 1L)
          ask_transition(event$source_id, blank_of(event$target_id))
        },
        add_component = {
          adding(FALSE)
          mbdiagram_send(session, "diagram", "set_tool", list(tool = "select"))
          taken <- machine()$states$name
          name <- "New state"
          i <- 1L
          while (name %in% taken) {
            i <- i + 1L
            name <- sprintf("New state %d", i)
          }
          written <- behavior_edit(state, save_state(con(), fields = list(sm_id = sm_id(), name = name), note = "Added state"), "add state")
          if (!is.null(written)) save_layout(con(), view_id, layout_items(data.frame(id = written$id, x = event$x, y = event$y), item_kind = "sm_state"))
        }
      )
    })

    # --- Inspector ---
    output$inspector <- shiny::renderUI({
      db_dep(state, "behavior")
      item <- selected()
      if (length(item$ids) != 1L) return(shiny::tags$p(class = "mb-caption", "Select a state or transition to edit it."))
      m <- machine()
      if (item$kind == "connector") {
        row <- m$transitions[m$transitions$id == item$ids, , drop = FALSE]
        if (nrow(row) == 0L) return(NULL)
        field <- function(name) change_js(ns("transition_field"), list(id = row$id, field = name))
        return(shiny::tags$section(
          class = "mb-inspector",
          shiny::tags$div(class = "mb-head", shiny::tags$b(sprintf("%s → %s", state_name(m, row$from_state_id), state_name(m, row$to_state_id))), shiny::tags$span(class = "mono mb-caption", row$id)),
          if (row$review_state == "unreviewed") review_block(ns, "review_transition", "transition_note"),
          shiny::tags$label("Event", select_tag(stats::setNames(m$events$id, m$events$name), row$event_id, field("event_id"))),
          shiny::tags$label("Guard", shiny::tags$input(type = "text", class = "mono", value = blank_of(row$guard_expr), onchange = field("guard_expr"))),
          shiny::tags$label("Effect", shiny::tags$input(type = "text", class = "mono", value = blank_of(row$effect_expr), onchange = field("effect_expr"))),
          shiny::tags$label("Hold (s)", shiny::tags$input(type = "text", placeholder = "[value]", class = if (row$hold_state == "placeholder") "mb-is-placeholder", onchange = field("hold"),
                                                          value = switch(row$hold_state, set = format(row$hold_s, trim = TRUE), placeholder = "[value]", ""))),
          shiny::tags$label("Priority", shiny::tags$input(type = "text", value = row$priority, onchange = field("priority"))),
          danger_zone(confirm_button("Delete transition…", ns("delete_transition"), row$id, "Delete this transition?",
                                     sprintf("%s → %s on %s goes from the state machine. Undo brings it back.", state_name(m, row$from_state_id), state_name(m, row$to_state_id),
                                             m$events$name[match(row$event_id, m$events$id)]), confirm = "Delete transition"))
        ))
      }
      row <- m$states[m$states$id == item$ids, , drop = FALSE]
      if (nrow(row) == 0L) return(NULL)
      field <- function(name) change_js(ns("state_field"), list(id = row$id, field = name))
      shiny::tags$section(
        class = "mb-inspector",
        shiny::tags$div(class = "mb-head", shiny::tags$b(row$name), shiny::tags$span(class = "mono mb-caption", row$id)),
        if (row$review_state == "unreviewed") review_block(ns, "review_state_row", "state_note"),
        shiny::tags$label("Name", shiny::tags$input(type = "text", value = row$name, onchange = field("name"))),
        shiny::tags$label("Entry effect", shiny::tags$input(type = "text", class = "mono", value = blank_of(row$entry_effect), onchange = field("entry_effect"))),
        shiny::tags$label("Kind", select_tag(c("Normal" = "normal", "Final" = "final"), row$kind, field("kind"))),
        shiny::tags$div(class = "mb-label", "Must handle"),
        lapply(seq_len(nrow(m$events)), function(i) {
          event <- m$events[i, ]
          on <- any(m$scope$event_id == event$id & m$scope$state_id == row$id)
          shiny::tags$label(style = "display:block", shiny::tags$input(type = "checkbox", checked = if (on) NA, onchange = sprintf("Shiny.setInputValue('%s', {state: '%s', event: '%s', on: this.checked, n: Math.random()}, {priority: 'event'})", ns("scope"), row$id, event$id)), " ", event$name)
        }),
        if (!identical(row$id, m$initial_state_id)) shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("make_start"), row$id), "Make start state")),
        danger_zone(confirm_button(sprintf("Delete %s…", row$name), ns("delete_state"), row$id, sprintf("Delete %s?", row$name),
                                   sprintf("%s and the %d transition%s into or out of it go from the state machine. Undo brings them back.", row$name,
                                           sum(m$transitions$from_state_id == row$id | m$transitions$to_state_id == row$id),
                                           if (sum(m$transitions$from_state_id == row$id | m$transitions$to_state_id == row$id) == 1L) "" else "s"),
                                   confirm = "Delete state"))
      )
    })
    shiny::observeEvent(input$transition_field, {
      change <- input$transition_field
      value <- trimws(change$value)
      fields <- switch(change$field,
                       hold = if (!nzchar(value)) list(hold_state = "none", hold_s = NA_real_)
                              else if (value == "[value]") list(hold_state = "placeholder", hold_s = NA_real_)
                              else list(hold_state = "set", hold_s = suppressWarnings(as.numeric(value))),
                       priority = list(priority = suppressWarnings(as.integer(value))),
                       stats::setNames(list(if (nzchar(value)) value else NA_character_), change$field))
      if (change$field == "hold" && identical(fields$hold_state, "set") && is.na(fields$hold_s)) return(refusal_toast("A hold is a number of seconds, or [value]."))
      if (change$field == "priority" && is.na(fields$priority)) return(refusal_toast("Priority is a whole number."))
      written <- behavior_edit(state, save_transition(con(), change$id, fields, note = "Edited transition"), "edit transition")
      if (is.null(written)) redraw(redraw() + 1L)
    })
    shiny::observeEvent(input$state_field, {
      change <- input$state_field
      value <- trimws(change$value)
      fields <- stats::setNames(list(if (nzchar(value)) value else NA_character_), change$field)
      behavior_edit(state, save_state(con(), change$id, fields, note = "Edited state"), "edit state")
    })
    shiny::observeEvent(input$scope, {
      change <- input$scope
      key <- paste(change$event, change$state, sep = "|")
      if (isTRUE(change$on)) {
        behavior_edit(state, write_sm_event_scope(con(), key, note = "In scope"), "scope")
      } else {
        behavior_edit(state, delete_entity(con(), "sm_event_scope", key, note = "Out of scope"), "scope")
      }
    })
    shiny::observeEvent(input$make_start, behavior_edit(state, write_state_machine(con(), sm_id(), list(initial_state_id = input$make_start), note = "Changed start state"), "start state"))
    shiny::observeEvent(input$delete_transition, {
      behavior_edit(state, delete_entity(con(), "sm_transition", input$delete_transition), "delete transition")
      selected(list(kind = "none", ids = character(0)))
    })
    shiny::observeEvent(input$delete_state, {
      if (identical(input$delete_state, machine()$initial_state_id)) return(refusal_toast("Choose another start state first."))
      behavior_edit(state, delete_entity(con(), "sm_state", input$delete_state), "delete state")
      selected(list(kind = "none", ids = character(0)))
    })
    shiny::observeEvent(input$review_transition, behavior_edit(state, review_entity(con(), "sm_transition", selected()$ids, input$review_transition$note), "review"))
    shiny::observeEvent(input$review_state_row, behavior_edit(state, review_entity(con(), "sm_state", selected()$ids, input$review_state_row$note), "review"))

    # --- AI questions, after the code panel ---
    question_task <- shiny::ExtendedTask$new(function(connection, id) run_behavior_analysis_async(connection, id))
    question_error <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$ask, {
      question_error(NULL)
      question_task$invoke(con(), sm_id())
    })
    shiny::observeEvent(question_task$result(), {
      result <- question_task$result()
      if (!isTRUE(result$ok)) question_error(result$error)
      invalidate_db(state, "ai")
    })
    output$questions <- shiny::renderUI({
      db_dep(state, "ai")
      shiny::req(sm_id())
      open <- DBI::dbGetQuery(con(), "SELECT id, title FROM ai_proposal WHERE status = 'open' AND target_kind = 'state_machine' AND target_id = ? ORDER BY id", params = list(sm_id()))
      shiny::tags$section(
        class = "mb-sketch-panel",
        shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b("Behavior questions")),
        lapply(open$title, shiny::tags$div),
        if (nrow(open) > 0L) shiny::tags$span(class = "mb-caption", "Answer these in the Review queue."),
        shiny::actionButton(ns("ask"), "Ask about this state machine", class = "mb-btn mb-btn-ai"),
        if (question_task$status() == "running") shiny::tags$span(class = "mb-caption", "Checking…"),
        if (!is.null(question_error())) shiny::tags$span(question_error())
      )
    })

    invisible(list(sim = sim, machine = machine, assertions = assertions, question_task = question_task))
  })
}

# --- Behavioural impact ---

mod_behavior_impact_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  pane <- function(which, label) {
    shiny::tags$div(id = ns(paste0(which, "_pane")), class = "mb-compare-pane",
                    shiny::tags$div(class = "mb-label", label), shiny::uiOutput(ns(paste0(which, "_sequence"))),
                    shiny::conditionalPanel("output.is_activity", ns = ns, class = "mb-fill",
                                            shiny::tags$div(class = "mb-diagram", mbdiagramOutput(ns(paste0(which, "_activity"))))))
  }
  shiny::tagList(
    screen_bar(tabs,
               # The change name can be long, so the picker takes the room it needs (§2.23).
               shiny::tags$div(class = "mb-grow", shiny::selectInput(ns("card"), "Change", choices = character(0), width = "100%", selectize = FALSE)),
               shiny::uiOutput(ns("sides"), inline = TRUE)),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "impact.list", "Scenarios", width = 280, shiny::uiOutput(ns("list"))),
      shiny::tags$main(
        class = "mb-main",
        shiny::uiOutput(ns("summary")),
        shiny::tags$div(class = "mb-compare", pane("before", "Before"), pane("after", "After")),
        shiny::uiOutput(ns("pane_style"))
      )
    )
  )
}

mod_behavior_impact_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    side <- shiny::reactiveVal("after")
    picked <- shiny::reactiveVal(NULL)

    cards <- shiny::reactive({
      db_dep(state, "model")
      db_dep(state, "spec")
      db_dep(state, "baseline")
      latest <- latest_baseline(con())
      if (is.na(latest)) return(list())
      compare_baselines(con(), latest)
    })
    shiny::observe({
      all <- cards()
      ids <- vapply(all, function(card) card$change_set, "")
      focus <- state$behavior_focus()
      keep <- if (!is.null(focus) && focus %in% ids) focus else shiny::isolate(input$card)
      shiny::updateSelectInput(session, "card", choices = stats::setNames(ids, vapply(all, function(card) sprintf("%s: %s", card$title, card$change_text), "")),
                               selected = if (!is.null(keep) && keep %in% ids) keep)
    })
    card <- shiny::reactive({
      all <- cards()
      if (length(all) == 0L) return(NULL)
      hit <- Filter(function(one) identical(one$change_set, input$card), all)
      if (length(hit) == 1L) hit[[1]] else all[[1]]
    })
    items <- shiny::reactive({
      db_dep(state, "behavior")
      one <- card()
      if (is.null(one)) return(list())
      behavior_impact(con(), one)
    })
    scenario <- shiny::reactive({
      all <- items()
      if (length(all) == 0L) return(NULL)
      hit <- Filter(function(one) identical(one$id, picked()), all)
      if (length(hit) == 1L) hit[[1]] else all[[1]]
    })
    output$list <- shiny::renderUI({
      all <- items()
      on <- scenario()
      shiny::tagList(
        shiny::tags$div(class = "mb-label", sprintf("%d scenario%s affected", length(all), if (length(all) == 1L) "" else "s")),
        lapply(all, function(one) {
          touched <- length(one$changed_steps) + length(one$changed_nodes)
          shiny::tags$button(type = "button", class = "mb-card-select", `aria-current` = if (identical(one$id, on$id)) "true", onclick = event_js(ns("pick"), one$id),
                             shiny::tags$span(class = "mono mb-caption", sprintf("%s · %d hop%s", one$id, one$hops, if (one$hops == 1L) "" else "s")),
                             shiny::tags$b(one$name),
                             shiny::tags$span(class = if (touched > 0L) "mb-gap mb-caption" else "mb-caption", if (touched > 0L) sprintf("%d step%s the changed input", touched, if (touched == 1L) " uses" else "s use") else "Reached indirectly"))
        })
      )
    })
    shiny::observeEvent(input$pick, picked(input$pick))
    output$sides <- shiny::renderUI({
      button <- function(value, label) shiny::tags$button(type = "button", `aria-pressed` = tolower(as.character(identical(side(), value))), onclick = event_js(ns("set_side"), value), label)
      shiny::tags$div(role = "group", `aria-label` = "View", class = "mb-segmented", button("before", "Before"), button("after", "After"), button("both", "Side by side"))
    })
    shiny::observeEvent(input$set_side, side(input$set_side))
    output$pane_style <- shiny::renderUI({
      hidden <- switch(side(), before = "after_pane", after = "before_pane", NULL)
      if (is.null(hidden)) return(NULL)
      shiny::tags$style(sprintf("#%s { display: none; }", ns(hidden)))
    })
    output$is_activity <- shiny::reactive({
      one <- scenario()
      !is.null(one) && one$has_activity && !one$has_sequence
    })
    shiny::outputOptions(output, "is_activity", suspendWhenHidden = FALSE)

    output$summary <- shiny::renderUI({
      one <- scenario()
      if (is.null(one)) return(shiny::tags$p(class = "mb-caption", "No scenarios are reached by this change."))
      touched <- length(one$changed_steps) + length(one$changed_nodes)
      shiny::tags$section(
        class = "mb-inspector",
        shiny::tags$h1(style = "margin:0;font-size:18px;font-weight:600", one$name),
        shiny::tags$span(class = "mono", style = "font-size:12px", one$path),
        lapply(one$inputs, function(item) shiny::tags$span(class = "mb-gap", sprintf("%s: %s → %s", item$title, item$before, item$after))),
        if (touched == 0L) shiny::tags$span(class = "mb-caption", "This scenario is reached through the model, but no step here uses the changed input directly.")
      )
    })
    sequence_side <- function(which) {
      shiny::renderUI({
        one <- scenario()
        if (is.null(one) || !one$has_sequence) return(NULL)
        steps <- scenario_steps(con(), one$id)
        shiny::tagList(
          lapply(one$inputs, function(item) shiny::tags$div(class = "mono mb-caption", sprintf("%s = %s", item$title, item[[which]]))),
          seqview(scenario_lifelines(con(), one$id), steps, highlighted = one$changed_steps)
        )
      })
    }
    activity_side <- function(which) {
      renderMbdiagram({
        one <- scenario()
        shiny::req(one, one$has_activity)
        data <- activity_diagram(con(), one$id, impact_callouts(one$inputs, which))
        # Before and After zoom and scroll together, and open on what changed (§0.4).
        mbdiagram(c(data, list(selected = list(), tool = "select_only", sync = session$ns("impact"), focus = as.list(one$changed_nodes))))
      })
    }
    output$before_sequence <- sequence_side("before")
    output$after_sequence <- sequence_side("after")
    output$before_activity <- activity_side("before")
    output$after_activity <- activity_side("after")

    invisible(list(items = items, scenario = scenario, card = card))
  })
}
