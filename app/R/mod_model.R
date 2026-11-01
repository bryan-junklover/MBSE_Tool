# Model › Structure: tree, block diagram and inspector. Diagram and tree edits go
# through the same core functions, so both reach the same rows.

ELEMENT_STATUSES <- c("Not started" = "not_started", "In progress" = "in_progress", "Designed" = "designed",
                      "Decision needed" = "decision_needed", "Open question" = "open_question")
ELEMENT_DOMAINS <- c("Hardware" = "hw", "Software" = "sw", "Hardware and software" = "hw_sw", "Other" = "other")
DIAGRAM_TOOLS <- c("Select" = "select", "Component" = "component", "Port" = "port", "Connect" = "connect")
TOOL_HINTS <- c(select = "Drag port to port to connect · double-click a component to open its sub-architecture",
                component = "Click the canvas to add a component",
                port = "Click near a component's edge to add a port there",
                connect = "Drag from one port to another")

mod_model_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "mb-body",
    shiny::tags$aside(class = "mb-left",
                      shiny::tags$div(class = "mb-label", "Model tree"),
                      shiny::uiOutput(ns("tree")),
                      shiny::selectInput(ns("view"), "Saved view", choices = c("All components" = "VW-all"))),
    shiny::tags$main(
      class = "mb-main",
      shiny::uiOutput(ns("crumbs")),
      shiny::tags$div(class = "mb-toolbar",
                      shiny::uiOutput(ns("tools")),
                      shiny::actionButton(ns("group"), "Group into subsystem", class = "mb-btn"),
                      shiny::actionButton(ns("undo"), "Undo", class = "mb-btn"),
                      shiny::actionButton(ns("redo"), "Redo", class = "mb-btn"),
                      shiny::actionButton(ns("auto_layout"), "Auto-layout", class = "mb-btn")),
      shiny::tags$div(class = "mb-caption", shiny::textOutput(ns("hint"), inline = TRUE)),
      shiny::tags$div(class = "mb-diagram", mbdiagramOutput(ns("diagram"), height = "560px")),
      shiny::uiOutput(ns("facts")),
      shiny::uiOutput(ns("connections"))
    ),
    shiny::tags$aside(class = "mb-right", shiny::uiOutput(ns("inspector")))
  )
}

render_tree <- function(tree, select_input, open_input, selected) {
  shiny::tags$div(class = "mb-tree", lapply(seq_len(nrow(tree)), function(i) {
    row <- tree[i, ]
    shiny::tags$button(
      type = "button", class = if (row$review_state == "unreviewed") "mb-sketch",
      style = sprintf("padding-left:%dpx", 6L + 14L * row$depth),
      `aria-current` = if (row$id %in% selected) "true",
      onclick = event_js(select_input, row$id), ondblclick = event_js(open_input, row$id),
      row$name, if (row$review_state == "unreviewed") " (unreviewed)"
    )
  }))
}

render_crumbs <- function(path, names, open_input) {
  shiny::tags$nav(`aria-label` = "Level", class = "mb-crumbs", lapply(seq_along(path), function(i) {
    if (i == length(path)) return(shiny::tags$b(names[i]))
    shiny::tagList(shiny::tags$button(type = "button", onclick = event_js(open_input, path[i]), names[i]), " › ")
  }))
}

render_facts <- function(view, budgets, analyses, findings) {
  gaps <- sum(view$nodes$gap)
  shiny::tags$div(
    class = "mb-facts",
    shiny::tags$span(sprintf("%d components · %d connectors", nrow(view$nodes), nrow(view$edges))),
    lapply(seq_len(nrow(budgets)), function(i) {
      shiny::tags$span(class = if (!budgets$pass[i]) "mb-gap",
                       sprintf("%s %s%s", budgets$label[i], budgets$text[i], if (!budgets$pass[i]) " · FAIL" else ""))
    }),
    lapply(seq_len(nrow(analyses)), function(i) {
      shiny::tags$span(class = if (!analyses$pass[i]) "mb-gap",
                       sprintf("%s %s%s", term_to_title(analyses$analysis[i]), analyses$text[i], if (!analyses$pass[i]) " · FAIL" else ""))
    }),
    if (gaps > 0L) shiny::tags$span(class = "mb-gap", sprintf("%d open gap%s on this level", gaps, if (gaps == 1L) "" else "s")),
    if (nrow(findings) > 0L) shiny::tags$span(class = "mb-gap", sprintf("%d gate finding%s", nrow(findings), if (nrow(findings) == 1L) "" else "s"))
  )
}

# The accessible parallel to the diagram (handoff §7).
render_connections <- function(view, element_names) {
  if (nrow(view$edges) == 0L) return(NULL)
  name_of <- function(id) ifelse(is.na(id), "outside this view", ifelse(id %in% names(element_names), element_names[id], id))
  shiny::tags$details(
    shiny::tags$summary(sprintf("Connections (%d)", nrow(view$edges))),
    shiny::tags$div(class = "mb-table", shiny::tags$table(
      class = "mb-simple",
      shiny::tags$thead(shiny::tags$tr(shiny::tags$th("From"), shiny::tags$th("To"), shiny::tags$th("Label"))),
      shiny::tags$tbody(lapply(seq_len(nrow(view$edges)), function(i) {
        edge <- view$edges[i, ]
        shiny::tags$tr(shiny::tags$td(name_of(edge$source_holder)), shiny::tags$td(name_of(edge$target_holder)),
                       shiny::tags$td(class = "mono", if (is.na(edge$label)) "" else edge$label))
      }))
    ))
  )
}

labelled <- function(label, control) shiny::tags$label(style = "display:flex;flex-direction:column;gap:2px", shiny::tags$span(class = "mb-caption", label), control)

select_tag <- function(choices, selected, onchange, blank = NULL) {
  options <- c(if (!is.null(blank)) stats::setNames("", blank), choices)
  shiny::tags$select(onchange = onchange, lapply(seq_along(options), function(i) {
    shiny::tags$option(value = unname(options[i]), selected = if (identical(unname(options[i]), selected)) NA, names(options)[i])
  }))
}

render_element_inspector <- function(row, properties, ports, requirements, stereotypes, addable, pinned, ns) {
  text_or_blank <- function(x) if (is.na(x)) "" else x
  field <- function(name) change_js(ns("element_change"), list(field = name))
  unreviewed <- row$review_state == "unreviewed"
  shiny::tags$div(
    class = "mb-inspector",
    shiny::tags$div(class = "mono mb-caption", row$id),
    labelled("Name", shiny::tags$input(type = "text", value = row$name, onchange = field("name"))),
    if (unreviewed) {
      shiny::tags$section(
        class = "mb-sketch-panel",
        shiny::tags$div(class = "mb-head", shiny::tags$span(class = "mb-badge-ai", "AI"), shiny::tags$b("Unreviewed sketch")),
        shiny::tags$span("This came from the AI. Say in your own words why it belongs."),
        shiny::tags$textarea(id = ns("review_note"), rows = 2, `aria-label` = "Why it belongs"),
        shiny::tags$button(type = "button", class = "mb-btn",
                           onclick = sprintf("Shiny.setInputValue('%s', {note: document.getElementById('%s').value, n: Math.random()}, {priority: 'event'})",
                                             ns("review"), ns("review_note")), "Review, with my note")
      )
    },
    labelled("Stereotype", select_tag(stereotypes, text_or_blank(row$stereotype_id), field("stereotype_id"), "None")),
    labelled("Domain", select_tag(ELEMENT_DOMAINS, text_or_blank(row$domain), field("domain"), "Not set")),
    labelled("Status", select_tag(ELEMENT_STATUSES, row$status, field("status"))),
    labelled("How many", shiny::tags$input(type = "text", value = row$multiplicity, onchange = field("multiplicity"))),
    labelled("Description", shiny::tags$textarea(rows = 2, onchange = field("description"), text_or_blank(row$description))),
    shiny::tags$div(class = "mb-label", "Properties"),
    if (nrow(properties) > 0L) {
      shiny::tags$div(class = "mb-prop-grid", unlist(lapply(seq_len(nrow(properties)), function(i) {
        prop <- properties[i, ]
        is_placeholder <- prop$value_state == "placeholder"
        value <- if (is_placeholder) "" else if (!is.na(prop$num_value)) format(prop$num_value, trim = TRUE, digits = 15) else text_or_blank(prop$text_value)
        list(shiny::tags$span(prop$label),
             shiny::tags$input(type = "text", value = value, placeholder = "[value]", class = if (is_placeholder) "mb-is-placeholder",
                               `aria-label` = prop$label, onchange = change_js(ns("property_change"), list(id = prop$id))),
             shiny::tags$span(class = "mb-unit", if (is.na(prop$unit)) "" else prop$unit))
      }), recursive = FALSE))
    } else {
      shiny::tags$span(class = "mb-caption", "None recorded.")
    },
    if (length(addable) > 0L) labelled("Add a property", select_tag(addable, "", change_js(ns("add_property"), list()), "Choose…")),
    shiny::tags$div(class = "mb-label", "Ports"),
    if (nrow(ports) > 0L) {
      shiny::tags$div(class = "mb-prop-grid", style = "grid-template-columns: minmax(0,1fr) auto", unlist(lapply(seq_len(nrow(ports)), function(i) {
        port <- ports[i, ]
        no_iface <- is.na(port$interface)
        list(shiny::tags$span(class = if (no_iface || !port$connected) "mb-gap",
                              sprintf("%s · %s · %s%s", port$name, port$direction, if (no_iface) "no interface" else port$interface,
                                      if (port$connected) "" else " · no connector")),
             shiny::tags$button(type = "button", class = "mb-btn mb-btn-small", `aria-label` = paste("Remove port", port$name),
                                onclick = event_js(ns("remove_port"), port$id), "Remove"))
      }), recursive = FALSE))
    } else {
      shiny::tags$span(class = "mb-caption", "No ports yet.")
    },
    shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn mb-btn-small",
                                       onclick = event_js(ns("add_port_here"), row$id), "Add port…")),
    shiny::tags$div(class = "mb-label", sprintf("Linked requirements (%d)", nrow(requirements))),
    lapply(seq_len(nrow(requirements)), function(i) {
      shiny::tags$div(shiny::tags$span(class = "mono", requirements$id[i]), " ", text_or_blank(requirements$short_title[i]))
    }),
    shiny::tags$label(shiny::tags$input(type = "checkbox", checked = if (pinned) NA,
                                        onchange = sprintf("Shiny.setInputValue('%s', {on: this.checked, n: Math.random()}, {priority: 'event'})", ns("pin"))),
                      " Keep this position on auto-layout"),
    shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("delete_element"), row$id), "Delete from the model"))
  )
}

render_connector_inspector <- function(row, ns) {
  shiny::tags$div(
    class = "mb-inspector",
    shiny::tags$div(class = "mono mb-caption", row$id),
    shiny::tags$div(sprintf("%s · %s → %s · %s", row$source_element, row$source_port, row$target_element, row$target_port)),
    shiny::tags$div(class = "mono", if (is.na(row$interface)) "no interface" else row$interface),
    labelled("Label", shiny::tags$input(type = "text", value = if (is.na(row$label)) "" else row$label,
                                        onchange = change_js(ns("connector_change"), list(id = row$id)))),
    shiny::tags$div(shiny::tags$button(type = "button", class = "mb-btn", onclick = event_js(ns("delete_connector"), row$id), "Delete connector"))
  )
}

mod_model_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    root <- shiny::reactiveVal(NULL)
    tool <- shiny::reactiveVal("select")
    redraw <- shiny::reactiveVal(0L)
    selection <- state$selected_entity
    pending_port <- shiny::reactiveVal(NULL)

    current_root <- shiny::reactive({
      db_dep(state, "model")
      chosen <- root()
      if (!is.null(chosen) && !is.null(read_entity(con(), "element", chosen))) chosen else default_structure_root(con())
    })
    view_id <- shiny::reactive(if (is.null(input$view)) "VW-all" else input$view)
    view_data <- shiny::reactive({
      db_dep(state, "model")
      redraw()
      structure_view(con(), view_id(), current_root())
    })

    shiny::observe({
      views <- DBI::dbGetQuery(con(), "SELECT id, name FROM view_def WHERE view_kind = 'structure' ORDER BY is_builtin DESC, name")
      shiny::updateSelectInput(session, "view", choices = stats::setNames(views$id, views$name), selected = shiny::isolate(view_id()))
    })

    output$diagram <- renderMbdiagram({
      view <- view_data()
      mbdiagram(list(root = view$root, nodes = view$nodes, ports = view$ports, boundary_ports = view$boundary_ports, edges = view$edges,
                     selected = as.list(shiny::isolate(selection()$ids)), tool = shiny::isolate(tool())))
    })
    shiny::observeEvent(selection(), mbdiagram_send(session, "diagram", "set_selection", list(ids = as.list(selection()$ids))))
    shiny::observeEvent(tool(), mbdiagram_send(session, "diagram", "set_tool", list(tool = tool())))

    tree <- shiny::reactive({
      db_dep(state, "model")
      model_tree(con())
    })
    output$tree <- shiny::renderUI(render_tree(tree(), ns("tree_select"), ns("open"), selection()$ids))
    output$crumbs <- shiny::renderUI({
      path <- view_data()$breadcrumb
      names <- tree()$name[match(path, tree()$id)]
      render_crumbs(path, names, ns("open"))
    })
    output$tools <- shiny::renderUI({
      shiny::tags$div(role = "group", `aria-label` = "Diagram tools", class = "mb-segmented", lapply(names(DIAGRAM_TOOLS), function(label) {
        value <- DIAGRAM_TOOLS[[label]]
        shiny::tags$button(type = "button", `aria-pressed` = tolower(as.character(tool() == value)), onclick = event_js(ns("set_tool"), value), label)
      }))
    })
    output$hint <- shiny::renderText(TOOL_HINTS[[tool()]])
    shiny::observeEvent(input$set_tool, tool(input$set_tool))
    output$facts <- shiny::renderUI({
      view <- view_data()
      render_facts(view, budget_status(con()), analysis_status(con()), element_gate_findings(con()))
    })
    output$connections <- shiny::renderUI({
      all <- tree()
      render_connections(view_data(), stats::setNames(all$name, all$id))
    })

    select_element <- function(id) selection(list(kind = "element", ids = id))
    shiny::observeEvent(input$tree_select, select_element(input$tree_select))
    shiny::observeEvent(input$open, {
      root(input$open)
      selection(list(kind = "none", ids = character(0)))
    })

    shiny::observe({
      stack <- state$commands()
      shiny::updateActionButton(session, "undo", disabled = length(stack$undo) == 0L)
      shiny::updateActionButton(session, "redo", disabled = length(stack$redo) == 0L)
    })
    run_stack <- function(action) {
      stack <- guarded_edit(action(con(), state$commands()))
      if (is.null(stack)) return(invisible(NULL))
      state$commands(stack)
      invalidate_db(state, c("model", "spec"))
      redraw(redraw() + 1L)
    }
    shiny::observeEvent(input$undo, run_stack(undo_command))
    shiny::observeEvent(input$redo, run_stack(redo_command))
    shiny::observeEvent(input$auto_layout, mbdiagram_send(session, "diagram", "auto_layout"))

    shiny::observeEvent(input$group, {
      chosen <- selection()
      if (chosen$kind != "element" || length(chosen$ids) == 0L) return(refusal_toast("Select the components to group first."))
      written <- model_edit(state, group_into_subsystem(con(), chosen$ids), "group")
      if (!is.null(written)) select_element(written$id)
    })

    # --- Diagram events ---
    record_positions <- function(positions, undoable) {
      view <- view_id()
      before <- layout_rows(con(), view, positions$id)
      save_layout(con(), view, layout_items(positions))
      if (undoable) {
        state$commands(push_command(state$commands(), layout_command(view, before, layout_rows(con(), view, positions$id), "layout")))
      }
    }
    shiny::observeEvent(input$diagram_event, {
      event <- input$diagram_event
      switch(
        event$type,
        select = selection(list(kind = event$kind, ids = as.character(unlist(event$ids)))),
        open = {
          root(event$id)
          selection(list(kind = "none", ids = character(0)))
        },
        move = record_positions(data.frame(id = event$id, x = event$x, y = event$y), TRUE),
        layout = {
          rows <- do.call(rbind, lapply(event$positions, function(p) data.frame(id = p$id, x = p$x, y = p$y)))
          if (!is.null(rows)) record_positions(rows, isTRUE(event$user))
        },
        connect = {
          written <- model_edit(state, connect_ports(con(), event$source, event$target), "connect")
          # A refused link is on screen until the diagram is redrawn from the model.
          if (is.null(written)) redraw(redraw() + 1L)
        },
        add_component = {
          written <- model_edit(state, write_element(con(), fields = list(kind = "component", name = "New component", parent_id = current_root())), "add component")
          if (!is.null(written)) {
            save_layout(con(), view_id(), layout_items(data.frame(id = written$id, x = event$x, y = event$y)))
            redraw(redraw() + 1L)
            select_element(written$id)
          }
        },
        add_port = ask_port(event$element, event$side)
      )
    })

    ask_port <- function(element, side = NULL) {
      pending_port(list(element = element, side = side))
      interfaces <- DBI::dbGetQuery(con(), "SELECT id, name FROM interface ORDER BY layer DESC, name")
      shiny::showModal(shiny::modalDialog(
        title = "Add a port",
        shiny::textInput(ns("port_name"), "Name"),
        shiny::selectInput(ns("port_direction"), "Direction", choices = c("in", "out", "inout")),
        shiny::selectInput(ns("port_interface"), "Interface", choices = with_blank(stats::setNames(interfaces$id, interfaces$name), "None yet")),
        footer = shiny::tagList(shiny::modalButton("Cancel"), shiny::actionButton(ns("port_confirm"), "Add port", class = "mb-btn mb-btn-primary"))
      ))
    }
    shiny::observeEvent(input$add_port_here, ask_port(input$add_port_here))
    shiny::observeEvent(input$port_confirm, {
      pending <- pending_port()
      shiny::req(pending)
      name <- trimws(input$port_name)
      if (!nzchar(name)) return(refusal_toast("A port needs a name."))
      shiny::removeModal()
      interface <- if (nzchar(input$port_interface)) input$port_interface else NA_character_
      side <- if (is.null(pending$side)) NA_character_ else pending$side
      model_edit(state, write_port(con(), fields = list(element_id = pending$element, name = name, direction = input$port_direction,
                                                        interface_id = interface, side = side)), "add port")
      pending_port(NULL)
    })

    # --- Inspector ---
    output$inspector <- shiny::renderUI({
      db_dep(state, "model")
      chosen <- selection()
      if (length(chosen$ids) == 0L) return(shiny::tags$p(class = "mb-caption", "Select a block or connector to see its details."))
      if (length(chosen$ids) > 1L) return(shiny::tags$p(class = "mb-caption", sprintf("%d components selected.", length(chosen$ids))))
      if (chosen$kind == "connector") {
        row <- connector_detail(con(), chosen$ids)
        if (nrow(row) == 0L) return(NULL)
        return(render_connector_inspector(row, ns))
      }
      if (chosen$kind != "element") return(NULL)
      row <- read_entity(con(), "element", chosen$ids)
      if (is.null(row)) return(NULL)
      stereotypes <- DBI::dbGetQuery(con(), "SELECT id, name FROM stereotype WHERE applies_to = 'component' ORDER BY name")
      properties <- element_properties(con(), chosen$ids)
      defs <- DBI::dbGetQuery(con(), "SELECT id, label FROM property_def ORDER BY label")
      held <- DBI::dbGetQuery(con(), "SELECT property_def_id FROM property_value WHERE owner_kind = 'element' AND owner_id = ?", params = list(chosen$ids))$property_def_id
      addable <- defs[!defs$id %in% held, , drop = FALSE]
      stored <- layout_rows(con(), view_id(), chosen$ids)
      pinned <- nrow(stored) == 1L && grepl('"pinned":true', stored$extra_json, fixed = TRUE)
      render_element_inspector(row, properties, element_ports(con(), chosen$ids), element_requirements(con(), chosen$ids),
                               stats::setNames(stereotypes$id, stereotypes$name), stats::setNames(addable$id, addable$label), pinned, ns)
    })

    selected_id <- function() selection()$ids[1]
    shiny::observeEvent(input$element_change, {
      change <- input$element_change
      value <- change$value
      if (change$field == "name" && !nzchar(trimws(value))) return(refusal_toast("A component needs a name."))
      if (change$field == "multiplicity") {
        value <- suppressWarnings(as.integer(value))
        if (is.na(value) || value < 1L) return(refusal_toast("How many has to be a whole number, 1 or more."))
      } else if (!nzchar(value)) {
        value <- NA_character_
      }
      model_edit(state, write_element(con(), selected_id(), stats::setNames(list(value), change$field)), "edit")
    })
    shiny::observeEvent(input$property_change, {
      model_edit(state, set_property_value(con(), input$property_change$id, input$property_change$value), "set value")
    })
    shiny::observeEvent(input$add_property, {
      if (nzchar(input$add_property$value)) model_edit(state, add_property_to_element(con(), selected_id(), input$add_property$value), "add property")
    })
    shiny::observeEvent(input$remove_port, model_edit(state, delete_entity(con(), "port", input$remove_port), "remove port"))
    shiny::observeEvent(input$delete_element, {
      written <- model_edit(state, delete_entity(con(), "element", input$delete_element), "delete")
      if (!is.null(written)) selection(list(kind = "none", ids = character(0)))
    })
    shiny::observeEvent(input$review, model_edit(state, review_entity(con(), "element", selected_id(), input$review$note), "review"))
    shiny::observeEvent(input$pin, {
      set_pinned(con(), view_id(), selected_id(), isTRUE(input$pin$on))
      redraw(redraw() + 1L)
    })
    shiny::observeEvent(input$connector_change, {
      label <- if (nzchar(input$connector_change$value)) input$connector_change$value else NA_character_
      model_edit(state, write_connector(con(), input$connector_change$id, list(label = label)), "edit")
    })
    shiny::observeEvent(input$delete_connector, {
      written <- model_edit(state, delete_entity(con(), "connector", input$delete_connector), "delete connector")
      if (!is.null(written)) selection(list(kind = "none", ids = character(0)))
    })

    invisible(list(view_data = view_data, root = current_root, selection = selection, tool = tool))
  })
}
