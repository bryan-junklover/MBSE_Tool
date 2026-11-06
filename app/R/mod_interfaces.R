# Model › Interfaces: the interface dictionary, with fields and where each one is used.

FIELD_TYPES <- c("float32", "float64", "int32", "uint32", "uint64", "bool", "enum", "string", "bytes")
FLOW_TYPES <- c("Data" = "data", "Energy" = "energy", "Matter" = "matter")

mod_interfaces_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs, actions = shiny::tagList(
      shiny::tags$span(class = "mb-caption", "Interface control document"),
      export_button(ns("icd_html"), "ICD (HTML)"), export_button(ns("icd_pdf"), "ICD (PDF)"),
      shiny::actionButton(ns("add"), "+ Interface", class = "mb-btn mb-btn-primary mb-btn-compact"))),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", "interfaces.list", "Interfaces", width = 260,
                  shiny::textInput(ns("filter"), "Filter", placeholder = "Name"),
                  shiny::uiOutput(ns("list"))),
      shiny::tags$main(class = "mb-main", shiny::uiOutput(ns("form")), shiny::uiOutput(ns("fields"))),
      side_column("right", "interfaces.used", "Where used", width = 320, shiny::uiOutput(ns("used")))
    )
  )
}

render_interface_list <- function(rows, select_input, selected) {
  lapply(names(FLOW_TYPES), function(label) {
    group <- rows[rows$flow_type == FLOW_TYPES[[label]], , drop = FALSE]
    if (nrow(group) == 0L) return(NULL)
    shiny::tags$div(
      class = "mb-list-group",
      shiny::tags$div(class = "mb-label", label),
      lapply(seq_len(nrow(group)), function(i) {
        row <- group[i, ]
        shiny::tags$button(
          type = "button", class = "mb-list-item", `aria-current` = if (identical(row$id, selected)) "true",
          onclick = event_js(select_input, row$id),
          shiny::tags$span(class = "mono", row$name),
          shiny::tags$span(class = if (row$placeholders > 0L) "mb-gap",
                           sprintf("%d port%s%s", row$ports, if (row$ports == 1L) "" else "s",
                                   if (row$placeholders > 0L) sprintf(" · %d [value]", row$placeholders) else ""))
        )
      })
    )
  })
}

render_where_used <- function(used) {
  line <- function(label, values) if (length(values) > 0L) shiny::tags$div(class = "mb-caption", paste0(label, ": ", paste(values, collapse = ", ")))
  shiny::tags$section(
    class = "mb-inspector",
    shiny::tags$b("Where it's used"),
    if (nrow(used$ports) == 0L) shiny::tags$span(class = "mb-caption", "No ports use it yet."),
    lapply(seq_len(nrow(used$ports)), function(i) {
      shiny::tags$div(shiny::tags$span(class = "mono", used$ports$direction[i]), " · ", used$ports$element[i])
    }),
    line("Scenarios", used$scenarios$name),
    line("State machine events", used$events$name),
    line("Requirements", used$requirements$id),
    line("Functional flows", used$functional_flows$label)
  )
}

mod_interfaces_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    output$icd_html <- export_handler(state$con, "ICD", "html", function(con, file) render_icd(con, file, "html"))
    output$icd_pdf <- export_handler(state$con, "ICD", "pdf", function(con, file) render_icd(con, file, "pdf"))
    ns <- session$ns
    con <- state$con
    selected <- state$selected_interface

    rows <- shiny::reactive({
      db_dep(state, "model")
      all <- interface_list(con())
      needle <- trimws(if (is.null(input$filter)) "" else input$filter)
      if (nzchar(needle)) all <- all[grepl(tolower(needle), tolower(all$name), fixed = TRUE), , drop = FALSE]
      all
    })
    output$list <- shiny::renderUI(render_interface_list(rows(), ns("select"), selected()))
    shiny::observeEvent(input$select, selected(input$select))

    # The first interface opens by default, so the page is never empty.
    current <- shiny::reactive({
      db_dep(state, "model")
      id <- selected()
      row <- if (is.null(id)) NULL else read_entity(con(), "interface", id)
      if (!is.null(row)) return(row)
      all <- interface_list(con())
      if (nrow(all) == 0L) NULL else read_entity(con(), "interface", all$id[1])
    })

    output$form <- shiny::renderUI({
      row <- current()
      if (is.null(row)) return(shiny::tags$p(class = "mb-caption", "No interfaces yet."))
      change <- function(field) change_js(ns("interface_change"), list(id = row$id, field = field))
      text <- function(x) if (is.na(x)) "" else as.character(x)
      rate <- if (is.na(row$rate_value)) "" else paste(format(row$rate_value, trim = TRUE), row$rate_unit)
      shiny::tags$section(
        class = "mb-panel mb-inspector",
        shiny::tags$div(class = "mb-fields",
                        labelled("Name", shiny::tags$input(type = "text", class = "mono", value = row$name, onchange = change("name"))),
                        labelled("Flow type", select_tag(FLOW_TYPES, row$flow_type, change("flow_type"))),
                        labelled("Rate", shiny::tags$input(type = "text", value = rate, placeholder = "not set", onchange = change("rate"))),
                        labelled("Transport", shiny::tags$input(type = "text", value = text(row$transport), placeholder = "not set", onchange = change("transport")))),
        labelled("Description", shiny::tags$input(type = "text", value = text(row$description), onchange = change("description")))
      )
    })

    output$fields <- shiny::renderUI({
      row <- current()
      shiny::req(row)
      fields <- interface_fields(con(), row$id)
      units <- DBI::dbGetQuery(con(), "SELECT symbol FROM unit ORDER BY symbol")$symbol
      shiny::tags$section(
        class = "mb-inspector",
        shiny::tags$div(class = "mb-head", shiny::tags$h2(style = "margin:0;font-size:16px;font-weight:600", "Fields"),
                        shiny::tags$span(class = "mb-caption", "Units are checked against the unit registry on every edit"),
                        shiny::tags$button(type = "button", class = "mb-btn mb-push", onclick = event_js(ns("add_field"), row$id), "+ Field")),
        if (nrow(fields) == 0L) shiny::tags$span(class = "mb-caption", "No fields yet."),
        if (nrow(fields) > 0L) shiny::tags$div(class = "mb-table", shiny::tags$table(
          class = "mb-simple",
          shiny::tags$thead(shiny::tags$tr(lapply(c("Field", "Type", "Unit", "Range", "Notes", ""), shiny::tags$th))),
          shiny::tags$tbody(lapply(seq_len(nrow(fields)), function(i) {
            field <- fields[i, ]
            cell <- function(name) change_js(ns("field_change"), list(id = field$id, field = name))
            is_placeholder <- field$range_state == "placeholder"
            shiny::tags$tr(
              class = if (is_placeholder) "mb-gap-row",
              shiny::tags$td(shiny::tags$input(type = "text", class = "mb-table-input mono", value = field$name, `aria-label` = "Field name", onchange = cell("name"))),
              shiny::tags$td(select_tag(FIELD_TYPES, field$value_type, cell("value_type")) |> htmltools::tagAppendAttributes(class = "mb-table-input", `aria-label` = "Type")),
              shiny::tags$td(select_tag(stats::setNames(units, units), if (is.na(field$unit)) "" else field$unit, cell("unit"), "–") |>
                               htmltools::tagAppendAttributes(class = "mb-table-input", `aria-label` = "Unit")),
              shiny::tags$td(shiny::tags$input(type = "text", class = paste("mb-table-input mono", if (is_placeholder) "mb-is-placeholder"),
                                               value = format_field_range(field$range_state, field$min_value, field$max_value),
                                               placeholder = "[min] … [max]", `aria-label` = "Range", onchange = cell("range"))),
              shiny::tags$td(shiny::tags$input(type = "text", class = "mb-table-input", value = if (is.na(field$notes)) "" else field$notes,
                                               `aria-label` = "Notes", onchange = cell("notes"))),
              shiny::tags$td(confirm_button("Remove", ns("delete_field"), field$id, sprintf("Remove field %s?", field$name),
                                            sprintf("%s loses its field %s. Messages that name it will show as missing.", row$name, field$name),
                                            confirm = "Remove field", class = "mb-btn mb-btn-small") |>
                               htmltools::tagAppendAttributes(`aria-label` = paste("Remove field", field$name)))
            )
          }))
        ))
      )
    })
    output$used <- shiny::renderUI({
      row <- current()
      shiny::req(row)
      used <- interface_where_used(con(), row$id)
      shiny::tagList(
        render_where_used(used),
        danger_zone(confirm_button(sprintf("Delete %s…", row$name), ns("delete"), row$id, sprintf("Delete %s?", row$name),
                                   delete_effects_text(delete_effects(con(), "interface", row$id)),
                                   confirm = "Delete interface"))
      )
    })

    shiny::observeEvent(input$add, {
      written <- model_edit(state, write_interface(con(), fields = list(name = next_interface_name(con()), flow_type = "data")), "add interface")
      if (!is.null(written)) selected(written$id)
    })
    shiny::observeEvent(input$delete, {
      if (!is.null(model_edit(state, delete_entity(con(), "interface", input$delete), "delete interface"))) selected(NULL)
    })
    shiny::observeEvent(input$interface_change, {
      change <- input$interface_change
      fields <- if (change$field == "rate") {
        guarded_edit(parse_interface_rate(con(), change$value))
      } else {
        stats::setNames(list(if (nzchar(change$value)) change$value else NA_character_), change$field)
      }
      if (!is.null(fields)) model_edit(state, write_interface(con(), change$id, fields), "edit interface")
    })
    shiny::observeEvent(input$add_field, {
      model_edit(state, write_interface_field(con(), fields = list(interface_id = input$add_field, name = next_field_name(con(), input$add_field),
                                                                   value_type = "float32", range_state = "placeholder",
                                                                   sort_order = nrow(interface_fields(con(), input$add_field)) + 1L)), "add field")
    })
    shiny::observeEvent(input$delete_field, model_edit(state, delete_entity(con(), "interface_field", input$delete_field), "remove field"))
    shiny::observeEvent(input$field_change, {
      change <- input$field_change
      fields <- if (change$field == "range") {
        guarded_edit(parse_field_range(change$value))
      } else {
        stats::setNames(list(if (nzchar(change$value)) change$value else NA_character_), change$field)
      }
      if (!is.null(fields)) model_edit(state, write_interface_field(con(), change$id, fields), "edit field")
    })

    invisible(list(current = current, rows = rows))
  })
}
