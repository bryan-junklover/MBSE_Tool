# Analyze › Documents: read-only documents for each stakeholder viewpoint
# (docs/VIEWPOINTS_AND_FAILURES.md §2). Preview here, export as HTML or PDF.

mod_documents_ui <- function(id, tabs = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    screen_bar(tabs,
               shiny::selectInput(ns("source"), "From", choices = c("Working copy" = "working"), width = "auto", selectize = FALSE),
               actions = shiny::tagList(export_button(ns("export_html"), "Export HTML"), export_button(ns("export_pdf"), "Export PDF"))),
    shiny::tags$div(class = "mb-body",
                    side_column("left", "documents.list", "Documents", width = 280, shiny::uiOutput(ns("list"))),
                    shiny::tags$main(class = "mb-main", shiny::uiOutput(ns("preview"))))
  )
}

render_viewpoint_list <- function(rows, select_input, selected) {
  shiny::tagList(
    shiny::tags$div(class = "mb-label", "Documents"),
    lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, ]
      shiny::tags$button(type = "button", class = "mb-card-select", `aria-current` = if (identical(row$id, selected)) "true",
                         onclick = event_js(select_input, row$id),
                         shiny::tags$b(row$name),
                         shiny::tags$span(class = "mb-caption", sprintf("For %s", tolower(row$stakeholder))),
                         shiny::tags$span(style = "font-size:13px", row$concerns))
    }),
    shiny::tags$p(class = "mb-caption", style = "margin-top:12px",
                  "Each document is read-only and generated from the model. Systems covers everything, for reporting.")
  )
}

# Quarto's heading anchors and callouts, as plain Markdown for the preview.
preview_markdown <- function(markdown) {
  markdown <- gsub("(?m)^(#+ .*?) \\{#[^}]*\\}$", "\\1", markdown, perl = TRUE)
  gsub("::: \\{\\.callout-[a-z]+\\}\n(.*?)\n:::", "> \\1", markdown, perl = TRUE)
}

# The document as the reader will get it, rendered from the same Markdown as the export.
render_document_preview <- function(markdown, title) {
  shiny::tags$article(class = "mb-doc", `aria-label` = paste(title, "preview"),
                      shiny::tags$h1(class = "mb-doc-title", title),
                      shiny::HTML(commonmark::markdown_html(preview_markdown(markdown), extensions = TRUE)))
}

mod_documents_server <- function(id, state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- state$con
    selected <- shiny::reactiveVal("VP-hardware")

    viewpoints <- shiny::reactive(viewpoint_list(con()))
    shiny::observe({
      db_dep(state, "baseline")
      rows <- baseline_list(con())
      labels <- ifelse(rows$is_safety == 1L, sprintf("%s · safety", rows$id), sprintf("%s · %s", rows$id, rows$name))
      shiny::updateSelectInput(session, "source", choices = c("Working copy" = "working", stats::setNames(rows$id, labels)),
                               selected = shiny::isolate(if (is.null(input$source)) "working" else input$source))
    })
    shiny::observeEvent(input$select, selected(input$select))
    source_id <- function() if (is.null(input$source) || !nzchar(input$source)) "working" else input$source

    output$list <- shiny::renderUI(render_viewpoint_list(viewpoints(), ns("select"), selected()))

    document <- shiny::reactive({
      for (area in c("model", "spec", "behavior", "baseline")) db_dep(state, area)
      tryCatch(list(markdown = viewpoint_document(con(), selected(), source_id()), title = viewpoint_title(con(), selected())),
               error = function(e) list(error = conditionMessage(e)))
    })
    output$preview <- shiny::renderUI({
      doc <- document()
      if (!is.null(doc$error)) return(shiny::tags$p(class = "mb-gap", doc$error))
      render_document_preview(doc$markdown, doc$title)
    })

    writer <- function(format) function(connection, file) render_viewpoint(connection, selected(), file, format, source_id())
    name <- function() gsub("[^A-Za-z0-9]+", "_", paste(read_viewpoint(con(), selected())$name, if (source_id() != "working") source_id()))
    output$export_html <- shiny::downloadHandler(
      filename = function() sprintf("%s_%s.html", gsub("[^A-Za-z0-9]+", "_", get_setting(con(), "project_name", "project")), name()),
      content = export_content(con, writer("html")))
    output$export_pdf <- shiny::downloadHandler(
      filename = function() sprintf("%s_%s.pdf", gsub("[^A-Za-z0-9]+", "_", get_setting(con(), "project_name", "project")), name()),
      content = export_content(con, writer("pdf")))
    invisible(list(selected = selected, document = document))
  })
}
