# Help › User guide and How-to: the Markdown in docs/, rendered in the app. The files in
# docs/ are the only copy, so the guide read on GitHub and in the app can't drift.

# docs/ sits beside app/, and the app runs from app/. The modelbench.docs_dir option points
# somewhere else, as the tests do.
help_path <- function(file) {
  candidates <- c(file.path(getOption("modelbench.docs_dir", file.path("..", "docs")), file), file.path("docs", file))
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0L) NA_character_ else normalizePath(found[1])
}

help_slug <- function(text) {
  slug <- gsub("[^a-z0-9]+", "-", tolower(text))
  gsub("^-+|-+$", "", slug)
}

# The page as HTML, with an id on each section heading, and its contents for the side column.
render_help_markdown <- function(markdown, prefix) {
  html <- commonmark::markdown_html(markdown, extensions = TRUE)
  headings <- regmatches(html, gregexpr("<h2>(.*?)</h2>", html))[[1]]
  titles <- gsub("^<h2>|</h2>$", "", headings)
  ids <- paste(prefix, vapply(gsub("<[^>]+>", "", titles), help_slug, "", USE.NAMES = FALSE), sep = "-")
  for (i in seq_along(headings)) html <- sub(headings[i], sprintf("<h2 id=\"%s\">%s</h2>", ids[i], titles[i]), html, fixed = TRUE)
  contents <- data.frame(id = ids, title = gsub("<[^>]+>", "", titles))
  # A page's own contents list is for reading it outside the app; the side column does that job.
  list(html = html, contents = contents[contents$title != "Contents", , drop = FALSE])
}

render_help_contents <- function(contents) {
  shiny::tags$nav(
    `aria-label` = "On this page", class = "mb-help-contents",
    shiny::tags$div(class = "mb-label", "On this page"),
    lapply(seq_len(nrow(contents)), function(i) {
      shiny::tags$a(href = paste0("#", contents$id[i]), class = "mb-list-item",
                    onclick = sprintf("event.preventDefault(); document.getElementById('%s').scrollIntoView({block: 'start'});", contents$id[i]),
                    contents$title[i])
    })
  )
}

mod_help_ui <- function(id, file, tabs = NULL) {
  ns <- shiny::NS(id)
  path <- help_path(file)
  page <- if (is.na(path)) NULL else render_help_markdown(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), ns("s"))
  shiny::tagList(
    screen_bar(tabs),
    shiny::tags$div(
      class = "mb-body",
      side_column("left", paste0("help.", id), "Contents", width = 240, if (!is.null(page)) render_help_contents(page$contents)),
      shiny::tags$main(class = "mb-main",
                       if (is.null(page)) {
                         shiny::tags$p(class = "mb-gap", sprintf("The help file %s isn't where the app expects it, in docs/ beside app/.", file))
                       } else {
                         shiny::tags$article(class = "mb-doc mb-help", shiny::HTML(page$html))
                       })
    )
  )
}
