# The diagram gets its data as one JSON string, so R's vector rules can't change its shape.
mbdiagram <- function(data, width = "100%", height = NULL, elementId = NULL) {
  htmlwidgets::createWidget(
    "mbdiagram", list(json = as.character(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", na = "null", dataframe = "rows", digits = NA))),
    width = width, height = height, package = "mbdiagram", elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(browser.fill = FALSE, knitr.figure = FALSE)
  )
}

mbdiagramOutput <- function(outputId, width = "100%", height = "560px") {
  htmlwidgets::shinyWidgetOutput(outputId, "mbdiagram", width, height, package = "mbdiagram")
}

renderMbdiagram <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, mbdiagramOutput, env, quoted = TRUE)
}

# Updates the live diagram without rebuilding it: set_tool, set_selection or auto_layout.
mbdiagram_send <- function(session, output_id, type, payload = list()) {
  session$sendCustomMessage(paste0("mbdiagram-", session$ns(output_id)), list(type = type, payload = payload))
}
