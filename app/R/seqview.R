# The sequence diagram as plain SVG (handoff §5.7). Rows go to the table beside it when
# clicked; the drawing holds no state of its own.

SEQ_COLUMN <- 170
SEQ_ROW <- 46
SEQ_TOP <- 62

svg_tag <- function(name, ...) htmltools::tag(name, list(...))

seq_click <- function(input_id, step_id, label) {
  if (is.null(input_id)) return(list())
  js <- event_js(input_id, step_id)
  list(role = "button", tabindex = "0", `aria-label` = label, onclick = js,
       onkeydown = sprintf("if (event.key === 'Enter') { %s }", js))
}

seq_colours <- function(step, highlighted) {
  if (identical(step$review_state, "unreviewed")) return(list(line = "var(--ai)", text = "var(--ai-text)", dash = "6 6"))
  if (isTRUE(step$field_missing) || step$id %in% highlighted) return(list(line = "var(--gap-stroke)", text = "var(--gap-text)", dash = if (step$id %in% highlighted) "6 4" else NA))
  list(line = "var(--ink-2)", text = "var(--ink)", dash = NA)
}

seq_arrow <- function(x1, x2, y, colours, width) {
  head <- if (x2 >= x1) -1 else 1
  svg_tag("g",
          svg_tag("line", x1 = x1, y1 = y, x2 = x2, y2 = y, stroke = colours$line, `stroke-width` = width, `stroke-dasharray` = if (!is.na(colours$dash)) colours$dash),
          svg_tag("path", d = sprintf("M %g %g L %g %g L %g %g z", x2, y, x2 + 8 * head, y - 4, x2 + 8 * head, y + 4), fill = colours$line))
}

# Frames from fragment_open to fragment_close, innermost drawn last.
seq_frames <- function(steps) {
  open <- list()
  frames <- list()
  for (i in seq_len(nrow(steps))) {
    kind <- steps$step_kind[i]
    if (kind == "fragment_open") open[[length(open) + 1L]] <- i
    if (kind == "fragment_close" && length(open) > 0L) {
      start <- open[[length(open)]]
      open[[length(open)]] <- NULL
      frames[[length(frames) + 1L]] <- list(start = start, end = i, depth = length(open))
    }
  }
  # A frame nobody closed runs to the end, so it's still visible and can be fixed.
  for (start in open) frames[[length(frames) + 1L]] <- list(start = start, end = nrow(steps) + 1L, depth = 0L)
  frames
}

seqview <- function(lifelines, steps, selected = NULL, input_id = NULL, highlighted = character(0)) {
  n <- nrow(lifelines)
  if (n == 0L) return(shiny::tags$p(class = "mb-caption", "No lifelines yet. Add one to start."))
  x_of <- function(id) (match(id, lifelines$id) - 1L) * SEQ_COLUMN + SEQ_COLUMN / 2
  y_of <- function(i) SEQ_TOP + i * SEQ_ROW
  width <- n * SEQ_COLUMN
  bottom <- y_of(nrow(steps) + 1L)

  heads <- lapply(seq_len(n), function(i) {
    x <- (i - 1L) * SEQ_COLUMN + SEQ_COLUMN / 2
    svg_tag("g",
            svg_tag("rect", x = x - 72, y = 8, width = 144, height = 34, rx = 4, fill = "var(--surface)", stroke = "var(--line-strong)"),
            svg_tag("text", x = x, y = 30, `text-anchor` = "middle", `font-size` = 12, `font-weight` = 600, fill = "var(--ink)", lifelines$name[i]),
            svg_tag("line", x1 = x, y1 = 42, x2 = x, y2 = bottom, stroke = "var(--line-strong)", `stroke-dasharray` = "4 4"))
  })
  frames <- lapply(seq_frames(steps), function(frame) {
    top <- y_of(frame$start) - 24
    inset <- 6 + frame$depth * 8
    open <- steps[frame$start, ]
    tag_text <- sprintf("%s [%s]", open$fragment_kind, if (is.na(open$guard)) open$label else open$guard)
    htmltools::tag("g", c(seq_click(input_id, open$id, tag_text),
      list(svg_tag("rect", x = inset, y = top, width = width - 2 * inset, height = y_of(frame$end) - top - 8, fill = "none", stroke = "var(--ink-2)"),
           svg_tag("text", x = inset + 6, y = top + 15, `font-size` = 11, `font-family` = "var(--font-mono)", fill = "var(--ink)", tag_text))))
  })
  rows <- lapply(seq_len(nrow(steps)), function(i) {
    step <- steps[i, ]
    y <- y_of(i)
    on <- identical(step$id, selected)
    weight <- if (on) 3 else 1.5
    colours <- seq_colours(step, highlighted)
    label <- step_label(step)
    body <- switch(
      step$step_kind,
      message = {
        x1 <- x_of(step$from_lifeline_id)
        x2 <- x_of(step$to_lifeline_id)
        list(seq_arrow(x1, x2, y, colours, weight),
             svg_tag("text", x = (x1 + x2) / 2, y = y - 7, `text-anchor` = "middle", `font-size` = 12, `font-weight` = if (on) 600 else 400, fill = colours$text, label))
      },
      self = {
        x <- x_of(step$from_lifeline_id)
        list(svg_tag("path", d = sprintf("M %g %g h 34 v 16 h -28", x, y - 12), fill = "none", stroke = colours$line, `stroke-width` = weight),
             svg_tag("path", d = sprintf("M %g %g l 8 -4 v 8 z", x, y + 4), fill = colours$line),
             svg_tag("text", x = x + 40, y = y, `font-size` = 12, fill = colours$text, label))
      },
      note = {
        x <- x_of(step$from_lifeline_id)
        box <- max(120, nchar(label) * 6.2 + 14)
        list(svg_tag("rect", x = x - box / 2, y = y - 18, width = box, height = 26, fill = "var(--surface-muted)", stroke = if (on) "var(--ink)" else "var(--line-strong)"),
             svg_tag("text", x = x, y = y, `text-anchor` = "middle", `font-size` = 12, fill = "var(--ink)", label))
      },
      fragment_else = list(svg_tag("line", x1 = 6, y1 = y - 8, x2 = width - 6, y2 = y - 8, stroke = "var(--ink-2)", `stroke-dasharray` = "4 4"),
                           svg_tag("text", x = 14, y = y + 6, `font-size` = 11, `font-family` = "var(--font-mono)", fill = "var(--ink)", sprintf("[%s]", if (is.na(step$guard)) step$label else step$guard))),
      NULL
    )
    if (is.null(body)) return(NULL)
    htmltools::tag("g", c(seq_click(input_id, step$id, label), body))
  })
  shiny::tags$div(
    class = "mb-seqview",
    htmltools::tag("svg", list(viewBox = sprintf("0 0 %g %g", width, bottom + 12), width = width, height = bottom + 12, role = "img",
                               `aria-label` = sprintf("Sequence diagram with %d steps", nrow(steps)), style = "display:block;font-family:var(--font-sans)", heads, frames, rows))
  )
}
