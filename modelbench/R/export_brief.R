# The stakeholder brief: one page of plain words, as HTML and then PDF through Chrome.

html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  gsub(">", "&gt;", text, fixed = TRUE)
}

STATUS_LABELS <- c(not_started = "Not started", in_progress = "In progress", designed = "Designed", decision_needed = "Decision needed", open_question = "Open question")

brief_html <- function(con) {
  overview <- stakeholder_overview(con)
  parts <- vapply(seq_len(nrow(overview$parts)), function(i) {
    row <- overview$parts[i, ]
    sprintf("<div class='part'><b>%s</b> <span class='chip %s'>%s</span><br>%s</div>", html_escape(row$name), row$status, STATUS_LABELS[[row$status]],
            html_escape(if (is.na(row$plain_summary)) "No summary yet." else row$plain_summary))
  }, "")
  meters <- vapply(overview$meters, function(meter) {
    sprintf("<div class='meter'><div class='row'><span>%s</span><span class='mono'>%s</span></div><div class='track'><div class='fill %s' style='width:%d%%'></div></div><div class='line %s'>%s</div></div>",
            html_escape(meter$label), html_escape(meter$value_text), if (meter$pass) "" else "fail", round(meter$fraction * 100), if (meter$pass) "" else "fail", html_escape(meter$line))
  }, "")
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", html_escape(overview$project), " brief</title><style>",
    "body{font:14px/1.5 'IBM Plex Sans',system-ui,sans-serif;color:#1C1D1F;margin:32px}h1{font-size:22px;margin:0 0 4px}h2{font-size:16px;margin:20px 0 8px}",
    ".mono{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:13px}.grid{display:grid;grid-template-columns:1fr 1fr;gap:10px}",
    ".part{border:1px solid #D4D5D8;border-radius:4px;padding:10px;break-inside:avoid}.chip{font-size:11px;padding:1px 6px;border-radius:10px;background:#ECEDEF}",
    ".chip.designed{background:#1C1D1F;color:#fff}.chip.decision_needed,.chip.open_question{background:#FFF3E3;color:#8A4500;border:1px solid #E8B77A}.chip.not_started{background:#fff;border:1px dashed #9A9DA3}",
    ".meter{margin-bottom:12px}.row{display:flex;justify-content:space-between}.track{height:10px;background:#ECEDEF;border-radius:5px;overflow:hidden;-webkit-print-color-adjust:exact}",
    ".fill{height:10px;background:#45484D;-webkit-print-color-adjust:exact}.fill.fail{background:#B35A00}.line{font-size:12px;color:#5B5E63}.line.fail{color:#8A4500}",
    "</style></head><body><h1>", html_escape(overview$project), "</h1><p>", html_escape(overview$paragraph), "</p>",
    "<h2>Are we on track?</h2>", paste(meters, collapse = ""),
    "<h2>The main parts</h2><div class='grid'>", paste(parts, collapse = ""), "</div>",
    "<h2>Open questions</h2><ol>", paste(sprintf("<li>%s</li>", html_escape(overview$questions)), collapse = ""), "</ol>",
    "</body></html>"
  )
}

write_brief_pdf <- function(con, path) print_html_pdf(brief_html(con), path, "The PDF brief")
