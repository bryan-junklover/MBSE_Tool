# Exports: requirements as CSV, Markdown, PDF and ReqIF, and the ICD through Quarto.
# The app is the system of record, so these only write files (BUILD_PLAN §6).

# One row per requirement, with its links flattened to text.
requirements_export <- function(con, include_drafts = TRUE) {
  rows <- DBI::dbGetQuery(con, sprintf("
    SELECT id, level, parent_id, short_title, text, req_type, status, rationale, verification_method
      FROM requirement WHERE status <> 'deprecated' %s ORDER BY CASE level WHEN 'stakeholder' THEN 1 WHEN 'system' THEN 2 ELSE 3 END, sort_order, id",
    if (include_drafts) "" else "AND status = 'committed'"))
  linked <- function(type, id) {
    hits <- DBI::dbGetQuery(con, "SELECT target_id FROM link WHERE link_type = ? AND source_kind = 'requirement' AND source_id = ? ORDER BY target_id", params = list(type, id))$target_id
    paste(hits, collapse = "; ")
  }
  rows$satisfied_by <- vapply(rows$id, function(id) linked("satisfied_by", id), "", USE.NAMES = FALSE)
  rows$verified_by <- vapply(rows$id, function(id) linked("verified_by", id), "", USE.NAMES = FALSE)
  rows$derives_from <- vapply(rows$id, function(id) linked("derives_from", id), "", USE.NAMES = FALSE)
  rows
}

export_csv <- function(con, path) {
  utils::write.csv(requirements_export(con), path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  invisible(path)
}

md_cell <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  gsub("\n", " ", gsub("|", "\\|", x, fixed = TRUE), fixed = TRUE)
}

md_table <- function(header, rows) {
  if (nrow(rows) == 0L) return("_None._")
  body <- apply(rows, 1L, function(row) paste0("| ", paste(md_cell(row), collapse = " | "), " |"))
  paste(c(paste0("| ", paste(header, collapse = " | "), " |"), paste0("|", paste(rep("---", length(header)), collapse = "|"), "|"), body), collapse = "\n")
}

LEVEL_TITLES <- c(stakeholder = "Stakeholder requirements", system = "System requirements", subsystem = "Subsystem requirements")

requirements_markdown <- function(con) {
  rows <- requirements_export(con)
  sections <- lapply(names(LEVEL_TITLES), function(level) {
    part <- rows[rows$level == level, , drop = FALSE]
    if (nrow(part) == 0L) return(NULL)
    items <- vapply(seq_len(nrow(part)), function(i) {
      row <- part[i, ]
      detail <- c(
        if (row$status != "committed") sprintf("Status: %s", row$status),
        if (!is.na(row$parent_id)) sprintf("Parent: %s", row$parent_id),
        if (!is.na(row$verification_method)) sprintf("Verified by %s", row$verification_method),
        if (nzchar(row$satisfied_by)) sprintf("Satisfied by: %s", row$satisfied_by),
        if (nzchar(row$verified_by)) sprintf("Verification: %s", row$verified_by),
        if (nzchar(row$derives_from)) sprintf("Derives from: %s", row$derives_from)
      )
      paste0("### ", row$id, if (!is.na(row$short_title)) paste0(" ", row$short_title), "\n\n", row$text, "\n",
             if (!is.na(row$rationale)) paste0("\n_Rationale:_ ", row$rationale, "\n"),
             if (length(detail) > 0L) paste0("\n", paste0("- ", detail, collapse = "\n"), "\n"))
    }, "")
    paste0("## ", LEVEL_TITLES[[level]], "\n\n", paste(items, collapse = "\n"))
  })
  paste0("# ", get_setting(con, "project_name", "Requirements"), ": requirements\n\n",
         sprintf("Exported %s · model %s\n\n", format(Sys.time(), "%Y-%m-%d", tz = "UTC"), substr(model_hash(con), 1L, 19L)),
         paste(unlist(sections), collapse = "\n"))
}

export_markdown <- function(con, path) {
  writeLines(requirements_markdown(con), path, useBytes = TRUE)
  invisible(path)
}

requirements_html <- function(con) {
  rows <- requirements_export(con)
  blocks <- vapply(names(LEVEL_TITLES), function(level) {
    part <- rows[rows$level == level, , drop = FALSE]
    if (nrow(part) == 0L) return("")
    items <- vapply(seq_len(nrow(part)), function(i) {
      row <- part[i, ]
      sprintf("<div class='req'><div class='mono'>%s%s</div><b>%s</b><p>%s</p>%s</div>", html_escape(row$id), if (row$status != "committed") sprintf(" · %s", row$status) else "",
              html_escape(if (is.na(row$short_title)) "" else row$short_title), html_escape(row$text),
              if (nzchar(row$satisfied_by)) sprintf("<div class='meta'>Satisfied by %s</div>", html_escape(row$satisfied_by)) else "")
    }, "")
    sprintf("<h2>%s</h2>%s", LEVEL_TITLES[[level]], paste(items, collapse = ""))
  }, "")
  paste0("<!doctype html><html><head><meta charset='utf-8'><title>Requirements</title><style>",
         "body{font:13px/1.5 'IBM Plex Sans',system-ui,sans-serif;color:#1C1D1F;margin:32px}h1{font-size:22px}h2{font-size:16px;margin-top:24px}",
         ".mono{font-family:'IBM Plex Mono',ui-monospace,monospace;font-size:11px;color:#5B5E63}.req{border-top:1px solid #D4D5D8;padding:8px 0;break-inside:avoid}",
         ".req p{margin:4px 0}.meta{font-size:12px;color:#5B5E63}</style></head><body><h1>", html_escape(get_setting(con, "project_name", "Requirements")),
         ": requirements</h1>", paste(blocks, collapse = ""), "</body></html>")
}

# Prints HTML with headless Chrome. Refuses in plain words if Chrome isn't there.
print_html_pdf <- function(html, path, what = "This PDF") {
  if (!requireNamespace("chromote", quietly = TRUE) || is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL))) {
    refuse(what, " needs Google Chrome installed.")
  }
  page <- tempfile(fileext = ".html")
  on.exit(unlink(page), add = TRUE)
  writeLines(html, page, useBytes = TRUE)
  browser <- chromote::ChromoteSession$new()
  on.exit(browser$close(), add = TRUE)
  loaded <- browser$Page$loadEventFired(wait_ = FALSE)
  browser$Page$navigate(paste0("file://", page), wait_ = FALSE)
  browser$wait_for(loaded)
  output <- browser$Page$printToPDF(printBackground = TRUE)
  writeBin(jsonlite::base64_dec(output$data), path)
  invisible(path)
}

export_requirements_pdf <- function(con, path) print_html_pdf(requirements_html(con), path, "The requirements PDF")

# --- ReqIF 1.0.1 ---

reqif_id <- function(kind, id) paste0("_", kind, "-", gsub("[^A-Za-z0-9_.-]", "_", id))

REQIF_ATTRIBUTES <- c(ID = "ReqIF.ForeignID", Title = "ReqIF.Name", Text = "ReqIF.Text", Level = "Level", Status = "Status",
                      Type = "Type", Rationale = "Rationale", Verification = "Verification method")
REQIF_LINKS <- c("satisfied_by", "allocated_to", "derives_from", "refines", "verified_by", "traces_to")

# Requirements, and what they link to, as SPEC-OBJECTS; every link between two exported
# objects as a SPEC-RELATION; the requirement tree as the SPECIFICATION.
export_reqif <- function(con, path) {
  stamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  requirements <- requirements_export(con)
  links <- DBI::dbGetQuery(con, sprintf("SELECT id, link_type, source_kind, source_id, target_kind, target_id FROM link WHERE link_type IN (%s) ORDER BY id",
                                        paste(sprintf("'%s'", REQIF_LINKS), collapse = ", ")))
  links <- links[links$source_kind == "requirement" & links$source_id %in% requirements$id, , drop = FALSE]
  others <- unique(links[links$target_kind != "requirement", c("target_kind", "target_id"), drop = FALSE])
  others$name <- vapply(seq_len(nrow(others)), function(i) {
    row <- read_entity(con, others$target_kind[i], others$target_id[i])
    if (is.null(row)) NA_character_ else if (!is.null(row$name)) row$name else if (!is.null(row$description)) row$description else others$target_id[i]
  }, "")
  others <- others[!is.na(others$name), , drop = FALSE]
  known <- c(reqif_id("requirement", requirements$id), reqif_id(others$target_kind, others$target_id))
  links <- links[reqif_id(links$target_kind, links$target_id) %in% known, , drop = FALSE]

  doc <- xml2::xml_new_root("REQ-IF", xmlns = "http://www.omg.org/spec/ReqIF/20110401/reqif.xsd", `xmlns:xhtml` = "http://www.w3.org/1999/xhtml")
  header <- xml2::xml_add_child(xml2::xml_add_child(doc, "THE-HEADER"), "REQ-IF-HEADER", IDENTIFIER = "_header")
  xml2::xml_add_child(header, "COMMENT", sprintf("Model %s", model_hash(con)))
  xml2::xml_add_child(header, "CREATION-TIME", stamp)
  xml2::xml_add_child(header, "REQ-IF-TOOL-ID", "ModelBench")
  xml2::xml_add_child(header, "REQ-IF-VERSION", "1.0")
  xml2::xml_add_child(header, "SOURCE-TOOL-ID", "ModelBench")
  xml2::xml_add_child(header, "TITLE", get_setting(con, "project_name", "Requirements"))
  content <- xml2::xml_add_child(xml2::xml_add_child(doc, "CORE-CONTENT"), "REQ-IF-CONTENT")
  child <- function(parent, name, ...) xml2::xml_add_child(parent, name, ...)
  ref <- function(parent, wrapper, tag, value) xml2::xml_add_child(child(parent, wrapper), tag, value)

  child(child(content, "DATATYPES"), "DATATYPE-DEFINITION-STRING", IDENTIFIER = "_dt-string", `LAST-CHANGE` = stamp, `LONG-NAME` = "Text", `MAX-LENGTH` = "100000")
  types <- child(content, "SPEC-TYPES")
  object_type <- function(id, name, attributes) {
    type <- child(types, "SPEC-OBJECT-TYPE", IDENTIFIER = id, `LAST-CHANGE` = stamp, `LONG-NAME` = name)
    defs <- child(type, "SPEC-ATTRIBUTES")
    for (key in attributes) {
      def <- child(defs, "ATTRIBUTE-DEFINITION-STRING", IDENTIFIER = paste0(id, "-", key), `LAST-CHANGE` = stamp, `LONG-NAME` = unname(REQIF_ATTRIBUTES[key]))
      ref(def, "TYPE", "DATATYPE-DEFINITION-STRING-REF", "_dt-string")
    }
  }
  object_type("_t-requirement", "Requirement", names(REQIF_ATTRIBUTES))
  object_type("_t-item", "Model item", c("ID", "Title", "Type"))
  for (type in REQIF_LINKS) child(types, "SPEC-RELATION-TYPE", IDENTIFIER = paste0("_rt-", type), `LAST-CHANGE` = stamp, `LONG-NAME` = type)
  child(types, "SPECIFICATION-TYPE", IDENTIFIER = "_t-spec", `LAST-CHANGE` = stamp, `LONG-NAME` = "Requirements specification")

  objects <- child(content, "SPEC-OBJECTS")
  spec_object <- function(id, name, type, values) {
    node <- child(objects, "SPEC-OBJECT", IDENTIFIER = id, `LAST-CHANGE` = stamp, `LONG-NAME` = name)
    holder <- child(node, "VALUES")
    for (key in names(values)) {
      if (is.na(values[[key]]) || !nzchar(values[[key]])) next
      value <- child(holder, "ATTRIBUTE-VALUE-STRING", `THE-VALUE` = values[[key]])
      ref(value, "DEFINITION", "ATTRIBUTE-DEFINITION-STRING-REF", paste0(type, "-", key))
    }
    ref(node, "TYPE", "SPEC-OBJECT-TYPE-REF", type)
  }
  for (i in seq_len(nrow(requirements))) {
    row <- requirements[i, ]
    spec_object(reqif_id("requirement", row$id), if (is.na(row$short_title)) row$id else row$short_title, "_t-requirement",
                list(ID = row$id, Title = row$short_title, Text = row$text, Level = row$level, Status = row$status, Type = row$req_type,
                     Rationale = row$rationale, Verification = row$verification_method))
  }
  for (i in seq_len(nrow(others))) {
    row <- others[i, ]
    spec_object(reqif_id(row$target_kind, row$target_id), row$name, "_t-item", list(ID = row$target_id, Title = row$name, Type = row$target_kind))
  }

  relations <- child(content, "SPEC-RELATIONS")
  for (i in seq_len(nrow(links))) {
    row <- links[i, ]
    relation <- child(relations, "SPEC-RELATION", IDENTIFIER = reqif_id("link", row$id), `LAST-CHANGE` = stamp, `LONG-NAME` = row$link_type)
    ref(relation, "SOURCE", "SPEC-OBJECT-REF", reqif_id("requirement", row$source_id))
    ref(relation, "TARGET", "SPEC-OBJECT-REF", reqif_id(row$target_kind, row$target_id))
    ref(relation, "TYPE", "SPEC-RELATION-TYPE-REF", paste0("_rt-", row$link_type))
  }

  spec <- child(child(content, "SPECIFICATIONS"), "SPECIFICATION", IDENTIFIER = "_spec", `LAST-CHANGE` = stamp, `LONG-NAME` = get_setting(con, "project_name", "Requirements"))
  ref(spec, "TYPE", "SPECIFICATION-TYPE-REF", "_t-spec")
  hierarchy <- function(parent_node, parent_id) {
    kids <- requirements$id[if (is.na(parent_id)) is.na(requirements$parent_id) | !requirements$parent_id %in% requirements$id else requirements$parent_id %in% parent_id]
    if (length(kids) == 0L) return(invisible(NULL))
    holder <- child(parent_node, "CHILDREN")
    for (id in kids) {
      entry <- child(holder, "SPEC-HIERARCHY", IDENTIFIER = reqif_id("h", id), `LAST-CHANGE` = stamp)
      ref(entry, "OBJECT", "SPEC-OBJECT-REF", reqif_id("requirement", id))
      hierarchy(entry, id)
    }
  }
  hierarchy(spec, NA_character_)
  child(content, "SPEC-RELATION-GROUPS")
  xml2::write_xml(doc, path, options = c("format", "as_xml"))
  invisible(path)
}

# --- ICD ---

find_quarto <- function() {
  candidates <- c(Sys.getenv("QUARTO_PATH"), Sys.which("quarto"),
                  "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
                  "/Applications/quarto/bin/quarto", "/usr/local/bin/quarto")
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found) == 0L) NA_character_ else unname(found[1])
}

# Every connector, with the elements and ports at each end.
connection_rows <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT c.id, sp.interface_id, se.id AS source_id, se.name AS source, sp.name AS source_port,
           te.id AS target_id, te.name AS target, tp.name AS target_port
      FROM connector c JOIN port sp ON sp.id = c.source_port_id JOIN port tp ON tp.id = c.target_port_id
      JOIN element se ON se.id = sp.element_id JOIN element te ON te.id = tp.element_id ORDER BY c.id")
}

interface_rate_text <- function(row) if (is.na(row$rate_value)) "" else sprintf("%s %s", format(row$rate_value, trim = TRUE), row$rate_unit)

# One interface as an ICD section, with the open items it raises. `connections` are the
# connectors to list for it.
icd_interface_section <- function(con, row, connections) {
  fields <- interface_fields(con, row$id)
  used <- interface_where_used(con, row$id)
  links <- connections[connections$interface_id == row$id, , drop = FALSE]
  rate <- interface_rate_text(row)
  ranges <- vapply(seq_len(nrow(fields)), function(j) format_field_range(fields$range_state[j], fields$min_value[j], fields$max_value[j]), "")
  open_items <- c(
    if (any(fields$range_state == "placeholder")) sprintf("%s: range to be decided for %s.", row$name, paste(fields$name[fields$range_state == "placeholder"], collapse = ", ")),
    if (nrow(links) == 0L) sprintf("%s has no connection yet.", row$name)
  )
  markdown <- paste0(
    sprintf("## %s · %s {#%s}\n\n", row$id, row$name, tolower(row$id)),
    if (!is.na(row$description)) paste0(row$description, "\n\n"),
    sprintf("Flow: %s%s%s.\n\n", row$flow_type, if (nzchar(rate)) paste0(" · rate ", rate) else "", if (!is.na(row$transport)) paste0(" · over ", row$transport) else ""),
    if (row$review_state == "unreviewed") "::: {.callout-warning}\nThis interface is an unreviewed AI draft.\n:::\n\n",
    "### Fields\n\n", md_table(c("Field", "Type", "Unit", "Range", "Notes"), data.frame(fields$name, fields$value_type, fields$unit, ranges, fields$notes)), "\n\n",
    "### Connections\n\n", md_table(c("Connector", "From", "To"), data.frame(links$id, sprintf("%s.%s", links$source, links$source_port), sprintf("%s.%s", links$target, links$target_port))), "\n\n",
    "### Used by\n\n",
    paste0("- ", c(
      if (nrow(used$ports) > 0L) sprintf("Ports: %s", paste(sprintf("%s.%s (%s)", used$ports$element, used$ports$name, used$ports$direction), collapse = ", ")),
      if (nrow(used$scenarios) > 0L) sprintf("Scenarios: %s", paste(sprintf("%s %s", used$scenarios$id, used$scenarios$name), collapse = ", ")),
      if (nrow(used$events) > 0L) sprintf("State machine events: %s", paste(used$events$name, collapse = ", ")),
      if (nrow(used$requirements) > 0L) sprintf("Requirements: %s", paste(used$requirements$id, collapse = ", "))
    ), collapse = "\n"), "\n"
  )
  list(markdown = markdown, open_items = open_items)
}

icd_markdown <- function(con) {
  interfaces <- DBI::dbGetQuery(con, "SELECT * FROM interface WHERE layer = 'physical' ORDER BY flow_type, name")
  summary <- data.frame(ID = interfaces$id, Name = interfaces$name, Flow = interfaces$flow_type,
                        Rate = vapply(seq_len(nrow(interfaces)), function(i) interface_rate_text(interfaces[i, ]), ""), Transport = interfaces$transport, stringsAsFactors = FALSE)
  connections <- connection_rows(con)
  sections <- lapply(seq_len(nrow(interfaces)), function(i) icd_interface_section(con, interfaces[i, ], connections))
  open_items <- unlist(lapply(sections, function(section) section$open_items))
  paste0(
    "# Scope\n\n", get_setting(con, "scope_text", "The physical interfaces of the system."), "\n\n",
    sprintf("This document is generated from model %s. It lists %d interfaces and %d connections.\n\n", substr(model_hash(con), 1L, 19L), nrow(interfaces), nrow(connections)),
    "# Interface summary\n\n", md_table(c("ID", "Name", "Flow", "Rate", "Transport"), summary), "\n\n",
    "# Interfaces\n\n", paste(vapply(sections, function(section) section$markdown, ""), collapse = "\n"), "\n",
    "# Open items\n\n", if (length(open_items) == 0L) "None.\n" else paste0("- ", open_items, collapse = "\n"), "\n"
  )
}

# Renders Markdown to a document with the Quarto template: "html" (self-contained) or
# "pdf" (through Typst). `what` names the document in refusals.
render_quarto_document <- function(markdown, title, path, format = c("html", "pdf"), what = "This document") {
  format <- match.arg(format)
  quarto <- find_quarto()
  if (is.na(quarto)) refuse(what, " needs Quarto. Install it, or set QUARTO_PATH.")
  dir <- tempfile("doc")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  template <- readLines(system.file("quarto", "document.qmd", package = "modelbench"), encoding = "UTF-8")
  fill <- function(text, key, value) gsub(paste0("{{", key, "}}"), value, text, fixed = TRUE)
  text <- fill(fill(fill(paste(template, collapse = "\n"), "title", gsub("\"", "'", title)),
                    "date", format(Sys.time(), "%Y-%m-%d", tz = "UTC")), "body", markdown)
  writeLines(text, file.path(dir, "document.qmd"), useBytes = TRUE)
  result <- processx::run(quarto, c("render", "document.qmd", "--to", if (format == "pdf") "typst" else "html"), wd = dir, error_on_status = FALSE)
  output <- file.path(dir, if (format == "pdf") "document.pdf" else "document.html")
  if (result$status != 0L || !file.exists(output)) refuse("Quarto couldn't render ", tolower(substr(what, 1L, 1L)), substring(what, 2L), ": ", utils::tail(strsplit(result$stderr, "\n")[[1]], 1L))
  file.copy(output, path, overwrite = TRUE)
  invisible(path)
}

render_icd <- function(con, path, format = c("html", "pdf")) {
  render_quarto_document(icd_markdown(con), paste0(get_setting(con, "project_name", "System"), ": interface control document"),
                         path, format, "The ICD")
}
