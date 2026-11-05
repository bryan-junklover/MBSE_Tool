validate_reqif <- function(path) {
  schema <- test_path("reqif-schema", "reqif.xsd")
  skip_if(!nzchar(Sys.which("xmllint")), "xmllint isn't installed")
  out <- suppressWarnings(system2("xmllint", c("--nonet", "--noout", "--schema", shQuote(schema), shQuote(path)), stdout = TRUE, stderr = TRUE))
  list(ok = is.null(attr(out, "status")), output = out)
}

test_that("the ReqIF export validates against the OMG ReqIF 1.0.1 schema", {
  con <- local_built_sample()
  path <- withr::local_tempfile(fileext = ".reqif")
  export_reqif(con, path)
  result <- validate_reqif(path)
  expect_true(result$ok, info = paste(result$output, collapse = "\n"))
  doc <- xml2::read_xml(path)
  ns <- c(r = "http://www.omg.org/spec/ReqIF/20110401/reqif.xsd")
  objects <- xml2::xml_find_all(doc, "//r:SPEC-OBJECT", ns)
  committed <- count_of(con, "SELECT count(*) AS n FROM requirement WHERE status <> 'deprecated'")
  expect_gte(length(objects), committed)
  # Links are SPEC-RELATIONS, typed by link type, between exported objects.
  relations <- xml2::xml_find_all(doc, "//r:SPEC-RELATION", ns)
  expect_gt(length(relations), 0L)
  targets <- xml2::xml_text(xml2::xml_find_all(doc, "//r:SPEC-RELATION/r:TARGET/r:SPEC-OBJECT-REF", ns))
  expect_true(all(targets %in% xml2::xml_attr(objects, "IDENTIFIER")))
  expect_true("_rt-satisfied_by" %in% xml2::xml_text(xml2::xml_find_all(doc, "//r:SPEC-RELATION-TYPE-REF", ns)))
  text <- xml2::xml_attr(xml2::xml_find_all(doc, "//r:ATTRIBUTE-VALUE-STRING", ns), "THE-VALUE")
  expect_true(any(grepl("return to its launch point", text, fixed = TRUE)))
})

test_that("a broken ReqIF file fails the same check, so the check means something", {
  path <- withr::local_tempfile(fileext = ".reqif")
  writeLines('<?xml version="1.0"?><REQ-IF xmlns="http://www.omg.org/spec/ReqIF/20110401/reqif.xsd"><THE-HEADER/></REQ-IF>', path)
  expect_false(validate_reqif(path)$ok)
})

test_that("the ICD renders for the sample through the Quarto template", {
  skip_if(is.na(find_quarto()), "Quarto isn't installed")
  con <- local_built_sample()
  path <- withr::local_tempfile(fileext = ".html")
  render_icd(con, path)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "interface control document", fixed = TRUE)
  for (name in DBI::dbGetQuery(con, "SELECT name FROM interface WHERE layer = 'physical'")$name) expect_match(html, name, fixed = TRUE)
  expect_match(html, "Open items", fixed = TRUE)
  pdf <- withr::local_tempfile(fileext = ".pdf")
  render_icd(con, pdf, "pdf")
  expect_identical(readBin(pdf, "raw", 4L), charToRaw("%PDF"))
})

test_that("the ICD body lists fields, connections and open items", {
  con <- local_built_sample()
  text <- icd_markdown(con)
  expect_match(text, "# Interface summary", fixed = TRUE)
  expect_match(text, "## IF-002 · NavSolution", fixed = TRUE)
  expect_match(text, "| gnss_valid |", fixed = TRUE)
  expect_match(text, "range to be decided", fixed = TRUE)
  expect_identical(md_cell("a|b\nc"), "a\\|b c")
})

test_that("CSV and Markdown carry every requirement with its links", {
  con <- local_built_sample()
  path <- withr::local_tempfile(fileext = ".csv")
  export_csv(con, path)
  rows <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_identical(nrow(rows), count_of(con, "SELECT count(*) AS n FROM requirement WHERE status <> 'deprecated'"))
  expect_true(all(c("satisfied_by", "verified_by", "derives_from") %in% names(rows)))
  expect_match(rows$verified_by[rows$id == "SYS-012"], "A-001", fixed = TRUE)
  text <- requirements_markdown(con)
  expect_match(text, "## Stakeholder requirements", fixed = TRUE)
  expect_match(text, "### SYS-012", fixed = TRUE)
  expect_false(grepl("deprecated", text))
})

test_that("the requirements PDF is printed when Chrome is there, and refused in words when not", {
  con <- local_built_sample()
  expect_match(requirements_html(con), "System requirements", fixed = TRUE)
  path <- withr::local_tempfile(fileext = ".pdf")
  chrome <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
  if (is.null(chrome)) {
    expect_error(export_requirements_pdf(con, path), "needs Google Chrome")
  } else {
    export_requirements_pdf(con, path)
    expect_identical(readBin(path, "raw", 4L), charToRaw("%PDF"))
  }
})
