section_of <- function(markdown, title) {
  lines <- strsplit(markdown, "\n")[[1]]
  start <- match(paste("#", title), lines)
  if (is.na(start)) return(NA_character_)
  ends <- which(startsWith(lines, "# ") & seq_along(lines) > start)
  paste(lines[start:(if (length(ends) == 0L) length(lines) else ends[1] - 1L)], collapse = "\n")
}

test_that("three built-in viewpoints are seeded, with sections that exist", {
  con <- local_project()
  rows <- viewpoint_list(con)
  expect_identical(rows$id, c("VP-hardware", "VP-software", "VP-systems"))
  for (id in rows$id) expect_true(all(read_viewpoint(con, id)$sections %in% VIEWPOINT_SECTIONS))
  expect_setequal(read_viewpoint(con, "VP-systems")$sections, VIEWPOINT_SECTIONS)
  expect_error(read_viewpoint(con, "VP-nope"), "no viewpoint")
  DBI::dbExecute(con, "INSERT INTO viewpoint (id, name, stakeholder, concerns, rules_json) VALUES
                       ('VP-0001', 'Safety', 'Safety', 'Hazards', '{\"domains\":[\"hw\"],\"flow_types\":[\"data\"],\"sections\":[\"hazards\"]}')")
  expect_error(read_viewpoint(con, "VP-0001"), "section that doesn't exist: hazards")
})

test_that("the hardware document shows parts, folds software into its host, and skips software sections", {
  con <- local_built_sample()
  md <- viewpoint_document(con, "VP-hardware")
  breakdown <- section_of(md, "Physical breakdown")
  expect_match(breakdown, "**FC Hardware** (C-008)", fixed = TRUE)
  expect_match(breakdown, "- runs Autopilot Software (C-009)", fixed = TRUE)
  expect_no_match(breakdown, "**Autopilot Software**", fixed = TRUE)
  expect_no_match(breakdown, "Ground Control Software", fixed = TRUE)
  expect_match(breakdown, "**Propulsion** (C-006) × 4", fixed = TRUE)
  expect_match(section_of(md, "Interfaces"), "IF-001 · PowerBus28V", fixed = TRUE)
  expect_match(section_of(md, "Budgets and properties"), "| Mass | Survey UAV | 3.31 / 3.60 kg | Within limit |", fixed = TRUE)
  expect_match(section_of(md, "Budgets and properties"), "| Power System | Battery capacity | 4200 mAh |", fixed = TRUE)
  expect_true(is.na(section_of(md, "Software")))
  expect_true(is.na(section_of(md, "States and transitions")))
  open <- section_of(md, "Open items")
  expect_match(open, "Power System: Return reserve is a placeholder.", fixed = TRUE)
  expect_no_match(open, "Ground Control Software")
  expect_no_match(open, "isn't handled")
})

test_that("the software document shows hosts, signals with ranges and state tables, and no power", {
  con <- local_built_sample()
  md <- viewpoint_document(con, "VP-software")
  software <- section_of(md, "Software")
  expect_match(software, "| Autopilot Software (C-009) | FC Hardware | Flight Controller | Control flight |", fixed = TRUE)
  expect_match(software, "| Ground Control Software (C-002) | — |", fixed = TRUE)
  signals <- section_of(md, "Signals")
  expect_match(signals, "## IF-002 · NavSolution", fixed = TRUE)
  expect_match(signals, "From Navigation Sensors to Flight Controller. Rate 50 Hz.", fixed = TRUE)
  expect_match(signals, "| heading_accuracy | float32 | deg | [to be decided] |", fixed = TRUE)
  expect_no_match(md, "PowerBus28V")
  expect_true(is.na(section_of(md, "Physical breakdown")))
  states <- section_of(md, "States and transitions")
  expect_match(states, "| Survey mission | gnss_lost | gnss_valid == false | 3 s | Return to home |", fixed = TRUE)
  expect_match(section_of(md, "Requirements"), "| SYS-012 |.*| A-001 not run |")
  open <- section_of(md, "Open items")
  expect_match(open, "Ground Control Software: nothing is recorded for it to run on.", fixed = TRUE)
  expect_match(open, "Flight modes: link_lost isn't handled in Land.", fixed = TRUE)
  expect_match(open, "Flight modes: hold_timeout time is a placeholder.", fixed = TRUE)
  expect_match(section_of(md, "About this document"), "3 components, 5 interfaces, 1 state machine and 2 requirements", fixed = TRUE)
})

test_that("the systems document has every section", {
  con <- local_built_sample()
  md <- viewpoint_document(con, "VP-systems")
  for (title in c("About this document", "Physical breakdown", "Interfaces", "Budgets and properties", "Software", "Signals",
                  "States and transitions", "Requirements", "Open items")) {
    expect_false(is.na(section_of(md, title)), info = title)
  }
  expect_match(section_of(md, "Physical breakdown"), "**Autopilot Software** (C-009)", fixed = TRUE)
})

test_that("a document from a baseline shows the model then and leaves the working copy alone", {
  con <- local_built_sample()
  before <- model_hash(con)
  baselines <- nrow(baseline_list(con))
  md <- viewpoint_document(con, "VP-hardware", "BL-03")
  expect_match(section_of(md, "About this document"), "**Source:** baseline BL-03, Baseline 3", fixed = TRUE)
  expect_match(md, "| Power System | Battery capacity | 5000 mAh |", fixed = TRUE)
  expect_identical(model_hash(con), before)
  expect_identical(nrow(baseline_list(con)), baselines)
  expect_match(viewpoint_document(con, "VP-hardware", "working"), "4200 mAh", fixed = TRUE)
  expect_error(viewpoint_document(con, "VP-hardware", "BL-99"), "no baseline BL-99")
})

test_that("viewpoint documents and the ICD render through Quarto", {
  skip_if(is.na(find_quarto()), "Quarto isn't installed")
  con <- local_built_sample()
  html <- withr::local_tempfile(fileext = ".html")
  render_viewpoint(con, "VP-software", html)
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Survey UAV: Software view", fixed = TRUE)
  expect_match(text, "NavSolution", fixed = TRUE)
  icd <- withr::local_tempfile(fileext = ".html")
  render_icd(con, icd)
  expect_match(paste(readLines(icd, warn = FALSE), collapse = "\n"), "interface control document", fixed = TRUE)
})
