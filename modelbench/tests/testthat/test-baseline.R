battery_card <- function(con) {
  cards <- compare_baselines(con, "BL-03")
  cards[[which(vapply(cards, function(card) any(vapply(card$entities, function(e) e$id == "V-0013", TRUE)), TRUE))]]
}

test_that("BL-03 against the working copy is exactly 3 change sets touching 4 entities", {
  con <- local_built_sample()
  cards <- compare_baselines(con, "BL-03")
  expect_length(cards, 3L)
  ids <- lapply(cards, function(card) vapply(card$entities, function(e) e$id, ""))
  expect_setequal(unlist(ids), c("V-0013", "V-0003", "SYS-002", "FLD-0005"))
  expect_identical(sort(lengths(ids)), c(1L, 1L, 2L))
  battery <- battery_card(con)
  expect_identical(battery$title, "Power System · Battery capacity")
  expect_identical(battery$change_text, "5000 → 4200 mAh")
  added <- cards[[which(vapply(cards, function(card) card$kind == "interface_field", TRUE))]]
  expect_identical(added$status, "added")
  expect_identical(added$title, "NavSolution · heading_accuracy")
  expect_identical(added$change_text, "float32, deg")
  requirement <- cards[[which(vapply(cards, function(card) card$kind == "requirement", TRUE))]]
  expect_identical(requirement$entities[[1]]$diff[[1]]$field, "text")
  expect_identical(requirement$change_text, "5 km → 8 km")
  expect_match(requirement$entities[[1]]$diff[[1]]$before, "5 km", fixed = TRUE)
  expect_match(requirement$entities[[1]]$diff[[1]]$after, "8 km", fixed = TRUE)
})

test_that("a draft requirement never shows in Compare", {
  con <- local_built_sample()
  save_draft(con, "The UAV shall float.", fields = list(level = "system"))
  expect_length(compare_baselines(con, "BL-03"), 3L)
})

test_that("creating BL-04 is refused with 8 blockers, and writes nothing", {
  con <- local_built_sample()
  result <- create_baseline(con, "Next")
  expect_false(result$ok)
  expect_identical(nrow(result$blockers), 8L)
  expect_identical(as.integer(table(result$blockers$group)[c("placeholder", "unreviewed", "stale")]), c(4L, 3L, 1L))
  expect_identical(nrow(DBI::dbGetQuery(con, "SELECT id FROM baseline")), 1L)
  expect_identical(next_baseline_id(con), "BL-04")
})

test_that("a clean project baselines, and Compare then shows nothing", {
  con <- local_project()
  write_element(con, "C-1", list(kind = "component", name = "a"))
  result <- create_baseline(con, "First", "Note")
  expect_true(result$ok)
  expect_identical(result$id, "BL-01")
  expect_length(compare_baselines(con, "BL-01"), 0L)
  expect_identical(latest_baseline(con), "BL-01")
  write_element(con, "C-1", list(name = "b"))
  expect_length(compare_baselines(con, "BL-01"), 1L)
  second <- create_baseline(con, "Second")
  expect_identical(length(compare_baselines(con, "BL-01", second$id)), 1L)
  expect_length(compare_baselines(con, second$id), 0L)
})

test_that("rollback restores the baseline, saves a safety baseline, and can be undone", {
  con <- local_built_sample()
  revisions <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM revision")$n
  working_hash <- DBI::dbGetQuery(con, "SELECT num_value FROM property_value WHERE id = 'V-0013'")$num_value
  result <- rollback_to_baseline(con, "BL-03")
  expect_identical(result$changed, 4L)
  expect_equal(read_entity(con, "property_value", "V-0013")$num_value, 5000)
  expect_null(read_entity(con, "interface_field", "FLD-0005"))
  expect_match(read_entity(con, "requirement", "SYS-002")$text, "5 km", fixed = TRUE)
  expect_true(DBI::dbGetQuery(con, "SELECT is_safety FROM baseline WHERE id = ?", params = list(result$safety))$is_safety == 1L)
  expect_gt(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM revision")$n, revisions)
  # The field diff against the baseline is empty, though the revs have moved on.
  diffs <- unlist(lapply(compare_entities(con, "BL-03"), function(e) length(e$diff)))
  expect_true(all(diffs == 0L))
  expect_length(compare_baselines(con, "BL-03"), 0L)
  # A safety baseline isn't what Compare starts from.
  expect_identical(latest_baseline(con), "BL-03")

  back <- rollback_to_baseline(con, result$safety)
  expect_equal(read_entity(con, "property_value", "V-0013")$num_value, working_hash)
  expect_identical(read_entity(con, "interface_field", "FLD-0005")$range_state, "placeholder")
  expect_match(read_entity(con, "requirement", "SYS-002")$text, "8 km", fixed = TRUE)
  expect_false(is.null(back$safety))
})

test_that("rollback restores deleted trees in order, and leaves drafts alone", {
  con <- local_built_sample()
  draft <- save_draft(con, "The UAV shall float.", fields = list(level = "system"))
  delete_entity(con, "element", "C-010")
  delete_entity(con, "connector", "K-0001")
  write_element(con, fields = list(kind = "component", name = "Extra", parent_id = "C-000"))
  rollback_to_baseline(con, "BL-03")
  expect_identical(read_entity(con, "element", "C-010")$origin, "ai")
  expect_false(is.null(read_entity(con, "port", "P-0023")))
  expect_false(is.null(read_entity(con, "connector", "K-0001")))
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM element WHERE name = 'Extra'")$n, 0L)
  expect_false(is.null(read_entity(con, "requirement", draft$id)))
})

test_that("a rollback that can't complete changes nothing", {
  con <- local_built_sample()
  write_requirement(con, "SYS-002", list(short_title = "Changed"))
  local_mocked_bindings(restorable_fields = function(kind, data) stop("boom"))
  before <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM baseline")$n
  expect_error(rollback_to_baseline(con, "BL-03"), "boom")
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM baseline")$n, before)
  expect_identical(read_entity(con, "requirement", "SYS-002")$short_title, "Changed")
})

test_that("the battery impact set matches its pinned fixture", {
  con <- local_built_sample()
  items <- impact_set(con, list(list(kind = "property_value", id = "V-0013"), list(kind = "property_value", id = "V-0003")))
  pinned <- utils::read.csv(test_path("fixtures", "battery_impact.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_equal(items[c("kind", "id", "hops", "path")], pinned[c("kind", "id", "hops", "path")])
})

test_that("impact is repeatable, respects max_hops, and shows the path", {
  con <- local_built_sample()
  sources <- list(list(kind = "property_value", id = "V-0013"))
  first <- impact_set(con, sources)
  expect_identical(first, impact_set(con, sources))
  near <- impact_set(con, sources, max_hops = 1L)
  expect_true(all(near$hops == 1L))
  expect_true("C-003" %in% near$id)
  expect_identical(first$path[first$id == "C-003"], "Power System · Battery capacity → Power System")
  expect_true("SYS-001" %in% first$id)
  expect_true("TC-007" %in% first$id)
  expect_true("SC-003" %in% first$id)
  expect_false(any(first$kind == "property_value"))
})

test_that("a mass change doesn't come back through its budget to every other mass", {
  items <- impact_set(local_built_sample(), list(list(kind = "property_value", id = "V-0003")), max_hops = 2L)
  expect_true("B-001" %in% items$id)
  expect_false("C-007" %in% items$id[items$hops == 2L & grepl("budget", items$path)])
})

test_that("an added interface field reaches what uses its interface", {
  items <- impact_set(local_built_sample(), list(list(kind = "interface_field", id = "FLD-0005")), max_hops = 2L)
  expect_true(all(c("IF-002", "C-001", "C-004", "SC-002", "SM-001") %in% items$id))
})

test_that("the impact table carries stale evidence on the test case", {
  con <- local_built_sample()
  items <- card_impact(con, battery_card(con))
  expect_match(items$stale[items$id == "TC-007"], "Battery capacity was 5000 mAh", fixed = TRUE)
  expect_true(is.na(items$stale[items$id == "SYS-001"]))
})

test_that("the rating prompt uses I handles, ratings map back, and unknown ones are dropped", {
  con <- local_built_sample()
  card <- battery_card(con)
  items <- card_impact(con, card)
  prompt <- impact_rating_prompt(con, card, items)
  expect_match(prompt$message, "I1 ", fixed = TRUE)
  expect_match(prompt$message, "31 / 35 min (misses limit)", fixed = TRUE)
  result <- list(ok = TRUE, call_id = NA_integer_, value = list(ratings = list(
    list(n = "I1", significance = "High", reason = "It drops by 16 percent"),
    list(n = "I999", significance = "Low", reason = "Nope"))))
  rated <- apply_ratings(con, items, result)
  expect_identical(rated$significance[1], "High")
  expect_match(rated$reason[1], "by [value] percent", fixed = TRUE)
  expect_true(all(is.na(rated$significance[-1])))
  expect_identical(DBI::dbGetQuery(con, "SELECT count(*) AS n FROM audit_log WHERE action = 'ai_dropped'")$n, 1L)
})

test_that("each impact prompt's dialog text is what's sent", {
  con <- local_built_sample()
  set_task_route(con, "impact_narrative", list(provider_id = "lmstudio", model = "m", fallback_provider_id = NA, fallback_model = NA))
  card <- battery_card(con)
  items <- card_impact(con, card)
  fake <- fake_client(list(list(ratings = list()), list(technical = "T", stakeholder = "S", decision = "")))
  local_mocked_bindings(build_llm_client = fake$builder)
  rating <- impact_rating_prompt(con, card, items)
  narrative <- impact_narrative_prompt(con, card, items)
  result <- settle(run_impact_ai_async(con, card, items))
  expect_identical(fake$log$messages, c(rating$message, narrative$message))
  expect_identical(fake$log$systems, c(rating$system, narrative$system))
  expect_true(result$ok)
  # Now it's cached for this model: nothing more is sent.
  hit <- impact_ai_from_cache(con, card, items)
  expect_identical(hit$narrative$technical, "T")
  settle(run_impact_ai_async(con, card, items))
  expect_length(fake$log$messages, 2L)
})

test_that("stakeholder text has no ids or interface names, and numbers come from facts", {
  con <- local_built_sample()
  card <- battery_card(con)
  value <- list(technical = "SYS-001 misses: 31 / 35 min, down 16 percent, from TC-007.",
                stakeholder = "SYS-001 on PowerBus28V and C-003 «HW» means 31 minutes, not 37.",
                decision = "Keep the battery on C-003, or accept 31 minutes.")
  out <- process_narrative(con, card, value)
  expect_match(out$technical, "SYS-001 misses: 31 / 35 min, down [value] percent", fixed = TRUE)
  expect_false(grepl("SYS-|C-0|PowerBus|«", out$stakeholder))
  expect_match(out$stakeholder, "Endurance on a connection and Power System", fixed = TRUE)
  expect_match(out$stakeholder, "31 minutes, not [value].", fixed = TRUE)
  expect_match(out$decision, "Keep the battery on Power System", fixed = TRUE)
  expect_identical(out$invented, 2L)
})

test_that("the decision only shows when something fails", {
  con <- local_built_sample()
  card <- battery_card(con)
  write_property_value(con, "V-0013", list(num_value = 5000))
  out <- process_narrative(con, card, list(technical = "", stakeholder = "", decision = "Choose."))
  expect_true(is.na(out$decision))
})

test_that("the stakeholder overview uses plain words and code's numbers", {
  con <- local_built_sample()
  overview <- stakeholder_overview(con)
  expect_identical(nrow(overview$parts), 7L)
  expect_false("C-010" %in% overview$parts$id)
  meters <- overview$meters
  expect_identical(vapply(meters, function(m) m$label, ""), c("Weight", "Power use in flight", "Flight time"))
  expect_identical(meters[[1]]$value_text, "3.31 of 3.60 kg")
  expect_identical(meters[[3]]$value_text, "31 min (need 35)")
  expect_false(meters[[3]]$pass)
  expect_match(meters[[3]]$line, "A decision is needed")
  expect_true(any(grepl("Payload Camera isn't fully connected", overview$questions)))
  expect_true(any(grepl("1 requirement isn't assigned", overview$questions)))
  expect_false(any(grepl("[A-Z]+-[0-9]", overview$questions)))
})

test_that("the AI summary fills only missing summaries, in plain words", {
  con <- local_built_sample()
  write_element(con, "C-002", list(plain_summary = NA_character_, plain_summary_origin = NA_character_))
  prompt <- stakeholder_prompt(con)
  handle <- prompt$candidates$n[prompt$candidates$id == "C-002"]
  other <- prompt$candidates$n[prompt$candidates$id == "C-001"]
  result <- list(call_id = NA_integer_, value = list(paragraph = "It flies for 31 min over C-002.",
    summaries = list(list(handle = handle, summary = "The laptop app."), list(handle = other, summary = "Overwrite?"), list(handle = "C99", summary = "No"))))
  store_stakeholder_answer(con, prompt, result)
  expect_identical(read_entity(con, "element", "C-002")$plain_summary, "The laptop app.")
  expect_identical(read_entity(con, "element", "C-002")$plain_summary_origin, "ai")
  expect_match(read_entity(con, "element", "C-001")$plain_summary, "Work out where")
  overview <- stakeholder_overview(con)
  expect_true(overview$paragraph_is_ai)
  expect_identical(overview$paragraph, "It flies for [value] min over Ground Control Software.")
})

test_that("the brief holds plain words and code's numbers", {
  html <- brief_html(local_built_sample())
  expect_match(html, "3.31 of 3.60 kg", fixed = TRUE)
  expect_match(html, "31 min (need 35)", fixed = TRUE)
  expect_match(html, "Open questions", fixed = TRUE)
  expect_false(grepl("C-00|SYS-0|PowerBus", html))
})

test_that("the PDF brief prints through Chrome", {
  testthat::skip_if_not_installed("chromote")
  testthat::skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)), "no Chrome")
  path <- withr::local_tempfile(fileext = ".pdf")
  write_brief_pdf(local_built_sample(), path)
  expect_identical(readChar(path, 5L), "%PDF-")
})
