#!/usr/bin/env Rscript
# Measures the coach against real models. This spends money on cloud routes, so it
# only runs when asked: Rscript tools/llm_eval.R --confirm-billed [--local-only]
# Reports the gate pass rate of example rewrites, the invented-number rate after
# validation (must be 0) and latency per provider. Never run in CI.
args <- commandArgs(trailingOnly = TRUE)
if (!"--confirm-billed" %in% args) stop("This run can be billed. Pass --confirm-billed to run it.")
pkgload::load_all("modelbench", quiet = TRUE, helpers = FALSE)

path <- tempfile(fileext = ".sqlite")
con <- build_sample_project(path)
on.exit(DBI::dbDisconnect(con))
local_model <- Sys.getenv("MODELBENCH_EVAL_LOCAL_MODEL", "")
if (nzchar(local_model)) set_task_route(con, "req_light_check", list(model = local_model))
if ("--local-only" %in% args) set_setting(con, "local_only", "1")

drafts <- readLines("modelbench/tests/testthat/fixtures/sample_requirements.txt", warn = FALSE)
dictionary <- model_terms(con)
passing <- drafts[vapply(drafts, function(text) check_requirement(text, dictionary)$passes_gate, TRUE)]
tasks <- c(if (nzchar(local_model)) "req_light_check", "req_deep_check")

rows <- list()
for (task in tasks) {
  for (draft in passing) {
    result <- run_coach(con, draft, task)
    draft_numbers <- regmatches(draft, gregexpr("[0-9][0-9.]*", draft))[[1]]
    example <- if (isTRUE(result$ok)) result$answer$example else list(status = "error", invented = 0L)
    shown <- if (identical(example$status, "ok")) regmatches(example$text, gregexpr("[0-9][0-9.]*", example$text))[[1]] else character(0)
    rows[[length(rows) + 1L]] <- data.frame(
      task = task, provider = result$provider_id, ok = isTRUE(result$ok), kind = if (is.null(result$kind)) NA_character_ else result$kind,
      example = example$status, invented_blocked = if (is.null(example$invented)) 0L else example$invented,
      invented_shown = length(setdiff(sub("\\.$", "", shown), sub("\\.$", "", draft_numbers))),
      latency_s = result$duration_ms / 1000, stringsAsFactors = FALSE
    )
  }
}
out <- do.call(rbind, rows)
print(out)
for (provider in unique(out$provider)) {
  one <- out[out$provider %in% provider, ]
  gated <- one[one$example %in% c("ok", "failed"), ]
  cat(sprintf("\n%s: %d calls, %d ok\n", provider, nrow(one), sum(one$ok)))
  cat(sprintf("  example rewrites passing the gate: %d of %d\n", sum(gated$example == "ok"), nrow(gated)))
  cat(sprintf("  invented numbers blocked: %d; shown after validation: %d (must be 0)\n", sum(one$invented_blocked), sum(one$invented_shown)))
  cat(sprintf("  latency: median %.1f s, max %.1f s\n", stats::median(one$latency_s, na.rm = TRUE), max(one$latency_s, na.rm = TRUE)))
}
cat(sprintf("\nCost: $%.4f\n", sum(DBI::dbGetQuery(con, "SELECT coalesce(cost_usd, 0) AS c FROM llm_call WHERE id > 1")$c)))
if (sum(out$invented_shown) > 0L) quit(status = 1L)
