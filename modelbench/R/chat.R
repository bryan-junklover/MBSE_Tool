# "Ask the model" (handoff §5.11): a read-only chat. The model reads through tools that
# return handles and plain data, never ids, and nothing here can write to the model.

CHAT_SYSTEM <- paste(
  "You answer questions about a system model, using only what the tools return.", PLAIN_TEXT_RULE,
  "Refer to things by name. Use numbers only as the tools give them; never work one out yourself.",
  "You can't change the model. If asked to, say the engineer makes changes in the app.",
  "If the tools don't have the answer, say so.", sep = "\n"
)

CHAT_PREFIXES <- c(element = "E", interface = "IF", requirement = "R", test_case = "TC", scenario = "SC", state_machine = "SM")

# Handles live for one conversation, so the model's words can be mapped back.
new_chat_registry <- function() {
  registry <- new.env()
  registry$items <- data.frame(handle = character(0), kind = character(0), id = character(0), name = character(0), stringsAsFactors = FALSE)
  registry$calls <- list()
  registry$numbers <- character(0)
  registry
}

chat_handle <- function(registry, kind, id, name) {
  items <- registry$items
  hit <- which(items$kind == kind & items$id == id)
  if (length(hit) == 1L) return(items$handle[hit])
  prefix <- CHAT_PREFIXES[[kind]]
  handle <- sprintf("%s%d", prefix, sum(startsWith(items$handle, prefix) & grepl(paste0("^", prefix, "[0-9]+$"), items$handle)) + 1L)
  registry$items <- rbind(items, data.frame(handle = handle, kind = kind, id = id, name = name, stringsAsFactors = FALSE))
  handle
}

# A handle the model was given, or a name it read: either finds the item.
chat_resolve <- function(con, registry, text, kind) {
  text <- trimws(as.character(text))
  items <- registry$items[registry$items$kind == kind, , drop = FALSE]
  hit <- items[items$handle == text | tolower(items$name) == tolower(text), , drop = FALSE]
  if (nrow(hit) > 0L) return(list(id = hit$id[1], name = hit$name[1]))
  table <- switch(kind, element = "element", interface = "interface")
  row <- DBI::dbGetQuery(con, sprintf("SELECT id, name FROM %s WHERE lower(name) = lower(?)", table), params = list(text))
  if (nrow(row) == 0L) return(NULL)
  list(id = row$id[1], name = row$name[1])
}

# Every number a tool hands back may appear in the answer; anything else is invented.
chat_note_numbers <- function(registry, value) {
  text <- paste(unlist(value), collapse = " ")
  registry$numbers <- unique(c(registry$numbers, unlist(regmatches(text, gregexpr("[0-9]+(\\.[0-9]+)?", text)))))
  value
}

chat_get_element <- function(con, registry, handle) {
  found <- chat_resolve(con, registry, handle, "element")
  if (is.null(found)) return(list(error = sprintf("No element called %s.", handle)))
  row <- read_entity(con, "element", found$id)
  props <- element_properties(con, found$id)
  ports <- element_ports(con, found$id)
  children <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE parent_id = ? ORDER BY sort_order, name", params = list(found$id))
  requirements <- element_requirements(con, found$id)
  chat_note_numbers(registry, list(
    handle = chat_handle(registry, "element", found$id, found$name), name = row$name, kind = row$kind, status = row$status,
    review = if (row$review_state == "unreviewed") "unreviewed AI draft" else "reviewed", description = row$description,
    properties = lapply(seq_len(nrow(props)), function(i) list(name = props$label[i], value = if (props$value_state[i] == "placeholder") "not set yet" else paste(format(props$num_value[i], trim = TRUE), if (is.na(props$unit[i])) "" else props$unit[i]))),
    ports = lapply(seq_len(nrow(ports)), function(i) list(name = ports$name[i], direction = ports$direction[i], interface = ports$interface[i], connected = isTRUE(as.logical(ports$connected[i])))),
    parts = lapply(seq_len(nrow(children)), function(i) list(handle = chat_handle(registry, "element", children$id[i], children$name[i]), name = children$name[i])),
    requirements = lapply(seq_len(nrow(requirements)), function(i) list(handle = chat_handle(registry, "requirement", requirements$id[i], requirements$id[i]), title = requirements$short_title[i]))
  ))
}

chat_get_interface <- function(con, registry, handle) {
  found <- chat_resolve(con, registry, handle, "interface")
  if (is.null(found)) return(list(error = sprintf("No interface called %s.", handle)))
  row <- read_entity(con, "interface", found$id)
  fields <- interface_fields(con, found$id)
  used <- interface_where_used(con, found$id)
  chat_note_numbers(registry, list(
    handle = chat_handle(registry, "interface", found$id, found$name), name = row$name, flow = row$flow_type,
    rate = if (is.na(row$rate_value)) "not set" else paste(format(row$rate_value, trim = TRUE), row$rate_unit), transport = row$transport,
    fields = lapply(seq_len(nrow(fields)), function(i) {
      list(name = fields$name[i], type = fields$value_type[i], unit = fields$unit[i], range = format_field_range(fields$range_state[i], fields$min_value[i], fields$max_value[i]))
    }),
    used_by = lapply(seq_len(nrow(used$ports)), function(i) list(element = chat_handle(registry, "element", used$ports$element_id[i], used$ports$element[i]), name = used$ports$element[i], port = used$ports$name[i]))
  ))
}

chat_list_requirements <- function(con, registry, filter = "") {
  rows <- requirement_table(con)
  rows <- rows[rows$status != "deprecated", , drop = FALSE]
  if (nzchar(trimws(filter))) {
    keep <- grepl(tolower(filter), tolower(rows$text), fixed = TRUE) | grepl(tolower(filter), tolower(ifelse(is.na(rows$short_title), "", rows$short_title)), fixed = TRUE) | rows$level == tolower(filter)
    rows <- rows[keep, , drop = FALSE]
  }
  chat_note_numbers(registry, list(count = nrow(rows), requirements = lapply(seq_len(min(nrow(rows), 40L)), function(i) {
    list(handle = chat_handle(registry, "requirement", rows$id[i], rows$id[i]), level = rows$level[i], status = rows$status[i], title = rows$short_title[i], text = rows$text[i])
  })))
}

chat_rollup <- function(con, registry, property, scope = "") {
  found <- if (nzchar(trimws(scope))) chat_resolve(con, registry, scope, "element") else NULL
  root <- if (is.null(found)) default_structure_root(con) else found$id
  result <- tryCatch(rollup(con, property, root), error = function(e) NULL)
  if (is.null(result)) return(list(error = sprintf("There's no roll-up for %s.", property)))
  budgets <- budget_status(con)
  chat_note_numbers(registry, list(property = property, scope = if (is.null(found)) "the whole system" else found$name,
                                   total = paste(format_amount(result$value), result$unit), complete = result$excluded_placeholders == 0L,
                                   budgets = lapply(seq_len(nrow(budgets)), function(i) list(label = budgets$label[i], status = budgets$text[i], within_limit = budgets$pass[i]))))
}

chat_analysis <- function(con, registry, name) {
  rows <- analysis_status(con)
  rows <- rows[tolower(rows$analysis) == tolower(trimws(name)) | trimws(name) == "", , drop = FALSE]
  if (nrow(rows) == 0L) return(list(error = sprintf("No analysis called %s. Known: %s.", name, paste(analysis_status(con)$analysis, collapse = ", "))))
  chat_note_numbers(registry, lapply(seq_len(nrow(rows)), function(i) list(analysis = rows$analysis[i], result = rows$text[i], meets_limit = rows$pass[i])))
}

chat_impact <- function(con, registry, handle) {
  found <- NULL
  for (kind in c("element", "interface")) if (is.null(found)) found <- c(chat_resolve(con, registry, handle, kind), kind = kind)
  if (is.null(found$id)) return(list(error = sprintf("Nothing called %s to trace from.", handle)))
  items <- impact_set(con, list(list(kind = found$kind, id = found$id)))
  chat_note_numbers(registry, list(from = found$name, reached = lapply(seq_len(min(nrow(items), 30L)), function(i) {
    kind <- items$kind[i]
    label <- if (kind %in% names(CHAT_PREFIXES)) chat_handle(registry, kind, items$id[i], items$label[i]) else items$label[i]
    list(item = label, name = items$label[i], hops = items$hops[i], path = items$path[i])
  }), total = nrow(items)))
}

chat_find <- function(con, registry, text) {
  like <- paste0("%", trimws(text), "%")
  elements <- DBI::dbGetQuery(con, "SELECT id, name FROM element WHERE name LIKE ? ORDER BY name LIMIT 15", params = list(like))
  interfaces <- DBI::dbGetQuery(con, "SELECT id, name FROM interface WHERE name LIKE ? ORDER BY name LIMIT 15", params = list(like))
  requirements <- DBI::dbGetQuery(con, "SELECT id, short_title FROM requirement WHERE status <> 'deprecated' AND (text LIKE ? OR short_title LIKE ?) ORDER BY id LIMIT 15", params = list(like, like))
  list(
    elements = lapply(seq_len(nrow(elements)), function(i) list(handle = chat_handle(registry, "element", elements$id[i], elements$name[i]), name = elements$name[i])),
    interfaces = lapply(seq_len(nrow(interfaces)), function(i) list(handle = chat_handle(registry, "interface", interfaces$id[i], interfaces$name[i]), name = interfaces$name[i])),
    requirements = lapply(seq_len(nrow(requirements)), function(i) list(handle = chat_handle(registry, "requirement", requirements$id[i], requirements$id[i]), title = requirements$short_title[i]))
  )
}

# The tools, each recording its call for the chips above the answer.
chat_tools <- function(con, registry) {
  wrap <- function(name, fun) {
    force(fun)
    # The wrapper keeps the tool's own arguments, which ellmer checks against the types.
    wrapper <- function() {
      args <- mget(names(formals(fun)), environment())
      registry$calls[[length(registry$calls) + 1L]] <- list(name = name, args = paste(vapply(args, function(a) as.character(a)[1], ""), collapse = ", "))
      jsonlite::toJSON(do.call(fun, args), auto_unbox = TRUE, null = "null", na = "null")
    }
    formals(wrapper) <- formals(fun)
    wrapper
  }
  s <- function(text) ellmer::type_string(text)
  so <- function(text) ellmer::type_string(text, required = FALSE)
  list(
    ellmer::tool(wrap("get_element", function(handle) chat_get_element(con, registry, handle)), "A component, function or external element: properties, ports, parts and linked requirements.", name = "get_element", arguments = list(handle = s("Its handle, such as E1, or its name."))),
    ellmer::tool(wrap("get_interface", function(handle) chat_get_interface(con, registry, handle)), "An interface: flow, rate, fields and where it's used.", name = "get_interface", arguments = list(handle = s("Its handle, such as IF1, or its name."))),
    ellmer::tool(wrap("list_requirements", function(filter = "") chat_list_requirements(con, registry, filter)), "Requirements, optionally filtered by a word or a level.", name = "list_requirements", arguments = list(filter = so("A word to match, or stakeholder, system or subsystem."))),
    ellmer::tool(wrap("rollup", function(property, scope = "") chat_rollup(con, registry, property, scope)), "Totals a property such as mass or power over an element and its parts, with budget status.", name = "rollup", arguments = list(property = s("The property, such as mass or power."), scope = so("An element handle or name; blank for the whole system."))),
    ellmer::tool(wrap("analysis", function(name = "") chat_analysis(con, registry, name)), "The result of an analysis such as endurance, against its limit.", name = "analysis", arguments = list(name = so("The analysis name; blank for all."))),
    ellmer::tool(wrap("impact_set", function(change) chat_impact(con, registry, change)), "Everything a change to an element or interface would reach, with the path.", name = "impact_set", arguments = list(change = s("The handle or name of what changes."))),
    ellmer::tool(wrap("find", function(text) chat_find(con, registry, text)), "Finds elements, interfaces and requirements by a word.", name = "find", arguments = list(text = s("The word to look for.")))
  )
}

# Handles become names; numbers the tools never gave become [value] (BUILD_PLAN §2).
chat_display_text <- function(registry, text) {
  items <- registry$items
  items <- items[order(-nchar(items$handle)), , drop = FALSE]
  for (i in seq_len(nrow(items))) {
    shown <- if (items$kind[i] == "requirement") items$id[i] else items$name[i]
    text <- gsub(paste0("\\b", items$handle[i], "\\b"), shown, text, perl = TRUE)
  }
  ids <- items$id[items$kind == "requirement"]
  in_names <- unlist(regmatches(c(ids, items$name), gregexpr("[0-9]+(\\.[0-9]+)?", c(ids, items$name))))
  replace_unknown_numbers(text, c(registry$numbers, in_names), keep_tokens = ids)$text
}

# Elements an answer is about: those named in it, else those its tools looked at.
chat_referenced_elements <- function(registry, text) {
  items <- registry$items[registry$items$kind == "element", , drop = FALSE]
  named <- items$id[vapply(items$name, function(name) grepl(name, text, fixed = TRUE), TRUE)]
  unique(named)
}

# The first route that can answer now: a local model, or a cloud one with a key. The
# chat keeps one conversation open, so it doesn't fall back mid-way like other tasks.
chat_attempt <- function(con) {
  attempts <- route_attempts(con, "model_chat")
  ready <- Filter(function(a) is.null(a$blocked) && (provider_row(con, a$provider_id)$is_local == 1L || provider_has_key(a$provider_id)), attempts)
  if (length(ready) > 0L) return(ready[[1]])
  blocked <- Find(function(a) !is.null(a$blocked), attempts)
  if (!is.null(blocked)) return(blocked)
  list(blocked = "no_key", provider_id = attempts[[1]]$provider_id, model = attempts[[1]]$model)
}

# shinychat sends a question as text, or as a list holding the text and any attachments.
chat_question_text <- function(input) {
  if (is.character(input)) return(paste(input, collapse = "\n"))
  for (key in c("text", "content", "prompt")) if (is.character(input[[key]])) return(paste(input[[key]], collapse = "\n"))
  paste(unlist(Filter(is.character, input)), collapse = "\n")
}

CHAT_PROMPT <- list(task = "model_chat", prompt_builder = "chat", prompt_version = "chat v1", system = CHAT_SYSTEM, schema = s_string(), candidates_json = NULL)

# One question. The client is kept across turns so the conversation carries on; it's
# rebuilt only when the route changes. Every call is logged like any other.
run_chat_async <- function(con, registry, question, client = NULL) {
  safely_async(function() {
    snapshot <- model_hash(con)
    prompt <- c(CHAT_PROMPT, list(message = chat_question_text(question)))
    attempt <- chat_attempt(con)
    if (!is.null(attempt$blocked)) return(promises::promise_resolve(finish_blocked(con, prompt, attempt, snapshot, list(kind = NULL, id = NULL))))
    outgoing <- outgoing_prompt(con, prompt, attempt)
    if (is.null(client) || !identical(client$route, paste(attempt$provider_id, attempt$model))) {
      client <- list(route = paste(attempt$provider_id, attempt$model), chat = build_llm_client(outgoing$provider, attempt$model, outgoing$system))
      client$chat$set_tools(chat_tools(con, registry))
    }
    registry$calls <- list()
    started <- Sys.time()
    promises::then(
      client$chat$chat_async(outgoing$message),
      onFulfilled = function(answer) {
        duration <- round(as.numeric(Sys.time() - started, units = "secs") * 1000)
        text <- unredact(as.character(answer), outgoing$terms)
        usage <- client_usage(client$chat)
        id <- log_attempt(con, prompt, attempt, outgoing, "ok", snapshot, list(kind = NULL, id = NULL), response = text, usage = usage,
                          cost = call_cost(outgoing$provider, attempt$model, usage$input, usage$output, client$chat), duration_ms = duration)
        list(ok = TRUE, text = chat_display_text(registry, text), calls = registry$calls, call_id = id, client = client,
             provider_id = attempt$provider_id, model = attempt$model, elements = chat_referenced_elements(registry, chat_display_text(registry, text)))
      },
      onRejected = function(e) {
        failed <- finish_failure(con, prompt, attempt, outgoing, conditionMessage(e), started, snapshot, list(kind = NULL, id = NULL))
        c(failed, list(client = client))
      }
    )
  })
}
