# A stand-in for an ellmer chat. Records what it was sent, and answers from a queue.
options(modelbench.no_keyring = TRUE)

fake_client <- function(answers, log = new.env()) {
  log$systems <- character(0)
  log$messages <- character(0)
  log$built <- list()
  next_answer <- local({
    i <- 0L
    function() {
      i <<- i + 1L
      answers[[min(i, length(answers))]]
    }
  })
  respond <- function(message) {
    log$messages <- c(log$messages, message)
    answer <- next_answer()
    if (inherits(answer, "error")) stop(answer)
    answer
  }
  builder <- function(provider, model, system_prompt) {
    log$systems <- c(log$systems, system_prompt)
    log$built[[length(log$built) + 1L]] <- list(provider = provider$id, model = model)
    list(
      chat_structured = function(message, type, convert) respond(message),
      chat_structured_async = function(message, type, convert) {
        tryCatch(promises::promise_resolve(respond(message)), error = function(e) promises::promise_reject(e))
      },
      get_tokens = function() data.frame(input = 1000, output = 100),
      get_cost = function() 0.5
    )
  }
  list(builder = builder, log = log)
}

good_coach_answer <- function(rewrite = "", questions = list(list(text = "Is the bus monitored?", context = "No requirement says.")), conflicts = list()) {
  list(criteria = list(singular = "pass", verifiable = "pass", design_independent = "question", conflicts = "none", fits_scope = "pass"),
       questions = questions, example_rewrite = rewrite, conflicts = conflicts)
}

gap_item <- function(kind = "question", title = "What brings it down?", target = "", name = "", stereotype = "", draft = "", ports = list(), body = "Because.") {
  list(kind = kind, title = title, body = body, target = target, confidence = "medium", name = name, stereotype = stereotype, draft = draft, ports = ports)
}

# Lets promises settle.
settle <- function(promise) {
  out <- NULL
  promises::then(promise, function(value) out <<- value)
  for (i in 1:50) {
    later::run_now(0.01)
    if (!is.null(out)) break
  }
  out
}

set_light_model <- function(con, model = "local-test") {
  set_task_route(con, "req_light_check", list(model = model))
}
