# The guard and effect language (handoff §5.8): a small recursive-descent parser and a
# tree-walking evaluator. Nothing here ever reaches eval() or parse().

EXPR_MAX_LENGTH <- 2000L
EXPR_MAX_DEPTH <- 40L
EXPR_KEYWORDS <- c("and", "or", "not", "true", "false", "prop")

expr_tokens <- function(text) {
  if (nchar(text) > EXPR_MAX_LENGTH) refuse("That expression is too long.")
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  store <- new.env()
  store$tokens <- list()
  add <- function(type, value) store$tokens[[length(store$tokens) + 1L]] <- list(type = type, value = value)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (grepl("^\\s$", ch)) {
      i <- i + 1L
    } else if (grepl("^[0-9.]$", ch)) {
      j <- i
      while (j < n && grepl("^[0-9.]$", chars[j + 1L])) j <- j + 1L
      word <- paste(chars[i:j], collapse = "")
      if (!grepl("^([0-9]+(\\.[0-9]*)?|\\.[0-9]+)$", word)) refuse("\"", word, "\" isn't a number.")
      add("number", as.numeric(word))
      i <- j + 1L
    } else if (grepl("^[A-Za-z_]$", ch)) {
      j <- i
      while (j < n && grepl("^[A-Za-z0-9_]$", chars[j + 1L])) j <- j + 1L
      word <- paste(chars[i:j], collapse = "")
      i <- j + 1L
      if (word == "prop") {
        rest <- if (i <= n) paste(chars[i:n], collapse = "") else ""
        found <- regmatches(rest, regexpr("^\\s*\\(([^()]*)\\)", rest))
        if (length(found) == 0L) refuse("Write a model property as prop(Element Name.property).")
        inner <- trimws(sub("\\)$", "", sub("^\\s*\\(", "", found)))
        dot <- regexpr("\\.[^.]*$", inner)
        if (dot < 2L || dot >= nchar(inner)) refuse("Write a model property as prop(Element Name.property).")
        add("prop", list(element = trimws(substr(inner, 1L, dot - 1L)), property = trimws(substr(inner, dot + 1L, nchar(inner)))))
        i <- i + nchar(found)
      } else if (word %in% EXPR_KEYWORDS) {
        add(word, word)
      } else {
        add("ident", word)
      }
    } else {
      two <- if (i < n) paste0(ch, chars[i + 1L]) else ""
      if (two %in% c("==", "!=", "<=", ">=", ":=")) {
        add(two, two)
        i <- i + 2L
      } else if (ch %in% c("<", ">", "+", "-", "*", "/", "(", ")", ";")) {
        add(ch, ch)
        i <- i + 1L
      } else {
        refuse("\"", ch, "\" can't be used in an expression.")
      }
    }
  }
  store$tokens
}

# Parses with closures over a cursor, one function per grammar rule.
expr_parser <- function(tokens) {
  cursor <- new.env()
  cursor$at <- 1L
  cursor$depth <- 0L
  peek <- function() if (cursor$at <= length(tokens)) tokens[[cursor$at]]$type else "end"
  advance <- function() {
    token <- tokens[[cursor$at]]
    cursor$at <- cursor$at + 1L
    token
  }
  nested <- function(rule) {
    cursor$depth <- cursor$depth + 1L
    if (cursor$depth > EXPR_MAX_DEPTH) refuse("That expression is nested too deeply.")
    value <- rule()
    cursor$depth <- cursor$depth - 1L
    value
  }
  binary <- function(next_rule, ops) {
    left <- next_rule()
    while (peek() %in% ops) {
      op <- advance()$type
      left <- list(node = "binary", op = op, left = left, right = next_rule())
    }
    left
  }
  or_rule <- function() binary(and_rule, "or")
  and_rule <- function() binary(not_rule, "and")
  not_rule <- function() {
    if (peek() != "not") return(cmp_rule())
    advance()
    list(node = "not", arg = nested(not_rule))
  }
  cmp_rule <- function() {
    left <- sum_rule()
    if (!peek() %in% c("==", "!=", "<", "<=", ">", ">=")) return(left)
    op <- advance()$type
    list(node = "binary", op = op, left = left, right = sum_rule())
  }
  sum_rule <- function() binary(term_rule, c("+", "-"))
  term_rule <- function() binary(atom_rule, c("*", "/"))
  atom_rule <- function() {
    type <- peek()
    if (type == "end") refuse("That expression stops too early.")
    token <- advance()
    switch(type,
           number = list(node = "number", value = token$value),
           true = list(node = "bool", value = TRUE),
           false = list(node = "bool", value = FALSE),
           ident = list(node = "ident", name = token$value),
           prop = list(node = "prop", element = token$value$element, property = token$value$property),
           `(` = {
             inner <- nested(or_rule)
             if (peek() != ")") refuse("A \"(\" has no matching \")\".")
             advance()
             inner
           },
           refuse("\"", token$value, "\" isn't expected there."))
  }
  effect_rule <- function() {
    assigns <- list()
    repeat {
      if (peek() != "ident") refuse("An effect assigns to a variable: name := value.")
      name <- advance()$value
      if (peek() != ":=") refuse("An effect assigns to a variable: name := value.")
      advance()
      assigns[[length(assigns) + 1L]] <- list(name = name, value = or_rule())
      if (peek() != ";") break
      advance()
    }
    assigns
  }
  finish <- function(value) {
    force(value)
    if (peek() != "end") refuse("\"", tokens[[cursor$at]]$value, "\" isn't expected there.")
    value
  }
  list(expression = function() finish(or_rule()), effect = function() finish(effect_rule()))
}

parse_guard <- function(text) expr_parser(expr_tokens(text))$expression()
parse_effect <- function(text) expr_parser(expr_tokens(text))$effect()

# What an expression may name: variables as name -> type, properties looked up by a function.
expr_scope <- function(con, sm_id) {
  variables <- DBI::dbGetQuery(con, "SELECT name, value_type FROM sm_variable WHERE sm_id = ?", params = list(sm_id))
  list(variables = stats::setNames(variables$value_type, variables$name), property = function(element, property) find_model_property(con, element, property))
}

find_model_property <- function(con, element, property) {
  rows <- DBI::dbGetQuery(con, "
    SELECT v.id, v.value_state, v.num_value FROM property_value v
      JOIN property_def d ON d.id = v.property_def_id JOIN element e ON e.id = v.owner_id
     WHERE v.owner_kind = 'element' AND e.name = ? AND (d.name = ? OR d.label = ?)", params = list(element, property, property))
  if (nrow(rows) == 0L) return(NULL)
  rows[1L, , drop = FALSE]
}

# The type of an expression, refusing unknown identifiers and mixed types.
expr_type <- function(node, scope) {
  switch(node$node,
         number = "number",
         bool = "bool",
         ident = {
           if (!node$name %in% names(scope$variables)) refuse("\"", node$name, "\" isn't a variable of this state machine.")
           unname(scope$variables[[node$name]])
         },
         prop = {
           if (is.null(scope$property(node$element, node$property))) refuse("There's no property \"", node$property, "\" on \"", node$element, "\".")
           "number"
         },
         not = {
           if (expr_type(node$arg, scope) != "bool") refuse("\"not\" needs a true or false value.")
           "bool"
         },
         binary = {
           left <- expr_type(node$left, scope)
           right <- expr_type(node$right, scope)
           if (node$op %in% c("and", "or")) {
             if (left != "bool" || right != "bool") refuse("\"", node$op, "\" needs true or false values.")
             "bool"
           } else if (node$op %in% c("==", "!=")) {
             if (left != right) refuse("\"", node$op, "\" compares a ", left, " with a ", right, ".")
             "bool"
           } else {
             if (left != "number" || right != "number") refuse("\"", node$op, "\" needs numbers.")
             if (node$op %in% c("<", "<=", ">", ">=")) "bool" else "number"
           }
         })
}

# Gate checks, run when a guard or effect is edited. Blank is allowed: no guard.
check_guard <- function(text, scope) {
  if (is.na(text) || !nzchar(trimws(text))) return(invisible(NULL))
  if (expr_type(parse_guard(text), scope) != "bool") refuse("A guard has to come out true or false.")
  invisible(NULL)
}

check_effect <- function(text, scope) {
  if (is.na(text) || !nzchar(trimws(text))) return(invisible(NULL))
  for (assign in parse_effect(text)) {
    if (!assign$name %in% names(scope$variables)) refuse("\"", assign$name, "\" isn't a variable of this state machine.")
    want <- unname(scope$variables[[assign$name]])
    got <- expr_type(assign$value, scope)
    if (want != got) refuse("\"", assign$name, "\" holds a ", want, ", not a ", got, ".")
  }
  invisible(NULL)
}

expr_value <- function(node, variables, property) {
  switch(node$node,
         number = node$value,
         bool = node$value,
         ident = {
           if (!node$name %in% names(variables)) stop_sim(sprintf("\"%s\" isn't a variable.", node$name))
           variables[[node$name]]
         },
         prop = {
           row <- property(node$element, node$property)
           if (is.null(row)) stop_sim(sprintf("There's no property \"%s\" on \"%s\".", node$property, node$element))
           if (row$value_state != "set" || is.na(row$num_value)) stop_sim(sprintf("%s.%s needs a value", node$element, node$property))
           row$num_value
         },
         not = !expr_value(node$arg, variables, property),
         binary = {
           left <- expr_value(node$left, variables, property)
           # Both sides are always worked out, so an error can't hide behind a short circuit.
           right <- expr_value(node$right, variables, property)
           switch(node$op,
                  and = left && right, or = left || right,
                  `==` = identical(left, right), `!=` = !identical(left, right),
                  `<` = left < right, `<=` = left <= right, `>` = left > right, `>=` = left >= right,
                  `+` = left + right, `-` = left - right, `*` = left * right,
                  `/` = {
                    if (right == 0) stop_sim("An expression divides by zero.")
                    left / right
                  })
         })
}

eval_guard <- function(text, variables, property = function(element, name) NULL) {
  if (is.na(text) || !nzchar(trimws(text))) return(TRUE)
  isTRUE(expr_value(parse_guard(text), variables, property))
}

# Assignments run in order, each seeing the ones before it.
apply_effect <- function(text, variables, property = function(element, name) NULL) {
  if (is.na(text) || !nzchar(trimws(text))) return(variables)
  for (assign in parse_effect(text)) variables[[assign$name]] <- expr_value(assign$value, variables, property)
  variables
}

stop_sim <- function(message) {
  stop(structure(class = c("modelbench_sim_error", "error", "condition"), list(message = message, call = NULL)))
}
