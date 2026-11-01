# Response schemas as plain data, so one definition gives the ellmer type sent to the
# model, the JSON shown in "See the prompt", and the check on what comes back.

s_string <- function() list(type = "string")
s_enum <- function(values) list(type = "enum", values = values)
s_array <- function(items) list(type = "array", items = items)
s_object <- function(...) list(type = "object", fields = list(...))

schema_to_ellmer <- function(spec) {
  switch(spec$type,
         string = ellmer::type_string(),
         enum = ellmer::type_enum(values = spec$values),
         array = ellmer::type_array(items = schema_to_ellmer(spec$items)),
         object = do.call(ellmer::type_object, lapply(spec$fields, schema_to_ellmer)))
}

schema_to_json <- function(spec) {
  plain <- switch(spec$type,
                  string = list(type = "string"),
                  enum = list(enum = as.list(spec$values)),
                  array = list(type = "array", items = jsonlite::fromJSON(schema_to_json(spec$items), simplifyVector = FALSE)),
                  object = list(type = "object", properties = lapply(spec$fields, function(field) {
                    jsonlite::fromJSON(schema_to_json(field), simplifyVector = FALSE)
                  })))
  canonical_json(plain)
}

# Every problem found in a response, as text. Empty means it fits.
schema_problems <- function(value, spec, path = "response") {
  bad <- function(what) sprintf("%s: %s", path, what)
  switch(spec$type,
         string = if (!is.character(value) || length(value) != 1L) bad("expected text") else character(0),
         enum = if (length(value) != 1L || !value %in% spec$values) bad("not an allowed value") else character(0),
         array = {
           if (!is.list(value) && length(value) > 0L) return(bad("expected a list"))
           unlist(lapply(seq_along(value), function(i) schema_problems(value[[i]], spec$items, sprintf("%s[%d]", path, i))))
         },
         object = {
           if (!is.list(value)) return(bad("expected an object"))
           unlist(lapply(names(spec$fields), function(name) {
             if (is.null(value[[name]])) return(bad(paste("missing", name)))
             schema_problems(value[[name]], spec$fields[[name]], paste0(path, ".", name))
           }))
         })
}

PLAIN_TEXT_RULE <- "Write plain text in every field: no Markdown, LaTeX or other markup."
