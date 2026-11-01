parts_of <- function(text) {
  parsed <- parse_sentence(text)
  expect_identical(paste(parsed$text, collapse = ""), stringr::str_squish(text))
  stats::setNames(parsed$text, parsed$part)
}

test_that("a full sentence splits into its parts", {
  parts <- parts_of("When the battery state of charge falls below the return reserve, the power system shall report the remaining flight time to the flight controller within 200 ms.")
  expect_identical(parts[["condition"]], "When the battery state of charge falls below the return reserve")
  expect_identical(parts[["actor"]], "the power system")
  expect_identical(parts[["shall"]], "shall")
  expect_identical(parts[["response"]], "report the remaining flight time to the flight controller")
  expect_identical(parts[["constraint"]], "within 200 ms")
})

test_that("a placeholder sits in the constraint", {
  parts <- parts_of("The UAV shall have a maximum take-off mass of no more than [value] kg.")
  expect_identical(parts[["constraint"]], "no more than [value] kg")
  expect_identical(parts[["response"]], "have a maximum take-off mass of")
})

test_that("categorical constraints and missing parts hold up", {
  expect_identical(parts_of("The system shall operate only in indoor locations.")[["constraint"]], "only in indoor locations")
  expect_identical(names(parts_of("The system shall stay on.")), c("actor", "plain", "shall", "plain", "response", "plain"))
  expect_identical(parse_sentence("It works.")$part, "plain")
  expect_identical(nrow(parse_sentence("")), 0L)
})

test_that("the EARS builder assembles each pattern and writes blanks as placeholders", {
  expect_identical(build_ears("ubiquitous", "power system", "report status", bound = "within", value = "5", unit = "s"),
                   "The power system shall report status within 5 s.")
  expect_identical(build_ears("event", "power system", "report status", "the bus drops"),
                   "When the bus drops, the power system shall report status.")
  expect_identical(build_ears("state", "power system", "report status", "armed"),
                   "While armed, the power system shall report status.")
  expect_identical(build_ears("unwanted", "power system", "report status", "the bus drops"),
                   "If the bus drops, then the power system shall report status.")
  expect_identical(build_ears("optional", "power system", "report status", "a logger is fitted"),
                   "Where a logger is fitted, the power system shall report status.")
  blank <- build_ears("event", "power system", "report status", "", value = "", unit = "ms")
  expect_identical(blank, "When [trigger], the power system shall report status within [value] ms.")
  expect_false(check_requirement(blank)$passes_gate)
  expect_error(build_ears("nope", "a", "b"), "Unknown EARS pattern")
})

test_that("an EARS sentence passes the gate with the right actor", {
  gate <- check_requirement(build_ears("unwanted", "power system", "report status", "the bus drops", value = "5", unit = "s"))
  expect_identical(gate$extracted_actor, "the power system")
  expect_true(gate$passes_gate)
})
