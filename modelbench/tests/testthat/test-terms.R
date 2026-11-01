terms_for <- function(con, text) {
  gate <- check_requirement(text, model_terms(con))
  gate$terms
}

resolved_to <- function(terms, term) terms$element_id[terms$term == term]

test_that("normalising ignores case, articles and plurals", {
  expect_identical(normalize_term("The Power Systems"), "power system")
  expect_identical(normalize_term("batteries"), "battery")
  expect_identical(normalize_term("GNSS status"), "gnss status")
  expect_identical(normalize_term("a sensor"), "sensor")
})

test_that("term resolution works on the sample", {
  con <- local_sample()
  dictionary <- model_terms(con)
  resolve <- function(text) dictionary$element_id[dictionary$norm == normalize_term(text)]
  expect_identical(resolve("power system"), "C-003")
  expect_identical(resolve("The Power Systems"), "C-003")
  expect_identical(resolve("autopilot"), "C-009")
  expect_identical(resolve("Autopilot Software"), "C-009")
  expect_identical(resolve("ground station"), "C-002")
})

test_that("a draft's actor and nouns resolve, and unknown phrases are questions", {
  con <- local_sample()
  text <- "When the battery state of charge falls below the return reserve, the power system shall report the remaining flight time to the flight controller within 200 ms."
  terms <- terms_for(con, text)
  expect_identical(terms$term[1], "power system")
  expect_identical(terms$role[1], "actor")
  expect_identical(resolved_to(terms, "power system"), "C-003")
  expect_identical(resolved_to(terms, "flight controller"), "C-004")
  expect_identical(resolved_to(terms, "return reserve"), "C-003")
  expect_identical(terms$source[terms$term == "return reserve"], "property")
  unknown <- terms[terms$term == "remaining flight time", ]
  expect_identical(unknown$resolution, "unresolved")
  expect_identical(unknown$role, "object")
})

test_that("an alias resolves inside a longer actor", {
  terms <- terms_for(local_sample(), "During field deployment, the UAV system shall be transportable by one person.")
  expect_identical(terms$element_id[terms$role == "actor"], "C-000")
})

test_that("a quantity is told apart from an object", {
  terms <- terms_for(local_sample(), "The UAV shall provide a flight endurance of at least 35 minutes.")
  expect_identical(terms$role[terms$term == "flight endurance"], "quantity")
})

test_that("an actor outside the model is a soft penalty, not a block", {
  con <- local_sample()
  gate <- check_requirement("When armed, the gimbal shall hold the camera level within 2 s.", model_terms(con))
  expect_true(gate$warnings$actor_not_in_model)
  expect_equal(gate$score, 90)
  expect_true(gate$passes_gate)
  expect_true(is.na(gate$actor_element_id))
  known <- check_requirement("When armed, the power system shall hold the bus at 28 V.", model_terms(con))
  expect_equal(known$score, 100)
  expect_identical(known$actor_element_id, "C-003")
})

test_that("an empty model leaves the check inert", {
  gate <- check_requirement("When armed, the gimbal shall hold the camera level within 2 s.")
  expect_false(gate$warnings$actor_not_in_model)
  expect_equal(gate$score, 100)
})

test_that("'not a model term' resolves the term and removes the penalty", {
  con <- local_sample()
  mark_not_model_term(con, "The Gimbals")
  gate <- check_requirement("When armed, the gimbal shall hold the camera level within 2 s.", model_terms(con))
  expect_identical(gate$terms$resolution[gate$terms$term == "gimbal"], "not_model")
  expect_equal(gate$score, 100)
})

test_that("each role offers the actions that fit it", {
  expect_identical(term_actions("actor"), c("component", "not_model"))
  expect_identical(term_actions("quantity"), c("property", "not_model"))
  expect_identical(term_actions("object"), c("property", "field", "not_model"))
})

test_that("adding a term creates it with its value as a placeholder", {
  con <- local_sample()
  before <- count_rows(con, "v_placeholders")
  add_term_as_property(con, "gimbal mass", "C-007", "kg")
  expect_identical(count_rows(con, "v_placeholders"), before + 1L)
  gate <- check_requirement("The payload camera shall limit the gimbal mass to 1 kg.", model_terms(con))
  expect_identical(gate$terms$element_id[gate$terms$term == "gimbal mass"], "C-007")
  expect_error(add_term_as_property(con, "gimbal mass", "C-007", "kg"), class = "modelbench_refused")

  field <- add_term_as_field(con, "remaining flight time", "IF-003", "float32", "s")
  row <- read_entity(con, "interface_field", field$id)
  expect_identical(row$name, "remaining_flight_time")
  expect_identical(row$range_state, "placeholder")
  expect_identical(count_rows(con, "v_placeholders"), before + 2L)

  component <- add_term_as_component(con, "gimbal", "C-000")
  expect_identical(read_entity(con, "element", component$id)$name, "Gimbal")
})
