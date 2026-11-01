test_that("every ellmer constructor is blocked", {
  expect_error(ellmer::chat_anthropic(), "LLM_NETWORK_GUARD")
  expect_error(ellmer::chat_openai(), "LLM_NETWORK_GUARD")
  expect_error(ellmer::chat_openai_compatible("http://localhost:1234/v1"), "LLM_NETWORK_GUARD")
  expect_error(ellmer::chat("anthropic"), "LLM_NETWORK_GUARD")
  expect_error(ellmer::models_openai(), "LLM_NETWORK_GUARD")
})

test_that("provider keys are cleared", {
  expect_identical(Sys.getenv("ANTHROPIC_API_KEY"), "")
  expect_identical(Sys.getenv("OPENAI_API_KEY"), "")
})
