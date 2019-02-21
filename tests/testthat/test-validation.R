context("test-validation")

test_that("validate_outcomes_is_univariate()", {

  expect_silent(validate_outcomes_is_univariate(data.frame(x = 1)))

  expect_silent(validate_outcomes_is_univariate(matrix()))

  expect_silent(validate_outcomes_is_univariate(1))

  expect_error(
    validate_outcomes_is_univariate(iris),
    "There must only be 1 outcome, not 5."
  )

})
