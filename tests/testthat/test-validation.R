context("test-validation")

test_that("validate_outcome_is_univariate()", {

  expect_silent(validate_outcome_is_univariate(data.frame(x = 1)))

  expect_error(
    validate_outcome_is_univariate(matrix()),
    "The outcome must be a data.frame, not a matrix."
  )

  expect_error(
    validate_outcome_is_univariate(iris),
    "There must only be 1 outcome, not 5."
  )

})
