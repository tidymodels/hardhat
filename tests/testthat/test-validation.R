context("test-validation")

test_that("validate_outcomes_is_univariate()", {

  expect_silent(validate_outcomes_is_univariate(data.frame(x = 1)))

  expect_silent(validate_outcomes_is_univariate(matrix()))

  expect_silent(validate_outcomes_is_univariate(1))

  expect_error(
    validate_outcomes_is_univariate(iris),
    "`data` must be univariate, but 5 columns were found."
  )

})

test_that("validate_no_formula_duplication()", {

  expect_silent(validate_no_formula_duplication(y ~ x))

  expect_error(
    validate_no_formula_duplication(y ~ y),
    "'y'"
  )

  expect_silent(validate_no_formula_duplication(y ~ log(y)))

  expect_error(
    validate_no_formula_duplication(y ~ log(y), original = TRUE),
    "'y'"
  )

  expect_error(
    validate_no_formula_duplication(y + x ~ y + x),
    "'y', 'x'"
  )

  expect_silent(validate_no_formula_duplication(y ~ .))

  expect_error(
    validate_no_formula_duplication(y ~ . + y),
    "'y'"
  )

  # offset() is a weird special case but this is ok
  expect_silent(validate_no_formula_duplication(offset(y) ~ offset(y)))

  expect_error(
    validate_no_formula_duplication(y ~ offset(y), original = TRUE),
    "'y'"
  )

})
