context("test-validation")

test_that("validate_outcomes_is_univariate()", {

  expect_silent(validate_outcomes_is_univariate(data.frame(x = 1)))

  expect_silent(validate_outcomes_is_univariate(matrix()))

  expect_silent(validate_outcomes_is_univariate(1))

  expect_error(
    validate_outcomes_is_univariate(iris),
    "The outcome must be univariate, but 5 columns were found."
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

test_that("validate_outcomes_are_factors()", {
  expect_silent(
    validate_outcomes_are_factors(data.frame(x = factor(c("A", "B"))))
  )

  date <- as.POSIXct(as.POSIXlt(as.Date("2019-01-01")))
  x <- data.frame(x = date, y = "hi", stringsAsFactors = FALSE)

  expect_error(
    validate_outcomes_are_factors(x),
    "'x': 'POSIXct', 'POSIXt'\n'y': 'character'"
  )
})

test_that("validate_outcomes_are_binary()", {

  expect_silent(
    validate_outcomes_are_binary(data.frame(x = factor(c("A", "B"))))
  )

  expect_error(
    validate_outcomes_are_binary(iris),
    "'Sepal.Length': 0\n'Sepal.Width': 0\n'Petal.Length': 0\n'Petal.Width': 0\n'Species': 3"
  )

})

test_that("validate_predictors_are_numeric()", {

  expect_silent(
    validate_predictors_are_numeric(mtcars)
  )

  expect_error(
    validate_predictors_are_numeric(iris),
    "'Species': 'factor'"
  )

  date <- as.POSIXct(as.POSIXlt(as.Date("2019-01-01")))
  x <- data.frame(x = date, y = "hi")

  expect_error(
    validate_predictors_are_numeric(x),
    "'x': 'POSIXct', 'POSIXt'\n'y': 'factor'"
  )

})

test_that("validate_prediction_size()", {

  expect_silent(
    validate_prediction_size(mtcars, mtcars)
  )

  expect_error(
    validate_prediction_size(mtcars[1:5,], mtcars),
    "The number of rows in `new_data` \\(32\\) must match the number of rows in `.pred` \\(5\\)."
  )

})
