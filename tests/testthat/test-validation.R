test_that("validate_outcomes_are_univariate()", {
  expect_silent(validate_outcomes_are_univariate(data.frame(x = 1)))

  expect_silent(validate_outcomes_are_univariate(matrix()))

  expect_silent(validate_outcomes_are_univariate(1))

  expect_snapshot(
    error = TRUE,
    validate_outcomes_are_univariate(iris)
  )
})

test_that("validate_outcomes_are_numeric()", {
  expect_silent(
    validate_outcomes_are_numeric(mtcars)
  )

  expect_snapshot(
    error = TRUE,
    validate_outcomes_are_numeric(iris)
  )

  date <- as.POSIXct(as.POSIXlt(as.Date("2019-01-01")))
  x <- data.frame(x = date, y = factor("hi"))

  expect_snapshot(
    error = TRUE,
    validate_outcomes_are_numeric(x)
  )
})

test_that("validate_no_formula_duplication()", {
  expect_silent(validate_no_formula_duplication(y ~ x))

  expect_snapshot(
    error = TRUE,
    validate_no_formula_duplication(y ~ y)
  )

  expect_silent(validate_no_formula_duplication(y ~ log(y)))

  expect_snapshot(
    error = TRUE,
    validate_no_formula_duplication(y ~ log(y), original = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    validate_no_formula_duplication(y + x ~ y + x)
  )

  expect_silent(validate_no_formula_duplication(y ~ .))

  expect_snapshot(
    error = TRUE,
    validate_no_formula_duplication(y ~ . + y)
  )

  # offset() is a weird special case but this is ok
  expect_silent(validate_no_formula_duplication(offset(y) ~ offset(y)))

  expect_snapshot(
    error = TRUE,
    validate_no_formula_duplication(y ~ offset(y), original = TRUE)
  )
})

test_that("validate_outcomes_are_factors()", {
  expect_silent(
    validate_outcomes_are_factors(data.frame(x = factor(c("A", "B"))))
  )

  date <- as.POSIXct(as.POSIXlt(as.Date("2019-01-01")))
  x <- data.frame(x = date, y = "hi", stringsAsFactors = FALSE)

  expect_snapshot(
    error = TRUE,
    validate_outcomes_are_factors(x)
  )
})

test_that("validate_outcomes_are_binary()", {
  expect_silent(
    validate_outcomes_are_binary(data.frame(x = factor(c("A", "B"))))
  )

  expect_snapshot(
    error = TRUE,
    validate_outcomes_are_binary(iris)
  )
})

test_that("validate_predictors_are_numeric()", {
  expect_silent(
    validate_predictors_are_numeric(mtcars)
  )

  expect_snapshot(
    error = TRUE,
    validate_predictors_are_numeric(iris)
  )

  date <- as.POSIXct(as.POSIXlt(as.Date("2019-01-01")))
  x <- data.frame(x = date, y = factor("hi"))

  expect_snapshot(
    error = TRUE,
    validate_predictors_are_numeric(x)
  )
})

test_that("validate_prediction_size()", {
  expect_silent(
    validate_prediction_size(mtcars, mtcars)
  )

  expect_snapshot(
    error = TRUE,
    validate_prediction_size(mtcars[1:5, ], mtcars)
  )
})
