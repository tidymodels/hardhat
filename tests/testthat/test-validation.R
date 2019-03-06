context("test-validation")

test_that("validate_is_univariate()", {

  expect_silent(validate_is_univariate(data.frame(x = 1)))

  expect_silent(validate_is_univariate(matrix()))

  expect_silent(validate_is_univariate(1))

  expect_error(
    validate_is_univariate(iris),
    "`data` must be univariate, but 5 columns were found."
  )

})
