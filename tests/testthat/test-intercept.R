context("test-intercept")

test_that("can add an intercept column", {
  x <- add_intercept_column(mtcars)

  expect_equal(colnames(x)[1], "(Intercept)")
  expect_is(x[, 1], "integer")

  xx <- add_intercept_column(as.matrix(mtcars))

  expect_is(xx, "matrix")
  expect_equal(colnames(xx)[1], "(Intercept)")
})

test_that("existing intercepts are skipped with a warning", {
  x <- add_intercept_column(mtcars)

  expect_warning(
    xx <- add_intercept_column(x),
    "`data` already has a column named"
  )

  expect_equal(
    xx,
    x
  )
})

test_that("can change the intercept column name", {
  x <- add_intercept_column(mtcars, name = "intercept")

  expect_equal(colnames(x)[1], "intercept")
})

test_that("name can only be a single character", {
  expect_error(
    add_intercept_column(mtcars, name = c("x", "y")),
    "name should have size 1, not 2."
  )

  expect_error(
    add_intercept_column(mtcars, name = 1),
    "name should be a character, not a numeric."
  )
})
