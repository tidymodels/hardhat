test_that("can add an intercept column", {
  x <- add_intercept_column(mtcars)

  expect_equal(colnames(x)[1], "(Intercept)")
  expect_type(x[, 1], "integer")

  xx <- add_intercept_column(as.matrix(mtcars))

  expect_matrix(xx)
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
  expect_snapshot(error = TRUE, {
    add_intercept_column(mtcars, name = c("x", "y"))
  })
  expect_snapshot(error = TRUE, {
    add_intercept_column(mtcars, name = 1)
  })
})
