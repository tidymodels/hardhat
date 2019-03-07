context("test-mold-xy")

test_that("can use x-y mold interface", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), ".outcome")
  expect_is(x$engine, "default_xy_engine")

})

test_that("xy intercepts can be added", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    engine = default_xy_engine(intercept = TRUE)
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})
