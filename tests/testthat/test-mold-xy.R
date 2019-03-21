context("test-mold-xy")

test_that("can use x-y mold interface", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), ".outcome")
  expect_is(x$blueprint, "default_xy_blueprint")

})

test_that("xy intercepts can be added", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = default_xy_blueprint(intercept = TRUE)
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})
