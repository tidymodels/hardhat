context("test-mold-xy")

test_that("unknown mold() inputs throw an error", {
  expect_error(
    mold("hi"),
    "`x` is not a recognized type.\nOnly data.frame, matrix, recipe, and formula objects are allowed.\nA character was specified."
  )
})

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

test_that("cannot pass anything in the dots", {
  expect_error(
    mold(
      iris[, "Sepal.Length", drop = FALSE],
      iris$Species,
      z = "in the dots"
    ),
    "`...` must not contain any input. 1 elements were found"
  )
})
