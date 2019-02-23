context("test-mold-recipes")

library(recipes)

test_that("can mold recipes", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "data.frame")
  expect_is(x$outcomes[[1]], "factor")
  expect_is(x$engine, "default_recipe_engine")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x$engine))
})

test_that("can mold recipes with intercepts", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("`data` is validated", {

  expect_error(
    mold(recipe(Species ~ Sepal.Length, data = iris), 1),
    "`data` must be a data.frame or a matrix"
  )

})
