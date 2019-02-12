# ------------------------------------------------------------------------------
context("test-prepare-formulas")

test_that("can prepare simple formulas", {

  x <- prepare(Species ~ Sepal.Length, iris)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "factor")
  expect_is(x$preprocessor, "terms_preprocessor")
})

test_that("formula intercepts can be added", {

  x <- prepare(
    Species ~ Sepal.Length,
    iris,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
  expect_equal(attr(x$preprocessor$engine, "intercept"), 1)

  # Don't want intercept in original predictors
  expect_false("(Intercept)" %in% attr(x$preprocessor$engine, "predictors"))
})

test_that("output type can be matrix", {

  x <- prepare(
    Species ~ Sepal.Length,
    iris,
    type = "matrix"
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$predictors, "matrix")
})

test_that("can prepare formulas with special terms", {

  x <- prepare(Species ~ Sepal.Length:Sepal.Width + I(Sepal.Length^2), iris)
  y <- prepare(Species ~ poly(Sepal.Length, degree = 2), iris)

  expect_equal(
    colnames(x$predictors),
    c("I(Sepal.Length^2)", "Sepal.Length:Sepal.Width")
  )

  expect_equal(
    x$preprocessor$predictors,
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("formulas with non-existant columns are caught", {

  expect_error(
    prepare(Species ~ y, iris),
    "object 'y' not found"
  )

})

test_that("global environment variables cannot be used", {

  expect_error(
    {
      y <- 1
      prepare(Species ~ y, iris)
    },
    "object 'y' not found"
  )

})

test_that("cannot manually remove intercept in the formula itself", {

  expect_error(
    prepare(Species ~ y + 0, iris),
    "`formula` must not contain"
  )

  expect_error(
    prepare(Species ~ y - 1, iris),
    "`formula` must not contain"
  )

})

# ------------------------------------------------------------------------------
context("test-prepare-xy")

test_that("can use x-y prepare interface", {

  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "factor")
  expect_is(x$preprocessor, "default_preprocessor")

})

test_that("xy intercepts can be added", {

  x <- prepare(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("output type can be matrix", {

  x <- prepare(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    type = "matrix"
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$predictors, "matrix")
})

# ------------------------------------------------------------------------------
context("test-prepare-recipes")

library(recipes)
prepare <- hardhat::prepare

test_that("can prepare recipes", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "data.frame")
  expect_is(x$outcomes[[1]], "factor")
  expect_is(x$preprocessor, "recipes_preprocessor")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x$preprocessor))
})

test_that("can prepare recipes with intercepts", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("can prepare recipes with matrix output", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    type = "matrix"
  )

  expect_is(x$predictors, "matrix")
})
