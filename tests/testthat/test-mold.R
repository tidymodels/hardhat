# ------------------------------------------------------------------------------
context("test-mold-formulas")

test_that("can mold simple formulas", {

  x <- mold(Species ~ Sepal.Length, iris)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "data.frame")
  expect_equal(colnames(x$outcomes), "Species")
  expect_is(x$preprocessor, "terms_preprocessor")
})

test_that("can mold multivariate formulas", {

  x <- mold(cbind(Sepal.Length, Sepal.Width) ~ Petal.Length, iris)

  expect_is(x$outcomes, "data.frame")
  expect_equal(colnames(x$outcomes), c("Sepal.Length", "Sepal.Width"))
})

test_that("factor predictors with no intercept are fully expanded", {

  x <- mold(Sepal.Length ~ Species, iris, intercept = TRUE)
  xx <- mold(Sepal.Length ~ Species, iris, intercept = FALSE)

  expect_equal(
    colnames(x$predictors),
    c("(Intercept)", "Speciesversicolor", "Speciesvirginica")
  )

  expect_equal(
    colnames(xx$predictors),
    c("Speciessetosa", "Speciesversicolor", "Speciesvirginica")
  )

})

test_that("formula intercepts can be added", {

  x <- mold(
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

  x <- mold(
    Species ~ Sepal.Length,
    iris,
    type = "matrix"
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$predictors, "matrix")
})

test_that("can mold formulas with special terms", {

  x <- mold(Species ~ Sepal.Length:Sepal.Width + I(Sepal.Length^2), iris)
  y <- mold(Species ~ poly(Sepal.Length, degree = 2), iris)

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
    mold(Species ~ y, iris),
    "object 'y' not found"
  )

})

test_that("global environment variables cannot be used", {

  expect_error(
    {
      y <- 1
      mold(Species ~ y, iris)
    },
    "object 'y' not found"
  )

})

test_that("cannot manually remove intercept in the formula itself", {

  expect_error(
    mold(Species ~ y + 0, iris),
    "`formula` must not contain"
  )

  expect_error(
    mold(Species ~ y - 1, iris),
    "`formula` must not contain"
  )

})

# ------------------------------------------------------------------------------
context("test-mold-xy")

test_that("can use x-y mold interface", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), ".outcome")
  expect_is(x$preprocessor, "default_preprocessor")

})

test_that("xy intercepts can be added", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("output type can be matrix", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    type = "matrix"
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$predictors, "matrix")
})

# ------------------------------------------------------------------------------
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
  expect_is(x$preprocessor, "recipes_preprocessor")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x$preprocessor))
})

test_that("can mold recipes with intercepts", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("can mold recipes with matrix output", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    type = "matrix"
  )

  expect_is(x$predictors, "matrix")
})
