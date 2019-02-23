context("test-forge-recipe")

library(recipes)

test_that("simple forge works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(iris, x$engine)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("asking for the outcome works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(iris, x$engine, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    forge(iris2, x$engine, outcomes = TRUE),
    "`new_data` is missing the following"
  )

})

test_that("outcomes steps get processed", {

  x <- mold(
    recipe(Sepal.Width ~ Sepal.Length, data = iris) %>%
      step_log(Sepal.Width),
    iris
  )

  processed <- forge(iris, x$engine, outcomes = TRUE)

  expect_equal(
    processed$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )

})

test_that("missing predictor columns fail appropriately", {

  x <- mold(
    recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_error(
    forge(iris[,1, drop = FALSE], x$engine),
    "Sepal.Width"
  )

  expect_error(
    forge(iris[,3, drop = FALSE], x$engine),
    "'Sepal.Length', 'Sepal.Width'"
  )

})

test_that("novel predictor levels are caught", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  x <- mold(recipe(y ~ f, dat), dat)

  expect_warning(
    xx <- forge(new, x$engine),
    "The following factor levels"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

})

test_that("novel outcome levels are caught", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  x <- mold(recipe(f ~ y, dat), dat)

  expect_warning(
    xx <- forge(new, x$engine, outcomes = TRUE),
    "The following factor levels"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

})

test_that("original predictor and outcome classes / names are recorded", {

  x <- mold(
    recipe(Sepal.Length ~ Species, data = iris) %>%
      step_dummy(Species),
    iris
  )

  expect_equal(
    x$engine$info$predictors$names,
    "Species"
  )

  expect_equal(
    x$engine$info$outcomes$names,
    "Sepal.Length"
  )

  expect_equal(
    x$engine$info$predictors$classes,
    list(Species = "factor")
  )

  expect_equal(
    x$engine$info$outcomes$classes,
    list(Sepal.Length = "numeric")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  expect_error(
    forge(iris2, x$engine),
    "`Species`: `character` should be `factor`"
  )

  xx <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    forge(iris2, xx$engine),
    NA
  )

  expect_error(
    forge(iris2, xx$engine, outcomes = TRUE),
    "`Species`: `character` should be `factor`"
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    forge(iris2, x$engine),
    NA
  )

  xx <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  expect_error(
    forge(iris2, xx$engine, outcomes = TRUE),
    NA
  )

})
