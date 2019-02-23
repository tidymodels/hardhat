context("test-forge-xy")

test_that("simple forge works", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
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
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE]
  )

  xx <- forge(iris, x$engine, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    tibble::tibble(Species = iris$Species)
  )

})

test_that("asking for the outcome is special cased for vector `y` values", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species
  )

  expect_equal(
    x$engine$info$outcomes$names,
    ".outcome"
  )

  iris2 <- iris
  iris2$.outcome <- iris2$Species
  iris2$Species <- NULL

  xx <- forge(iris2, x$engine, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    tibble::tibble(.outcome = iris2$.outcome)
  )

  # standard message
  expect_error(
    forge(iris, x$engine, outcomes = TRUE),
    "`new_data` is missing the following required columns"
  )

  # but also more detail
  expect_error(
    forge(iris, x$engine, outcomes = TRUE),
    "`new_data` must include a column with the automatically generated name '.outcome'"
  )

})

test_that("new_data can be a matrix", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- forge(iris_mat, x$engine),
    NA
  )

  sep_len <- iris$Sepal.Length
  pred_tbl <- tibble::tibble(Sepal.Length = sep_len)

  expect_equal(
    xx$predictors,
    pred_tbl
  )

})

test_that("new_data can only be a data frame / matrix", {
  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge("hi", x$engine),
    "The class of `new_data`, 'character'"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

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

  x <- mold(dat[, "f", drop = FALSE], dat$y)

  expect_warning(
    xx <- forge(new, x$engine),
    "The following factor levels"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
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

  x <- mold(
    x = dat[, "y", drop = FALSE],
    y = dat[, "f", drop = FALSE]
  )

  expect_warning(
    xx <- forge(new, x$engine, outcomes = TRUE),
    "The following factor levels"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )

})

test_that("original predictor and outcome classes are recorded", {

  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_equal(
    x$engine$info$predictors$classes,
    list(Sepal.Length = "numeric", Sepal.Width = "numeric")
  )

  expect_equal(
    x$engine$info$outcomes$classes,
    list(.outcome = "factor")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(iris[, "Species", drop = FALSE], iris$Sepal.Length)

  expect_error(
    forge(iris2, x$engine),
    "`Species`: `character` should be `factor`"
  )

  xx <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(iris2, xx$engine),
    NA
  )

  iris3 <- iris2
  iris3$.outcome <- iris2$Species
  iris3$Species <- NULL

  expect_error(
    forge(iris3, xx$engine, outcomes = TRUE),
    "`.outcome`: `character` should be `factor`."
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(iris2, x$engine),
    NA
  )

})

test_that("intercept is not included as a predictor", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    intercept = TRUE
  )

  expect_false(
    "(Intercept)" %in% x$engine$info$predictors$names
  )

  expect_error(
    xx <- forge(iris, x$engine),
    NA
  )

  expect_equal(
    colnames(xx$predictors),
    c("(Intercept)", "Sepal.Length")
  )

  # again, with matrices
  xx <- mold(
    as.matrix(iris[, "Sepal.Length", drop = FALSE]),
    iris$Sepal.Width,
    intercept = TRUE
  )

  expect_false(
    "(Intercept)" %in% xx$engine$info$predictors$names
  )

})
