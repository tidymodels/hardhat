context("test-forge-xy")

test_that("simple forge works", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  xx <- forge(iris, x$blueprint)

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

  xx <- forge(iris, x$blueprint, outcomes = TRUE)

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
    colnames(x$blueprint$ptypes$outcomes),
    ".outcome"
  )

  iris2 <- iris
  iris2$.outcome <- iris2$Species
  iris2$Species <- NULL

  xx <- forge(iris2, x$blueprint, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    tibble::tibble(.outcome = iris2$.outcome)
  )

  # standard message
  expect_error(
    forge(iris, x$blueprint, outcomes = TRUE),
    "The following required columns"
  )

  # but also more detail
  expect_error(
    forge(iris, x$blueprint, outcomes = TRUE),
    "`new_data` must include a column with the automatically generated name, '.outcome'"
  )

})

test_that("new_data can be a matrix", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- forge(iris_mat, x$blueprint),
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
    forge("hi", x$blueprint),
    "The class of `new_data`, 'character'"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_error(
    forge(iris[,1, drop = FALSE], x$blueprint),
    "Sepal.Width"
  )

  expect_error(
    forge(iris[,3, drop = FALSE], x$blueprint),
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
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )

})

test_that("novel predictor levels can be ignored", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  blueprint <- default_xy_blueprint(allow_novel_levels = TRUE)

  x <- mold(dat[, "f", drop = FALSE], dat$y, blueprint = blueprint)

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor("e", c("a", "b", "c", "d", "e"))
  )
})

test_that("novel predictor levels without any data are silently removed", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  # The 'e' level exists, but there is no data for it!
  new <- new[1:4,]

  x <- mold(dat[, "f", drop = FALSE], dat$y)

  expect_silent(
    xx <- forge(new, x$blueprint)
  )

  expect_equal(
    colnames(xx$predictors),
    colnames(x$predictors)
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
    xx <- forge(new, x$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )

})

test_that("original predictor and outcome classes are recorded", {

  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_equal(
    get_data_classes(x$blueprint$ptypes$predictors),
    list(Sepal.Length = "numeric", Sepal.Width = "numeric")
  )

  expect_equal(
    get_data_classes(x$blueprint$ptypes$outcomes),
    list(.outcome = "factor")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(iris[, "Species", drop = FALSE], iris$Sepal.Length)

  # Silent recovery
  expect_error(
    x_iris2 <- forge(iris2, x$blueprint),
    NA
  )

  expect_is(
    x_iris2$predictors$Species,
    "factor"
  )

  xx <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  iris3 <- iris2
  iris3$.outcome <- iris2$Species
  iris3$Species <- NULL

  expect_error(
    xx_iris3 <- forge(iris3, xx$blueprint, outcomes = TRUE),
    NA
  )

  expect_is(
    xx_iris3$outcomes$.outcome,
    "factor"
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(iris2, x$blueprint),
    NA
  )

})

test_that("intercept is not included as a predictor", {

  x <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    blueprint = default_xy_blueprint(intercept = TRUE)
  )

  expect_false(
    "(Intercept)" %in% colnames(x$blueprint$ptypes$predictors)
  )

  expect_error(
    xx <- forge(iris, x$blueprint),
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
    blueprint = default_xy_blueprint(intercept = TRUE)
  )

  expect_false(
    "(Intercept)" %in% colnames(xx$blueprint$ptypes$predictors)
  )

})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  x <- mold(iris, y = NULL)
  expect_equal(forge(iris, x$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  x <- mold(iris, y = NULL)

  forged <- forge(iris, x$blueprint, outcomes = TRUE)
  outcomes <- forged$outcomes

  expect_equal(nrow(outcomes), 150)
  expect_equal(ncol(outcomes), 0)
})
