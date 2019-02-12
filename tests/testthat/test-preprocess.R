# ------------------------------------------------------------------------------
context("test-preprocess-formula")

test_that("simple preprocess works", {
  x <- prepare(Species ~ Sepal.Length, iris)
  xx <- preprocess(x$preprocessor, iris)

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
  x <- prepare(Species ~ Sepal.Length, iris)
  xx <- preprocess(x$preprocessor, iris, outcome = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {
  x <- prepare(Species ~ Sepal.Length, iris)
  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    preprocess(x$preprocessor, iris2, outcome = TRUE),
    "`new_data` is missing the following"
  )
})

test_that("can use special inline functions", {
  x <- prepare(log(Sepal.Length) ~ poly(Sepal.Length, degree = 2), iris)
  xx <- preprocess(x$preprocessor, iris, outcome = TRUE)

  # manually create poly df
  x_poly <- stats::poly(iris$Sepal.Length, degree = 2)
  poly_df <- tibble::tibble(
    `poly(Sepal.Length, degree = 2)1` = x_poly[,1],
    `poly(Sepal.Length, degree = 2)2` = x_poly[,2]
  )
  attr(poly_df$`poly(Sepal.Length, degree = 2)1`, "assign") <- c(1, 1)
  attr(poly_df$`poly(Sepal.Length, degree = 2)2`, "assign") <- c(1, 1)

  # coerce to df for tolerance..tibbles don't have good tolerance
  expect_equal(
    as.data.frame(xx$predictors),
    as.data.frame(poly_df)
  )

  expect_equal(
    xx$outcomes,
    data.frame(`log(Sepal.Length)` = log(iris$Sepal.Length), check.names = FALSE)
  )

})

test_that("new_data can be a matrix", {
  x <- prepare(Species ~ Sepal.Length, iris)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- preprocess(x$preprocessor, iris_mat),
    NA
  )

  sep_len <- iris$Sepal.Length
  attr(sep_len, "assign") <- 1
  pred_tbl <- tibble::tibble(Sepal.Length = sep_len)

  expect_equal(
    xx$predictors,
    pred_tbl
  )

})

test_that("new_data can only be a data frame / matrix", {
  x <- prepare(Species ~ Sepal.Length, iris)

  expect_error(
    preprocess(x$preprocessor, "hi"),
    "new_data should be a"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- prepare(Species ~ Sepal.Length + Sepal.Width, iris)

  expect_error(
    preprocess(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    preprocess(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
  )

})

test_that("matrix type is used in preprocessing as well", {
  x <- prepare(Species ~ Sepal.Length, iris, type = "matrix")
  xx <- preprocess(x$preprocessor, iris)

  expect_is(xx$predictors, "matrix")
})

# ------------------------------------------------------------------------------
context("test-preprocess-xy")

test_that("simple preprocess works", {

  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  xx <- preprocess(x$preprocessor, iris)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("asking for the outcome fails for default preprocessor", {
  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    preprocess(x$preprocessor, iris, outcome = TRUE),
    "`outcome` cannot be specified"
  )
})

test_that("new_data can be a matrix", {

  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- preprocess(x$preprocessor, iris_mat),
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
  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    preprocess(x$preprocessor, "hi"),
    "new_data should be a"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- prepare(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_error(
    preprocess(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    preprocess(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
  )

})

test_that("matrix type is used in preprocessing as well", {
  x <- prepare(iris[, "Sepal.Length", drop = FALSE], iris$Species, type = "matrix")
  xx <- preprocess(x$preprocessor, iris)

  expect_is(xx$predictors, "matrix")
})

# ------------------------------------------------------------------------------
context("test-preprocess-recipe")

library(recipes)
prepare <- hardhat::prepare

test_that("simple preprocess works", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- preprocess(x$preprocessor, iris)

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

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- preprocess(x$preprocessor, iris, outcome = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    preprocess(x$preprocessor, iris2, outcome = TRUE),
    "`new_data` is missing the following"
  )

})

test_that("outcomes steps get processed", {

  x <- prepare(
    recipe(Sepal.Width ~ Sepal.Length, data = iris) %>%
      step_log(Sepal.Width),
    iris
  )

  processed <- preprocess(x$preprocessor, iris, outcome = TRUE)

  expect_equal(
    processed$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )

})

test_that("missing predictor columns fail appropriately", {

  x <- prepare(
    recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_error(
    preprocess(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    preprocess(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
  )

})

test_that("matrix type is used in preprocessing as well", {

  x <- prepare(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    type = "matrix"
  )

  xx <- preprocess(x$preprocessor, iris)

  expect_is(xx$predictors, "matrix")
})
