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
  xx <- preprocess(x$preprocessor, iris2, outcome = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
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

  expect_equal(
    xx$predictors,
    poly_df
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

  expect_equal(
    xx$predictors,
    tibble::tibble(Sepal.Length = iris$Sepal.Length)
  )

})

test_that("new_data can only be a data frame / matrix", {
  x <- prepare(Species ~ Sepal.Length, iris)

  expect_error(
    preprocess(x$preprocessor, "hi"),
    "new_data should be a"
  )

})

context("test-preprocess-xy")

context("test-preprocess-recipe")
