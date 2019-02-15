context("test-shrink")

test_that("an outcome can also be a predictor and is only returned once", {
  x <- mold(Sepal.Length ~ Sepal.Length, iris)

  expect_equal(
    colnames(shrink(x$preprocessor, iris, outcome = TRUE)),
    "Sepal.Length"
  )
})
