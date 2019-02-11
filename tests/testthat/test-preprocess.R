context("test-preprocess")

test_that("simple prepare works", {
  x <- prepare(Species ~ Sepal.Length, iris)

  expect_equal(
    colnames(preprocess(x$preprocessor, iris)),
    "Sepal.Length"
  )
})
