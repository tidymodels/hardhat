test_that("an outcome can also be a predictor and is only returned once", {
  x <- mold(Sepal.Length ~ Sepal.Length, iris)

  expect_equal(
    colnames(shrink(iris, x$blueprint$ptypes$predictors)),
    "Sepal.Length"
  )
})

test_that("`data` must be data-like", {
  ptype <- data.frame(x = integer())

  expect_snapshot(error = TRUE, {
    shrink(1, ptype)
  })
})
