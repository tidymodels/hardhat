test_that("snap.numeric clamps values correctly", {
  x <- c(-5, 0, 5, 10, 15)
  expect_equal(snap(x, lower = 0, upper = 10), c(0, 0, 5, 10, 10))

  x <- c(2, 4, 6)
  expect_equal(snap(x, lower = 0, upper = 10), x)

  x <- numeric(0)
  expect_equal(snap(x, lower = 0, upper = 10), numeric(0))
})

test_that("snap.quantile_pred clamps values correctly", {
  x <- quantile_pred(matrix(c(-5, 0, 5, 10, 15), nrow = 1), 1:5 / 6)
  s <- snap(x, lower = 0, upper = 10)
  expect_s3_class(s, "quantile_pred")
  expect_equal(extract_quantile_levels(s), 1:5 / 6)
  expect_equal(
    drop(as.matrix(snap(x, lower = 0, upper = 10))),
    c(0, 0, 5, 10, 10)
  )

  x <- quantile_pred(matrix(c(2, 4, 6), nrow = 1), 1:3 / 4)
  s <- snap(x, lower = 0, upper = 10)
  expect_s3_class(s, "quantile_pred")
  expect_equal(extract_quantile_levels(s), 1:3 / 4)
  expect_equal(
    drop(as.matrix(snap(x, lower = 0, upper = 10))),
    drop(as.matrix(x))
  )

  x <- new_quantile_pred()
  expect_equal(snap(x, lower = 0, upper = 10), x)
})

test_that("impute_quantiles failure modes", {
  x <- double(7)
  expect_snapshot(error = TRUE, impute_quantiles(x))

  probs <- c(0.5)
  x <- quantile_pred(matrix(0, nrow = 1), probs)
  expect_snapshot(error = TRUE, impute_quantiles(x, c(0.1, 0.5, 0.9)))

  probs <- c(0.1, 0.5, 0.9)
  x <- quantile_pred(matrix(qnorm(probs), nrow = 1), probs)
  expect_snapshot(error = TRUE, impute_quantiles(x, probs = c(-1, .2, 2)))
  expect_snapshot(error = TRUE, impute_quantiles(x, lower = "a"))
  expect_snapshot(error = TRUE, impute_quantiles(x, upper = "b"))
  expect_snapshot(error = TRUE, impute_quantiles(x, lower = NULL))
  expect_snapshot(error = TRUE, impute_quantiles(x, lower = 2, upper = -1))
  expect_snapshot(error = TRUE, impute_quantiles(x, middle = "middle"))
})

test_that("impute_quantiles returns existing quantiles when matched", {
  probs <- c(0.1, 0.5, 0.9)
  x <- quantile_pred(matrix(qnorm(probs), nrow = 1), probs)
  out <- impute_quantiles(x, probs)
  expect_equal(out, as.matrix(x))
})

test_that("impute_quantiles interpolates correctly", {
  probs <- c(0.1, 0.9)
  x <- quantile_pred(matrix(qnorm(probs), nrow = 1), probs)
  out <- impute_quantiles(x, c(0.1, 0.5, 0.9), middle = "linear")
  expect_equal(out[1], qnorm(0.1))
  expect_equal(out[3], qnorm(0.9))
  expect_equal(out[2], 0)
})

test_that("impute_quantiles extrapolates correctly", {
  probs <- c(0.2, 0.8)
  x <- quantile_pred(matrix(qnorm(probs), nrow = 1), probs)
  out <- impute_quantiles(x, c(0.01, 0.2, 0.8, 0.99))
  expect_equal(out[2], qnorm(0.2))
  expect_equal(out[3], qnorm(0.8))
  tail1 <- tail_extrapolate(.01, tibble(q = c(.2, .8), v = qnorm(q)))
  tail2 <- tail_extrapolate(.99, tibble(q = c(.8, .2), v = qnorm(q)))
  expect_equal(out[1], tail1)
  expect_equal(out[4], tail2)
})

test_that("impute_quantiles applies lower/upper bounds", {
  probs <- c(0.1, 0.5, 0.9)
  x <- quantile_pred(matrix(qnorm(probs), nrow = 1), probs)
  out <- impute_quantiles(x, c(0.01, 0.5, 0.99), lower = -1, upper = 1)
  expect_true(all(out >= -1 & out <= 1))
})
