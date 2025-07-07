test_that("quantile_pred error types", {
  expect_snapshot(
    error = TRUE,
    quantile_pred(1:10, 1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    quantile_pred(matrix(1:20, 5), -1:4 / 5)
  )
  expect_snapshot(
    error = TRUE,
    quantile_pred(matrix(1:20, 5), 1:5 / 6)
  )
  expect_snapshot(
    error = TRUE,
    quantile_pred(matrix(1:20, 5), 4:1 / 5)
  )
})

test_that("quantile levels are checked", {
  expect_snapshot(error = TRUE, {
    quantile_pred(matrix(1:20, 5), quantile_levels = NULL)
  })
  expect_snapshot(error = TRUE, {
    quantile_pred(matrix(1:20, 5), quantile_levels = c(0.7, 0.7, 0.7))
  })
  expect_snapshot(error = TRUE, {
    quantile_pred(
      matrix(1:20, 5),
      quantile_levels = c(rep(0.7, 2), rep(0.8, 3))
    )
  })
  expect_snapshot(error = TRUE, {
    quantile_pred(matrix(1:20, 5), quantile_levels = c(0.8, 0.7))
  })
})

test_that("quantile_pred outputs", {
  v <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  expect_s3_class(v, "quantile_pred")
  expect_identical(attr(v, "quantile_levels"), 1:4 / 5)
  expect_identical(
    vctrs::vec_data(v),
    lapply(vctrs::vec_chop(matrix(1:20, 5)), drop)
  )
})

test_that("extract_quantile_levels", {
  v <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  expect_identical(extract_quantile_levels(v), 1:4 / 5)

  expect_snapshot(
    error = TRUE,
    extract_quantile_levels(1:10)
  )
})

test_that("median for quantile_pred", {
  v <- quantile_pred(matrix(1:25, 5), 3:7 / 10)
  expect_identical(median(v), as.double(11:15)) # has explicit median, but dbl

  v_above_med <- quantile_pred(matrix(1:10, 2), 11:15 / 20)
  expect_equal(median(v_above_med), rep(NA, 2))

  v_below_med <- quantile_pred(matrix(1:10, 2), 5:9 / 20)
  expect_equal(median(v_above_med), rep(NA, 2))

  v4 <- quantile_pred(matrix(1:10, ncol = 1), 0.4)
  expect_equal(median(v4), rep(NA, 10))

  v5 <- quantile_pred(matrix(1:10, ncol = 1), 0.5)
  expect_equal(median(v5), as.double(1:10))
})

test_that("quantile_pred formatting", {
  # multiple quantiles
  v <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  expect_snapshot(v)
  expect_snapshot(quantile_pred(matrix(1:18, 9), c(1 / 3, 2 / 3)))
  expect_snapshot(
    quantile_pred(matrix(seq(0.01, 1 - 0.01, length.out = 6), 3), c(.2, .8))
  )
  expect_snapshot(tibble(qntls = v))
  m <- matrix(1:20, 5)
  m[2, 3] <- NA
  m[4, 2] <- NA
  expect_snapshot(quantile_pred(m, 1:4 / 5))

  # single quantile
  m <- matrix(1:5)
  one_quantile <- quantile_pred(m, 5 / 9)
  expect_snapshot(one_quantile)
  expect_snapshot(tibble(qntls = one_quantile))
  m[2] <- NA
  expect_snapshot(quantile_pred(m, 5 / 9))

  set.seed(393)
  v <- quantile_pred(matrix(exp(rnorm(20)), ncol = 4), 1:4 / 5)
  expect_snapshot(format(v))
  expect_snapshot(format(v, digits = 5))
})

test_that("as_tibble() for quantile_pred", {
  v <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  tbl <- as_tibble(v)
  expect_s3_class(tbl, c("tbl_df", "tbl", "data.frame"))
  expect_named(tbl, c(".pred_quantile", ".quantile_levels", ".row"))
  expect_true(nrow(tbl) == 20)
})

test_that("as.matrix() for quantile_pred", {
  x <- matrix(1:20, 5)
  v <- quantile_pred(x, 1:4 / 5)
  m <- as.matrix(v)
  expect_true(is.matrix(m))
  expect_identical(m, x)
})



test_that("unary math works on quantiles", {
  dstn <- quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  dstn2 <- quantile_pred(
    log(matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE)),
    1:4 / 5
  )
  expect_identical(log(dstn), dstn2)
})

test_that("arithmetic works on quantiles", {
  dstn <- quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  dstn2 <- quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE) + 1,
    1:4 / 5
  )
  expect_identical(dstn + 1, dstn2)
  expect_identical(1 + dstn, dstn2)

  dstn2 <- quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE) / 4,
    1:4 / 5
  )
  expect_identical(dstn / 4, dstn2)
  expect_identical((1 / 4) * dstn, dstn2)

  expect_snapshot(error = TRUE, sum(dstn))
})

test_that("vec_ptype works", {
  v1 <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  v2 <- quantile_pred(matrix(1:15, 5), 2:4 / 5)
  expect_identical(vec_ptype2(v1, v1), vec_ptype(v1))
  expect_identical(vec_ptype2(v1, v2), vec_ptype(v1))
  expect_identical(vec_ptype2(v2, v1), vec_ptype(v1))

  ugly_levels <- quantile_pred(matrix(1:20, 5), 1:4 / 5 + .1)
  expect_snapshot(error = TRUE, vec_ptype2(v1, ugly_levels))
})

test_that("vec_cast self-self works", {
  qp <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
  qp2 <- quantile_pred(matrix(rnorm(7), nrow = 1), 2:8 / 10)
  expect_identical(vec_cast(qp, qp), qp)
  expect_identical(vec_cast(qp2, qp2), qp2)

  qp_dat <- as.matrix(qp)
  qp_big <- matrix(NA, nrow(qp_dat), length(2:8))
  qp_big[, c(1, 3, 5, 7)] <- qp_dat
  expect_identical(vec_cast(qp, qp2), quantile_pred(qp_big, 2:8 / 10))
})
