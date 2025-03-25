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
    quantile_pred(matrix(1:20, 5), quantile_levels = c(rep(0.7, 2), rep(0.8, 3)))    
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
    vctrs::vec_data(v) %>% lapply(unname),
    list(quantile_values = matrix(1:20, 5))
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

test_that("quantile_pred formatting", {
  # multiple quantiles
  v <- quantile_pred(matrix(1:20, 5), 1:4 / 5)
  expect_snapshot(v)
  expect_snapshot(quantile_pred(matrix(1:18, 9), c(1/3, 2/3)))
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
  one_quantile <- quantile_pred(m, 5/9)
  expect_snapshot(one_quantile)
  expect_snapshot(tibble(qntls = one_quantile))
  m[2] <- NA
  expect_snapshot(quantile_pred(m, 5/9))

  set.seed(393)
  v <- quantile_pred(matrix(exp(rnorm(20)), ncol = 4), 1:4 / 5)
  expect_snapshot(format(v))
  expect_snapshot(format(v, digits = 5))
  expect_snapshot(data.frame(v = v))
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

test_that("Various ways to introduce NAs in quantile_pred work", {
  dbl_mat <- matrix(c(1:3, c(1, NA, NA), rep(NA, 3)), 3, 3, byrow = TRUE)
  int_mat <- dbl_mat
  storage.mode(int_mat) <- "integer"
  levels <- 1:3 / 4
  dbl_v <- quantile_pred(dbl_mat, levels)
  int_v <- quantile_pred(int_mat, levels)
  for (v in list(dbl_v, int_v)) {
    sentinel <- v[3]
    expect_identical(vec_init(v, 5), rep(sentinel, 5))
    expect_identical(vec_c(v[1:2], NA), v)
    expect_identical(vec_c(NA, v[1:2]), v[c(3, 1, 2)])
    expect_identical(
      merge(tibble(date = as.Date("2020-01-01") + 0:5),
            tibble(date = as.Date("2020-01-01") + 1:3,
                   pred = v),
            by = "date", all = TRUE),
      data.frame(date = as.Date("2020-01-01") + 0:5,
                 pred = v[c(3, 1:3, 3, 3)])
    )
    expect_identical(vec_detect_missing(v), c(FALSE, FALSE, TRUE))
    expect_identical(vec_detect_complete(v), c(TRUE, FALSE, FALSE))
  }
})

test_that("quantile_pred == logic outputs NAs when expected", {
  single_pred <- function(values, levels) quantile_pred(t(as.matrix(values)), levels)
  expect_identical(single_pred(1, 0.5) == single_pred(NA, 0.5), NA)
  expect_identical(single_pred(NA, 0.5) == single_pred(NA, 0.5), NA)
  expect_identical(single_pred(c(1, NA), 1:2/3) == single_pred(c(4, NA), 1:2/3), FALSE)
  expect_identical(single_pred(c(1, NA), 1:2/3) == single_pred(c(4, 5), 1:2/3), FALSE)
})

test_that("Inequalities don't work on quantile_preds, but equality & sorting does:", {
  v <- quantile_pred(matrix(c(6, 1, 2, 3, 5, 6), 2, 3, byrow = TRUE), 1:3/4)
  expect_error(v < v, class = "hardhat_error_comparing_quantile_preds")
  expect_error(v <= v, class = "hardhat_error_comparing_quantile_preds")
  expect_error(v > v, class = "hardhat_error_comparing_quantile_preds")
  expect_error(v >= v, class = "hardhat_error_comparing_quantile_preds")
  expect_identical(v == v, c(TRUE, TRUE))
  expect_identical(v != v, c(FALSE, FALSE))
  expect_identical(sort(v), v[c(2, 1)])
})

test_that("quantile_pred typeof compatibility works", {
  dbl_mat <- matrix(c(1:3, c(4, NA, NA), rep(NA, 3)), 3, 3, byrow = TRUE)
  int_mat <- dbl_mat
  storage.mode(int_mat) <- "integer"
  levels <- 1:3 / 4
  dbl_v <- quantile_pred(dbl_mat, levels)
  int_v <- quantile_pred(int_mat, levels)
  # ptype
  expect_identical(vec_ptype(dbl_v), dbl_v[0])
  expect_identical(vec_ptype(int_v), int_v[0])
  # ptype2
  expect_identical(vec_ptype2(dbl_v, int_v), dbl_v[0])
  expect_identical(vec_ptype2(int_v, dbl_v), dbl_v[0])
  # cast
  expect_identical(vec_cast(int_v, dbl_v), dbl_v)
  expect_identical(vec_cast(dbl_v, int_v), int_v)
  dbl_v2 <- dbl_v
  field(dbl_v2, "quantile_values") <- field(dbl_v2, "quantile_values") + 0.5
  expect_error(vec_cast(dbl_v2, int_v), class = "vctrs_error_cast_lossy")
})

test_that("quantile_pred level (in)compatibility works", {
  levels1 <- seq(0, 0.2, by = 0.05)
  levels2 <- c(0, 0.05, 0.1, 0.15, 0.2)
  expect_false(all(levels1 == levels2))
  v1 <- quantile_pred(t(as.matrix(1:5)), levels1)
  v2 <- quantile_pred(t(as.matrix(1:5)), levels2)
  expect_error(vec_ptype2(v1, v2), class = "vctrs_error_incompatible_type")
  expect_snapshot(vec_ptype2(v1, v2), error = TRUE, cnd_class = TRUE)
  expect_error(vec_cast(v1, v2), class = "vctrs_error_incompatible_type")
  expect_error(vec_cast(v2, v1), class = "vctrs_error_incompatible_type")
})
