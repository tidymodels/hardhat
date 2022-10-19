test_that("get_indicators works as expected", {

  fact <- factor(c("c", "d", "a", "c", "a"), levels = c("a", "d", "c"))
  exp_matrix <- matrix(
    c(0L, 0L, 1L, 0L, 1L,
      0L, 1L, 0L, 0L ,0L,
      1L, 0L, 0L, 1L, 0L),
    ncol = 3
  )
  colnames(exp_matrix) <- c("a", "d", "c")

  expect_identical(
    get_indicators(fact),
    exp_matrix
  )

  expect_snapshot(error = TRUE, get_indicators(letters))
})

test_that("get_indicators errors for missing values", {
  fact <- factor(c("c", "d", "a", "c", NA), levels = c("a", "d", "c"))

  expect_snapshot(error = TRUE, get_indicators(fact))
})
