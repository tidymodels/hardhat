test_that("factor_to_indicators works as expected", {

  fact <- factor(c("c", "d", "a", "c", "a"), levels = c("a", "d", "c"))
  exp_matrix <- matrix(
    c(0L, 0L, 1L, 0L, 1L,
      0L, 1L, 0L, 0L ,0L,
      1L, 0L, 0L, 1L, 0L),
    ncol = 3
  )
  colnames(exp_matrix) <- c("a", "d", "c")

  expect_identical(
    factor_to_indicators(fact),
    exp_matrix
  )

  expect_snapshot(error = TRUE, factor_to_indicators(letters))
})

test_that("factor_to_indicators works as expected", {

  fact <- factor(c("c", "d", "a", "c", "a"), levels = c("a", "d", "c"))
  ord_fact <- ordered(fact)

  expect_identical(
    factor_to_indicators(ord_fact),
    factor_to_indicators(fact)
  )
})

test_that("factor_to_indicators errors for missing values", {
  fact <- factor(c("c", "d", "a", "c", NA), levels = c("a", "d", "c"))

  expect_snapshot(error = TRUE, factor_to_indicators(fact))
})
