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

  expect_equal(
    factor_to_indicators(factor(rep("a", 3))),
    matrix(1L, nrow = 3, ncol = 1, dimnames = list(NULL, "a"))
  )

  expect_equal(
    factor_to_indicators(factor(rep("a", 1))),
    matrix(1L, nrow = 1, ncol = 1, dimnames = list(NULL, "a"))
  )

  expect_equal(
    factor_to_indicators(factor(character(), levels = c("a", "b", "c"))),
    matrix(1L, nrow = 0, ncol = 3, dimnames = list(NULL, c("a", "b", "c")))
  )

  expect_equal(
    factor_to_indicators(factor()),
    matrix(1L, nrow = 0, ncol = 0, dimnames = list(NULL, NULL))
  )
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
