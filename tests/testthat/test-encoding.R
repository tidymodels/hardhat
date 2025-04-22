test_that("generates one-hot indicator matrix", {
  x <- factor(c("a", "b", "a", "a", "c"))

  expect <- matrix(
    0L,
    nrow = 5,
    ncol = 3,
    dimnames = list(NULL, c("a", "b", "c"))
  )
  expect[c(1, 3, 4, 7, 15)] <- 1L

  expect_identical(fct_encode_one_hot(x), expect)
})

test_that("works with factors with just 1 level", {
  x <- factor(rep("a", 3))

  expect_identical(
    fct_encode_one_hot(x),
    matrix(1L, nrow = 3, ncol = 1, dimnames = list(NULL, "a"))
  )
})

test_that("works with levels that aren't in the data", {
  x <- factor(c("a", "c", "a"), levels = c("a", "b", "c", "d"))

  expect <- matrix(
    0L,
    nrow = 3,
    ncol = 4,
    dimnames = list(NULL, c("a", "b", "c", "d"))
  )
  expect[c(1, 3, 8)] <- 1L

  expect_identical(fct_encode_one_hot(x), expect)
})

test_that("works with factors with explicit `NA` level but no `NA` data", {
  expect_identical(
    fct_encode_one_hot(factor("a", levels = c("a", NA), exclude = NULL)),
    matrix(
      data = c(1L, 0L),
      nrow = 1,
      ncol = 2,
      dimnames = list(NULL, c("a", NA))
    )
  )
})

test_that("works with empty factors", {
  expect_identical(
    fct_encode_one_hot(factor()),
    matrix(data = integer(), nrow = 0, ncol = 0, dimnames = list(NULL, NULL))
  )
})

test_that("works with empty factors with levels", {
  expect_identical(
    fct_encode_one_hot(factor(levels = c("a", "b"))),
    matrix(
      data = integer(),
      nrow = 0,
      ncol = 2,
      dimnames = list(NULL, c("a", "b"))
    )
  )
})

test_that("propagates names onto the row names", {
  x <- set_names(factor(c("a", "b", "a")), c("x", "y", "z"))
  expect_identical(rownames(fct_encode_one_hot(x)), c("x", "y", "z"))
})

test_that("works with ordered factors", {
  x <- factor(
    c("a", "b", "a", "a", "c"),
    levels = c("c", "b", "a"),
    ordered = TRUE
  )

  expect <- matrix(
    0L,
    nrow = 5,
    ncol = 3,
    dimnames = list(NULL, c("c", "b", "a"))
  )
  expect[c(5, 7, 11, 13, 14)] <- 1L

  expect_identical(fct_encode_one_hot(x), expect)
})

test_that("errors on missing values", {
  x <- factor(c("a", NA))

  expect_snapshot(error = TRUE, {
    fct_encode_one_hot(x)
  })
})

test_that("errors on non-factors", {
  expect_snapshot(error = TRUE, {
    fct_encode_one_hot(1)
  })
})
