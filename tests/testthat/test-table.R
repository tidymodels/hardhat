# ------------------------------------------------------------------------------
# weighted_table()

test_that("works with 1 factor", {
  x <- factor(c("x", "y", "x"))
  w <- c(1, 3, 2)

  expect_identical(
    weighted_table(x, weights = w),
    array(c(3, 3), dimnames = list(c("x", "y")))
  )
})

test_that("works with 2 factors", {
  x <- factor(c("x", "y", "x", "x"))
  y <- factor(c("a", "b", "a", "b"))
  w <- c(1, 2, 3, 4)

  expect_identical(
    weighted_table(x, y, weights = w),
    array(
      c(4, 0, 4, 2),
      dim = c(2, 2),
      dimnames = list(c("x", "y"), c("a", "b"))
    )
  )
})

test_that("works with 3+ factors", {
  x <- factor(c("x", "y", "x", "x"))
  y <- factor(c("a", "b", "a", "b"))
  z <- factor(c("c", "d", "c", "e"), levels = c("e", "d", "c"))
  w <- c(1, 2, 3, 4)

  expect_identical(
    weighted_table(x, y, z, weights = w),
    array(
      c(0, 0, 4, 0, 0, 0, 0, 2, 4, 0, 0, 0),
      dim = c(2, 2, 3),
      dimnames = list(c("x", "y"), c("a", "b"), c("e", "d", "c"))
    )
  )
})

test_that("dimension titles come from `...` names", {
  x <- factor("x")
  y <- factor("y")

  result <- weighted_table(foo = x, bar = y, weights = 1)

  expect_identical(
    dimnames(result),
    list(foo = "x", bar = "y")
  )
})

test_that("empty factor levels get a default of 0", {
  x <- factor(c("x", "x"), levels = c("x", "y"))
  y <- factor(c("a", "c"), levels = c("a", "b", "c"))
  w <- 1:2

  expect_identical(
    weighted_table(x, y, weights = w),
    array(
      c(1, 0, 0, 0, 2, 0),
      dim = c(2, 3),
      dimnames = list(c("x", "y"), c("a", "b", "c"))
    )
  )
})

test_that("explicit `NA` factor level is always included", {
  x <- factor(c("x", "x"), levels = c("x", NA), exclude = NULL)
  y <- factor(c("a", "b"))
  w <- 1:2

  expect_identical(
    weighted_table(x, y, weights = w),
    array(
      c(1, 0, 2, 0),
      dim = c(2, 2),
      dimnames = list(c("x", NA), c("a", "b"))
    )
  )
})

test_that("implicit `NA` factor level is never included", {
  x <- factor(c("x", NA))
  y <- factor(c("a", "b"))
  w <- 1:2

  expect_identical(
    weighted_table(x, y, weights = w),
    array(
      c(1, 0),
      dim = c(1, 2),
      dimnames = list("x", c("a", "b"))
    )
  )
})

test_that("`na_remove` can handle missing `weights`", {
  x <- factor(c("x", "y", "y"))
  y <- factor(c("a", "b", "b"))
  w <- c(1, NA, 3)

  expect_identical(
    weighted_table(x, y, weights = w),
    array(
      c(1, 0, 0, NA),
      dim = c(2, 2),
      dimnames = list(c("x", "y"), c("a", "b"))
    )
  )

  expect_identical(
    weighted_table(x, y, weights = w, na_remove = TRUE),
    array(
      c(1, 0, 0, 3),
      dim = c(2, 2),
      dimnames = list(c("x", "y"), c("a", "b"))
    )
  )
})

test_that("`na_remove` with only `NA`s in a cell results in `0`", {
  x <- factor("x")
  y <- factor("y")
  w <- NA_real_

  expect_identical(
    weighted_table(x, y, weights = w, na_remove = TRUE),
    array(
      0,
      dim = c(1, 1),
      dimnames = list("x", "y")
    )
  )
})

test_that("`na_remove` is validated", {
  x <- factor("x")
  y <- factor("y")
  w <- 1

  expect_snapshot(error = TRUE, {
    weighted_table(x, y, weights = w, na_remove = c(TRUE, FALSE))
  })
  expect_snapshot(error = TRUE, {
    weighted_table(x, y, weights = w, na_remove = 1)
  })
})

test_that("requires at least one `...`", {
  w <- 1:3
  expect_snapshot(error = TRUE, weighted_table(weights = w))
})

test_that("requires all `...` to be factors", {
  w <- 1:3
  expect_snapshot(error = TRUE, weighted_table(1, weights = w))
})

test_that("requires all `...` to be the same size", {
  x <- factor(c("x", "y"))
  y <- factor(c("x", "y", "z"))
  w <- 1:3

  expect_snapshot(error = TRUE, weighted_table(x, y, weights = w))
})

test_that("requires all `weights` to be the same size as `...` elements", {
  x <- factor(c("x", "y", "z"))
  y <- factor(c("x", "y", "z"))
  w <- 1:4

  expect_snapshot(error = TRUE, weighted_table(x, y, weights = w))
})

test_that("requires `weights` to be castable to double", {
  x <- factor("x")

  expect_identical(
    weighted_table(x, weights = 1L),
    array(1, dimnames = list("x"))
  )

  expect_snapshot(error = TRUE, weighted_table(x, weights = "a"))
})

test_that("you can create a weighted table directly from importance weights", {
  x <- factor(c("x", "y", "x"))
  w <- importance_weights(c(1.2, 2.5, 3))

  expect_identical(
    weighted_table(x, weights = w),
    array(c(4.2, 2.5), dimnames = list(c("x", "y")))
  )
})

test_that("you can create a weighted table directly from frequency weights (#193)", {
  x <- factor(c("x", "y", "x"))
  w <- frequency_weights(c(1L, 2L, 3L))

  expect_identical(
    weighted_table(x, weights = w),
    array(c(4, 2), dimnames = list(c("x", "y")))
  )
})
