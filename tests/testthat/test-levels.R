test_that("can extract levels", {
  x <- data.frame(
    x = factor(letters[1:5]),
    y = factor(letters[6:10])
  )

  expect_equal(
    get_levels(x),
    list(
      x = letters[1:5],
      y = letters[6:10]
    )
  )
})

test_that("non-factors are ignored", {
  x <- data.frame(
    x = factor(letters[1:5]),
    y = 6:10
  )

  expect_equal(
    get_levels(x),
    list(
      x = letters[1:5]
    )
  )
})

test_that("NULL returned when no factors", {
  x <- data.frame(
    x = 1:5,
    y = 6:10
  )

  expect_equal(
    get_levels(x),
    NULL
  )
})

test_that("Only data frames are allowed, others return NULL", {
  x <- matrix(
    1:5
  )

  expect_equal(
    get_levels(x),
    NULL
  )
})

test_that("Multivariate columns are skipped over", {
  x <- data.frame(x = factor(letters[1:5]))
  x$y <- matrix(1:10, ncol = 2, dimnames = list(NULL, c("c1", "c2")))

  expect_equal(
    get_levels(x),
    list(x = letters[1:5])
  )
})

test_that("Can extract levels from an outcome", {
  expect_equal(
    get_outcome_levels(1:5),
    NULL
  )

  expect_snapshot(
    error = TRUE,
    get_outcome_levels("a")
  )

  expect_equal(
    get_outcome_levels(factor("a")),
    list(.outcome = "a")
  )

  expect_equal(
    get_outcome_levels(matrix(1:5, dimnames = list(NULL, "c1"))),
    NULL
  )

  expect_equal(
    get_outcome_levels(data.frame(x = factor(letters[1:5]))),
    list(x = letters[1:5])
  )
})
