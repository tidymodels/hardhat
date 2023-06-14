test_that("data frames are coerced to tibble by default", {
  expect_identical(
    recompose(data.frame(x = 1)),
    tibble(x = 1)
  )
})

test_that("can coerce to base data frame", {
  expect_identical(
    recompose(tibble(x = 1), composition = "data.frame"),
    data.frame(x = 1)
  )
})

test_that("can coerce to matrix", {
  expect_identical(
    recompose(data.frame(x = 1), composition = "matrix"),
    matrix(1, dimnames = list(NULL, "x"))
  )
})

test_that("can coerce to sparse matrix", {
  skip_if_not_installed("Matrix")

  expect_identical(
    recompose(data.frame(x = 1:2), composition = "dgCMatrix"),
    Matrix::Matrix(1:2, dimnames = list(NULL, "x"), sparse = TRUE)
  )
})

test_that("non-numeric columns are allowed with tibble/data.frame output", {
  df <- data.frame(x = 1, y = "a", z = "b")
  expect_identical(recompose(df), tibble::as_tibble(df))
  expect_identical(recompose(df, composition = "data.frame"), df)
})

test_that("columns must be numeric when coercing to matrix", {
  df <- data.frame(x = 1, y = "a", z = "b")

  expect_snapshot(error = TRUE, {
    recompose(df, composition = "matrix")
  })
})

test_that("columns must be numeric when coercing to sparse matrix", {
  skip_if_not_installed("Matrix")

  df <- data.frame(x = 1, y = "a", z = "b")

  expect_snapshot(error = TRUE, {
    recompose(df, composition = "dgCMatrix")
  })
})

test_that("checks for data frame input", {
  expect_snapshot(error = TRUE, {
    recompose(1)
  })
})

test_that("dots must be empty", {
  expect_snapshot(error = TRUE, {
    recompose(data.frame(), 1)
  })
})

test_that("validates `composition`", {
  expect_snapshot(error = TRUE, {
    recompose(data.frame(), composition = "foo")
  })
  expect_snapshot(error = TRUE, {
    recompose(data.frame(), composition = 1)
  })
})
