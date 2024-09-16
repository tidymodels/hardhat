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

test_that("sparse tibble can be turned to tibble", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  res <- recompose(hotel_data, composition = "tibble")

  expect_identical(
    res,
    hotel_data
  )

  expect_true(is_sparse_tibble(res))
})

test_that("sparse tibble can be turned to data.frame", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  res <- recompose(hotel_data, composition = "data.frame")

  expect_identical(
    res,
    as.data.frame(hotel_data)
  )

  expect_true(is_sparse_tibble(res))
})

test_that("sparse tibble can be turned to matrix", {
  skip_if_not_installed("modeldata")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)
  hotel_data <- hotel_data[1:10, 1:10]

  res <- recompose(hotel_data, composition = "matrix")

  expect_identical(
    res,
    as.matrix(hotel_data)
  )
})

test_that("sparse tibble can be turned to dgCMatrix", {
  skip_if_not_installed("modeldata")

  hotel_mat <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_mat)

  res <- recompose(hotel_data, composition = "dgCMatrix")
  rownames(res) <- NULL

  expect_identical(
    res,
    hotel_mat
  )
})
