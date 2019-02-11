context("test-extract-outcome")

u <- "univariate"
m <- "multivariate"
c <- "classification"
r <- "regression"

test_that("single factor outcome", {

  y <- factor(letters[1:5])
  y_df <- data.frame(y = y)

  expect_equal(
    extract_outcome(y, c, u),
    y
  )

  expect_error(extract_outcome(y, r, u), "`y` should be")
  expect_error(extract_outcome(y, r, m), "`y` should be")
  expect_error(extract_outcome(y, c, m), "`y` should be")

  expect_equal(
    extract_outcome(y_df, c, u),
    y
  )

  expect_error(extract_outcome(y_df, r, u), "`y` should be")
  expect_error(extract_outcome(y_df, r, m), "`y` should be")
  expect_error(extract_outcome(y_df, c, m), "`y` should be")

})

test_that("multiple factor outcome", {

  y <- factor(letters[1:5])
  z <- factor(letters[6:10])
  y_z_df <- data.frame(y = y, z = z)

  expect_equal(
    extract_outcome(y_z_df, c, m),
    y_z_df
  )

  expect_error(extract_outcome(y_z_df, r, u), "`y` should be")
  expect_error(extract_outcome(y_z_df, r, m), "`y` should be")
  expect_error(extract_outcome(y_z_df, c, u), "`y` should be")

})

test_that("single numeric outcome", {

  y <- 1:5
  y_df <- data.frame(y = y)
  y_mat <- matrix(y)

  expect_equal(extract_outcome(y, r, u), y)

  expect_error(extract_outcome(y, r, m), "`y` should be")
  expect_error(extract_outcome(y, c, u), "`y` should be")
  expect_error(extract_outcome(y, c, m), "`y` should be")

  expect_equal(extract_outcome(y_df, r, u), y)

  expect_error(extract_outcome(y_df, r, m), "`y` should be")
  expect_error(extract_outcome(y_df, c, u), "`y` should be")
  expect_error(extract_outcome(y_df, c, m), "`y` should be")

  expect_equal(extract_outcome(y_mat, r, u), y)

  expect_error(extract_outcome(y_mat, r, m), "`y` should be")
  expect_error(extract_outcome(y_mat, c, u), "`y` should be")
  expect_error(extract_outcome(y_mat, c, m), "`y` should be")

})

test_that("multiple numeric outcome", {

  y_z_df <- data.frame(
    y = 1:5,
    z = 6:10
  )

  y_z_mat <- as.matrix(y_z_df)

  expect_equal(extract_outcome(y_z_df, r, m), y_z_mat)

  expect_error(extract_outcome(y_z_df, r, u), "`y` should be")
  expect_error(extract_outcome(y_z_df, c, u), "`y` should be")
  expect_error(extract_outcome(y_z_df, c, m), "`y` should be")

  expect_equal(extract_outcome(y_z_mat, r, m), y_z_mat)

  expect_error(extract_outcome(y_z_mat, r, u), "`y` should be")
  expect_error(extract_outcome(y_z_mat, c, u), "`y` should be")
  expect_error(extract_outcome(y_z_mat, c, m), "`y` should be")
})
