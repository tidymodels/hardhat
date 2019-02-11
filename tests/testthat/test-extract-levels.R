context("test-extract-levels")

u <- "univariate"
m <- "multivariate"
c <- "classification"
r <- "regression"

y <- as.factor(c(letters[1], letters[1:2]))

test_that("can extract levels from a vector", {
  expect_equal(extract_outcome_levels(y, c, u), letters[1:2])
})

test_that("can extract levels from a 1 column data frame", {
  y_df <- data.frame(y = y)

  expect_equal(extract_outcome_levels(y_df, c, u), letters[1:2])
})

test_that("can extract levels from multivariate outcome", {

  y_z_df <- data.frame(
    y = y,
    z = y
  )

  expect_equal(
    extract_outcome_levels(y_z_df, c, m),
    list(
      y = letters[1:2],
      z = letters[1:2]
    )
  )

})

test_that("returns NULL with regression", {
  expect_equal(
    extract_outcome_levels(1, r, u),
    NULL
  )
})

test_that("mode/variateness validation exists", {
  expect_error(extract_outcome_levels(y, "hi", u), "`mode` must be")
  expect_error(extract_outcome_levels(y, c, "hi"), "`variateness` must be")
})
