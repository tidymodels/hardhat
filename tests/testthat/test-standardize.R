context("test-standardize")

test_that("standardize - factor", {

  std <- standardize(factor(letters[1:5]))
  expect_is(std, "tbl_df")
  expect_equal(colnames(std), ".outcome")

})

test_that("standardize - numeric", {

  std <- standardize(1:5)
  expect_is(std, "tbl_df")
  expect_equal(colnames(std), ".outcome")

  std2 <- standardize(as.double(1:5))
  expect_is(std2, "tbl_df")
  expect_equal(colnames(std2), ".outcome")

})

test_that("standardize - matrix", {

  mat_bad <- matrix(1:10, ncol = 2)
  mat_bad2 <- matrix("a", dimnames = list(NULL, "c1"))

  mat_good <- mat_bad
  colnames(mat_good) <- c("a", "b")

  expect_error(standardize(mat_bad), "All columns of `y`")

  expect_error(standardize(mat_bad2), "`y` should have numeric elements")

  std <- standardize(mat_good)
  expect_is(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))

})

test_that("standardize - array", {

  bad <- array(1:10, c(5,2))

  expect_error(standardize(bad), "All columns of `y`")

  bad2 <- array("a", c(1,1), dimnames = list(NULL, "c1"))

  expect_error(standardize(bad2), "`y` should have numeric elements")

  good <- bad
  colnames(good) <- c("a", "b")

  std <- standardize(good)
  expect_is(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))

  good2 <- array(1:5)

  std2 <- standardize(good2)
  expect_is(std2, "tbl_df")
  expect_equal(colnames(std2), ".outcome")

})

test_that("standardize - data.frame", {

  bad <- data.frame(1:5, 6:10)
  colnames(bad) <- NULL

  expect_error(standardize(bad), "All columns of `y`")

  bad2 <- data.frame(x = "a", stringsAsFactors = FALSE)

  expect_error(standardize(bad2), "These columns have unknown types: 'x'.")

  good <- bad
  colnames(good) <- c("a", "b")

  std <- standardize(good)
  expect_is(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))

  good2 <- data.frame(x = factor(letters[1:5]), y = factor(letters[6:10]))

  std2 <- standardize(good2)
  expect_is(std2, "tbl_df")
  expect_equal(colnames(std2), c("x", "y"))

})

test_that("standardize - unknown", {

  expect_error(standardize("hi"), "`y` is of unknown type 'character'")
  expect_error(standardize(Sys.time()), "`y` is of unknown type 'POSIXct'")

})
