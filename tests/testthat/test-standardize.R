test_that("standardize - factor", {
  std <- standardize(factor(letters[1:5]))
  expect_s3_class(std, "tbl_df")
  expect_equal(colnames(std), ".outcome")
})

test_that("standardize - numeric", {
  std <- standardize(1:5)
  expect_s3_class(std, "tbl_df")
  expect_equal(colnames(std), ".outcome")

  std2 <- standardize(as.double(1:5))
  expect_s3_class(std2, "tbl_df")
  expect_equal(colnames(std2), ".outcome")
})

test_that("standardize - matrix", {
  mat_bad <- matrix(1:10, ncol = 2)
  mat_bad2 <- matrix("a", dimnames = list(NULL, "c1"))

  mat_good <- mat_bad
  colnames(mat_good) <- c("a", "b")

  expect_snapshot(error = TRUE, {
    standardize(mat_bad)
  })

  expect_snapshot(error = TRUE, {
    standardize(mat_bad2)
  })

  std <- standardize(mat_good)
  expect_s3_class(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))
})

test_that("standardize - array", {
  bad <- array(1:10, c(5, 2))

  expect_snapshot(error = TRUE, {
    standardize(bad)
  })

  bad2 <- array("a", c(1, 1), dimnames = list(NULL, "c1"))

  expect_snapshot(error = TRUE, {
    standardize(bad2)
  })

  good <- bad
  colnames(good) <- c("a", "b")

  std <- standardize(good)
  expect_s3_class(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))

  good2 <- array(1:5)

  std2 <- standardize(good2)
  expect_s3_class(std2, "tbl_df")
  expect_equal(colnames(std2), ".outcome")
})

test_that("standardize - data.frame", {
  bad <- data.frame(1:5, 6:10)
  colnames(bad) <- NULL

  expect_snapshot(error = TRUE, {
    standardize(bad)
  })

  bad2 <- data.frame(x = "a", stringsAsFactors = FALSE)

  expect_snapshot(error = TRUE, standardize(bad2))

  bad3 <- data.frame(x = "a", y = "b", stringsAsFactors = FALSE)

  expect_snapshot(error = TRUE, standardize(bad3))

  good <- bad
  colnames(good) <- c("a", "b")

  std <- standardize(good)
  expect_s3_class(std, "tbl_df")
  expect_equal(colnames(std), c("a", "b"))

  good2 <- data.frame(x = factor(letters[1:5]), y = factor(letters[6:10]))

  std2 <- standardize(good2)
  expect_s3_class(std2, "tbl_df")
  expect_equal(colnames(std2), c("x", "y"))
})

test_that("standardize - unknown", {
  expect_snapshot(error = TRUE, standardize("hi"))
  expect_snapshot(error = TRUE, standardize(Sys.time()))
})
