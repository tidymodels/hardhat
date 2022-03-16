test_that("novel levels can be ignored", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  ptype <- vec_ptype(dat)

  expect_warning(
    x <- scream(new, ptype, allow_novel_levels = TRUE),
    NA
  )

  expect_equal(levels(x$f), letters[1:5])
})

test_that("novel levels in a new character vector can be ignored", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = letters[1:5],
    stringsAsFactors = FALSE
  )

  ptype <- vec_ptype(dat)

  expect_warning(
    x <- scream(new, ptype, allow_novel_levels = TRUE),
    NA
  )

  expect_equal(levels(x$f), new$f)
})

test_that("ignoring novel levels still passes through incompatible classes", {
  dat <- data.frame(f = factor(letters[1:4]))
  new <- data.frame(f = 1:5)

  ptype <- vec_ptype(dat)

  expect_error(
    scream(new, ptype, allow_novel_levels = TRUE),
    class = "vctrs_error_incompatible_type"
  )
})
