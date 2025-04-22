test_that("spruce - numeric", {
  spruced <- spruce_numeric(1:5)

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred")

  expect_snapshot(error = TRUE, spruce_numeric("hi"))
  expect_snapshot(error = TRUE, spruce_numeric(matrix(1)))
})

test_that("spruce - class", {
  spruced <- spruce_class(factor(letters[1:5]))

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred_class")

  expect_snapshot(error = TRUE, spruce_class(1))
  expect_snapshot(error = TRUE, spruce_class("hi"))
})

test_that("spruce - prob", {
  pred_levels <- letters[1:5]
  prob_matrix <- matrix(.5, ncol = 5, nrow = 2)

  spruced <- spruce_prob(pred_levels, prob_matrix)

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), paste0(".pred_", pred_levels))

  expect_snapshot(error = TRUE, spruce_prob(1, prob_matrix))
  expect_snapshot(error = TRUE, spruce_prob(pred_levels, 1))
  expect_snapshot(error = TRUE, spruce_prob("a", matrix("a")))

  expect_snapshot(
    error = TRUE,
    spruce_prob(c("a", "b"), matrix(1, ncol = 3))
  )

  expect_snapshot(
    error = TRUE,
    spruce_prob(c("a"), matrix(1, ncol = 2))
  )
})

test_that("`spruce_numeric_multiple()` generates the correct output", {
  expect_identical(
    spruce_numeric_multiple(1, y = 2),
    tibble(.pred_1 = 1, .pred_y = 2)
  )
})

test_that("`spruce_class_multiple()` generates the correct output", {
  x <- factor(c("a", "a"))
  y <- factor(c("a", "b"))

  expect_identical(
    spruce_class_multiple(x, y = y),
    tibble(.pred_class_1 = x, .pred_class_y = y)
  )
})

test_that("`spruce_prob_multiple()` generates the correct output", {
  x <- spruce_prob(
    c("a", "b"),
    matrix(c(.3, .7, .4, .6), nrow = 2, byrow = TRUE)
  )
  y <- spruce_prob(
    c("a", "b", "c"),
    matrix(c(.2, .7, .1, .2, .6, .2), nrow = 2, byrow = TRUE)
  )

  expect_identical(
    spruce_prob_multiple(x, y = y),
    tibble(.pred_1 = x, .pred_y = y)
  )
})

test_that("spruce multiple helpers check input type", {
  expect_snapshot(error = TRUE, {
    spruce_numeric_multiple(1, "x")
  })
  expect_snapshot(error = TRUE, {
    spruce_class_multiple(1)
  })
  expect_snapshot(error = TRUE, {
    spruce_prob_multiple(1)
  })
})

test_that("spruce multiple helpers check input sizes (and disallow recycling)", {
  expect_snapshot(error = TRUE, {
    spruce_numeric_multiple(1, 1:2)
  })
  expect_snapshot(error = TRUE, {
    spruce_class_multiple(factor("x"), factor(c("a", "b")))
  })
  expect_snapshot(error = TRUE, {
    spruce_prob_multiple(tibble(x = 1), tibble(x = 1:2))
  })
})

test_that("spruce multiple helpers work with no inputs", {
  expect_identical(spruce_numeric_multiple(), tibble())
  expect_identical(spruce_class_multiple(), tibble())
  expect_identical(spruce_prob_multiple(), tibble())
})
