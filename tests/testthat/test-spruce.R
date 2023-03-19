test_that("spruce - numeric", {
  spruced <- spruce_numeric(1:5)

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred")

  expect_error(spruce_numeric("hi"))
  expect_error(spruce_numeric(matrix(1)))
})

test_that("spruce - class", {
  spruced <- spruce_class(factor(letters[1:5]))

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred_class")

  expect_error(spruce_class(1))
  expect_error(spruce_class("hi"))
})

test_that("spruce - prob", {
  pred_levels <- letters[1:5]
  prob_matrix <- matrix(.5, ncol = 5, nrow = 2)

  spruced <- spruce_prob(pred_levels, prob_matrix)

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), paste0(".pred_", pred_levels))

  expect_error(spruce_prob(1, prob_matrix))
  expect_error(spruce_prob(pred_levels, 1))
  expect_error(spruce_prob("a", matrix("a")))

  expect_error(
    spruce_prob(c("a", "b"), matrix(1, ncol = 3)),
    "2"
  )

  expect_error(
    spruce_prob(c("a", "b"), matrix(1, ncol = 3)),
    "3"
  )

  expect_error(
    spruce_prob(c("a"), matrix(1, ncol = 2)),
    "1"
  )

  expect_error(
    spruce_prob(c("a"), matrix(1, ncol = 2)),
    "2"
  )
})


test_that("spruce - numeric - multi", {
  spruced <- spruce_numeric_multi(tibble(a = 1:5, b = 2:6))

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), c(".pred_1",".pred_2"))

  expect_error(spruce_numeric_multi(letters[1:5]))
  expect_error(spruce_numeric_multi(tibble(a = letters[1:5])))
  expect_error(spruce_numeric_multi(matrix(rnorm(4),nrow = 2, ncol = 2)))
})

test_that("spruce - class - multi", {
  spruced <- spruce_class_multi(
    tibble(a = factor(letters[1:5]), b = factor(letters[2:6]))
  )

  expect_s3_class(spruced, "tbl_df")
  expect_equal(colnames(spruced), c(".pred_class_1", ".pred_class_2"))

  expect_error(spruce_class_multi(tibble(a = 1:5)))
  expect_error(spruce_class_multi("hi"))
})

test_that("spruce - prob - multi", {
  # pred_levels_df <- tibble(day = letters[1:5],
  #                          month = letters[2:13])
  pred_levels_df <- tibble(day = letters[1:5],
                           month = letters[2:6])
  prob_matrix_df <- tibble(day = tibble(matrix(seq(.1, .5, .1), nrow = 2, ncol = 5, byrow = T ), .name_repair = "minimal"),
                           month = tibble(matrix(seq(.5, 1, .05), nrow = 2, ncol = 12, byrow = T), .name_repair = "minimal"))
  spruced <- spruce_prob_multi(pred_levels_df, prob_matrix_df)

  expect_s3_class(spruced, "tbl_df")
  # expect_equal(colnames(spruced), c(".pred_class_1", ".pred_class_2"))
  #
  # expect_error(spruce_class_multi(tibble(a = 1:5)))
  # expect_error(spruce_class_multi("hi"))
})


