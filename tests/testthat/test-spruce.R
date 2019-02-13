context("test-spruce")

test_that("spruce - response", {
  spruced <- spruce_response(1:5)

  expect_is(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred")

  expect_error(spruce_response("hi"))
  expect_error(spruce_response(matrix(1)))
})

test_that("spruce - class", {

  spruced <- spruce_class(factor(letters[1:5]))

  expect_is(spruced, "tbl_df")
  expect_equal(colnames(spruced), ".pred_class")

  expect_error(spruce_class(1))
  expect_error(spruce_class("hi"))
})

test_that("spruce - prob", {

  .pred_levels <- letters[1:5]
  .prob_matrix <- matrix(.5, ncol = 5, nrow = 2)

  spruced <- spruce_prob(.pred_levels, .prob_matrix)

  expect_is(spruced, "tbl_df")
  expect_equal(colnames(spruced), paste0(".pred_", .pred_levels))

  expect_error(spruce_prob(1, .prob_matrix))
  expect_error(spruce_prob(.pred_levels, 1))
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
