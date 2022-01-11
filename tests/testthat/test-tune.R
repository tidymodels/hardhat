test_that("tune creates a call", {
  expect_true(is.call(tune()))
  expect_true(is.call(tune("foo")))
})

test_that("tune `id` value", {
  expect_identical(tune(), call("tune"))
  expect_identical(tune(""), call("tune"))
  expect_identical(tune("foo"), call("tune", "foo"))
})

test_that("`id` is validated", {
  expect_error(tune(1), "The `id` should be a single character string.")
  expect_error(tune(c("x", "y")), "The `id` should be a single character string.")
  expect_error(tune(NA_character_), "The `id` can't be missing.")
})
