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
  expect_snapshot(error = TRUE, tune(1))
  expect_snapshot(error = TRUE, tune(c("x", "y")))
  expect_snapshot(error = TRUE, tune(NA_character_))
})
