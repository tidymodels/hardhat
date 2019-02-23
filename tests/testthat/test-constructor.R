context("test-constructor")

test_that("can create new models", {

  x <- new_base_model(engine = new_default_xy_engine(), class = "custom")

  expect_is(x, "custom")
  expect_is(x$engine, "default_xy_engine")
})

test_that("can have custom fields", {

  x <- new_base_model(
    new_default_xy_engine(),
    y = 1,
    class = "custom_class"
  )

  expect_equal(x$y, 1)
})

test_that("must use a valid engine", {

  expect_error(
    new_base_model(engine = new_default_xy_engine(), class = "custom"),
    NA
  )

  expect_error(
    new_base_model(engine = 1, class = "custom"),
    "not a numeric"
  )

})
