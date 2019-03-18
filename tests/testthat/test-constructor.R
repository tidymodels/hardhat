context("test-constructor")

test_that("can create new empty models", {

  x <- new_model()

  expect_is(x$engine, "default_xy_engine")
  expect_is(x, "hardhat_model")
  expect_is(x, "hardhat_scalar")
})

test_that("can create new models", {

  x <- new_model(class = "custom")

  expect_is(x, "custom")
  expect_is(x$engine, "default_xy_engine")
})

test_that("can have custom elements", {

  x <- new_model(
    y = 1,
    engine = default_xy_engine(),
    class = "custom_class"
  )

  expect_equal(x$y, 1)
})

test_that("must use a valid engine", {

  expect_error(
    new_model(engine = default_xy_engine(), class = "custom"),
    NA
  )

  expect_error(
    new_model(engine = 1, class = "custom"),
    "not a numeric"
  )

})

