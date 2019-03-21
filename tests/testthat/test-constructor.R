context("test-constructor")

test_that("can create new empty models", {

  x <- new_model()

  expect_is(x$blueprint, "default_xy_blueprint")
  expect_is(x, "hardhat_model")
  expect_is(x, "hardhat_scalar")
})

test_that("can create new models", {

  x <- new_model(class = "custom")

  expect_is(x, "custom")
  expect_is(x$blueprint, "default_xy_blueprint")
})

test_that("can have custom elements", {

  x <- new_model(
    y = 1,
    blueprint = default_xy_blueprint(),
    class = "custom_class"
  )

  expect_equal(x$y, 1)
})

test_that("must use a valid blueprint", {

  expect_error(
    new_model(blueprint = default_xy_blueprint(), class = "custom"),
    NA
  )

  expect_error(
    new_model(blueprint = 1, class = "custom"),
    "not a numeric"
  )

})

