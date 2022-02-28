context("test-constructor")

test_that("print - hardhat_model", {
  x <- new_model()

  expect_known_output(
    print(x),
    test_path("out/print-hardhat_model-1.txt")
  )

  x2 <- new_model(class = "custom_class")

  expect_known_output(
    print(x2),
    test_path("out/print-hardhat_model-2.txt")
  )

  x3 <- new_model(x = 4, y = "hi", class = "custom_class")

  expect_known_output(
    print(x3),
    test_path("out/print-hardhat_model-3.txt")
  )
})

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

test_that("`new_scalar()` must have elements", {
  expect_error(new_scalar(list()), "must be a list of length 1 or greater")
})

test_that("`new_scalar()` must have unique names", {
  expect_error(new_scalar(list(x = 1, x = 2)), "must have unique names")
})

test_that("`new_scalar()` must have no extra attributes", {
  x <- list(x = 1)
  attr(x, "extra") <- 1
  expect_error(new_scalar(x), "must have no attributes")
})
