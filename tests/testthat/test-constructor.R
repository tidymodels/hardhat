test_that("print - hardhat_model", {
  expect_snapshot({
    new_model()
    new_model(class = "custom_class")
    new_model(x = 4, y = "hi", class = "custom_class")
  })
})

test_that("can create new empty models", {
  x <- new_model()

  expect_s3_class(x$blueprint, "default_xy_blueprint")
  expect_s3_class(x, "hardhat_model")
  expect_s3_class(x, "hardhat_scalar")
})

test_that("can create new models", {
  x <- new_model(class = "custom")

  expect_s3_class(x, "custom")
  expect_s3_class(x$blueprint, "default_xy_blueprint")
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
  expect_no_error(
    new_model(blueprint = default_xy_blueprint(), class = "custom")
  )

  expect_snapshot(error = TRUE, {
    new_model(blueprint = 1, class = "custom")
  })
})

test_that("`new_scalar()` must have elements", {
  expect_snapshot(error = TRUE, new_scalar(list()))
})

test_that("`new_scalar()` must have unique names", {
  expect_snapshot(error = TRUE, new_scalar(list(x = 1, x = 2)))
})

test_that("`new_scalar()` must have no extra attributes", {
  x <- list(x = 1)
  attr(x, "extra") <- 1
  expect_snapshot(error = TRUE, new_scalar(x))
})
