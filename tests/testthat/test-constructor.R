context("test-constructor")

test_that("can create new models", {

  x <- new_base_model(class = "custom")

  expect_is(x, "custom")
  expect_is(x$preprocessor, "default_preprocessor")
})

test_that("can have custom fields", {

  x <- new_base_model(
    new_default_preprocessor(),
    y = 1,
    class = "custom_class"
  )

  expect_equal(x$y, 1)
})

test_that("must use a valid preprocessor", {

  expect_error(
    new_base_model(preprocessor = new_default_preprocessor(), class = "custom"),
    NA
  )

  expect_error(
    new_base_model(preprocessor = 1, class = "custom"),
    "not a numeric"
  )

})
