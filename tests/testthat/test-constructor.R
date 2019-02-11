context("test-constructor")

test_that("can create new models", {

  x <- new_base_model("classification", "univariate", class = "custom")

  expect_is(x, "custom")
  expect_is(x$preprocessor, "default_preprocessor")
  expect_equal(x$mode, "classification")
  expect_equal(x$variateness, "univariate")
})

test_that("can have custom fields", {

  x <- new_base_model(
    "regression",
    "univariate",
    default_preprocessor(),
    y = 1,
    class = "custom_class"
  )

  expect_equal(x$y, 1)
})

test_that("cannot create bad mode/variateness arguments", {

  expect_error(
    new_base_model(c("x", "y"), variateness = "univariate"),
    "mode should be length 1"
  )

  expect_error(
    new_base_model(1, variateness = "univariate"),
    "mode should be a character"
  )

  expect_error(
    new_base_model("regression", variateness = 1),
    "variateness should be a character"
  )

  expect_error(
    new_base_model("regression", variateness = c("x", "y")),
    "variateness should be length 1"
  )

})

test_that("must use a valid preprocessor", {

  expect_error(
    new_base_model("regression", "univariate", default_preprocessor(), class = "custom"),
    NA
  )

  expect_error(
    new_base_model("regression", "univariate", 1, class = "custom"),
    "not a numeric"
  )

})
