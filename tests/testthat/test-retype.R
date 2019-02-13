context("test-retype")

test_that("can retype to a matrix", {
  expect_is(retype(mtcars, "matrix"), "matrix")
})

test_that("can retype to a data.frame", {
  expect_is(retype(mtcars, "data.frame"), "data.frame")
})

test_that("can retype to a tibble", {
  expect_is(retype(mtcars, "tibble"), "tbl_df")
})

test_that("`type` is checked", {

  expect_error(
    retype(mtcars, "x"),
    "`type` must be a single character"
  )

  expect_error(
    retype(mtcars, c("tibble", "data.frame")),
    "`type` must be a single character"
  )

})
