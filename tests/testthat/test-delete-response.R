test_that("identical to delete.response() if no dataClasses", {
  trms <- terms(y ~ x)

  expect_equal(
    delete_response(trms),
    delete.response(trms)
  )
})

test_that("doesn't return dataClasses for y", {
  framed <- model_frame(Sepal.Width ~ Species, iris)

  expect_equal(
    attr(delete_response(framed$terms), "dataClasses"),
    c(Species = "factor")
  )

  # expected base R behavior
  expect_equal(
    attr(delete.response(framed$terms), "dataClasses"),
    c(Sepal.Width = "numeric", Species = "factor")
  )
})

test_that("equal results if no response, but dataClasses exist", {
  framed <- model_frame(~Species, iris)

  expect_equal(
    delete_response(framed$terms),
    delete.response(framed$terms)
  )
})

test_that("errors out if not passed a terms object", {
  expect_snapshot(error = TRUE, {
    delete_response(1)
  })
})
