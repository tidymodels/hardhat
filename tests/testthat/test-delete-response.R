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
})

test_that("works if delete.response() also removes dataClasses for y (#306)", {
  # mimics `stats::delete.response()` in R >= 4.7
  local_mocked_bindings(
    delete.response = function(termobj) {
      ans <- stats::delete.response(termobj)
      resp <- attr(termobj, "response")
      attr(ans, "dataClasses") <- attr(termobj, "dataClasses")[-resp]
      ans
    }
  )

  framed <- model_frame(Sepal.Width ~ Species + Petal.Width, iris)

  expect_equal(
    attr(delete_response(framed$terms), "dataClasses"),
    c(Species = "factor", Petal.Width = "numeric")
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
