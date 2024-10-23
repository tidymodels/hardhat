test_that("`model_matrix()` strips all attributes from the `model.matrix()` results", {
  framed <- model_frame(Sepal.Width ~ Species + 0, iris)
  matrix <- model_matrix(framed$terms, framed$data)

  # Mock what `model_matrix()` does by stripping all attributes
  f <- Sepal.Width ~ Species + 0
  expect <- model.matrix(f, model.frame(f, iris))
  expect <- expect[, 1, drop = TRUE]
  attributes(expect) <- NULL

  # `tibble:::matrixToDataFrame()` would propagate any attributes besides
  # column names to each individual column. `model.matrix()` would have
  # attached "assign" and "contrasts" attributes here
  expect_identical(matrix$Speciessetosa, expect)
})

test_that("`contr_one_hot()` input checks", {
  expect_snapshot(contr_one_hot(n = 1, sparse = TRUE))
  expect_snapshot(contr_one_hot(n = 1, contrasts = FALSE))
  expect_snapshot(error = TRUE, {
    contr_one_hot(n = 1:2)
  })
  expect_snapshot(error = TRUE, {
    contr_one_hot(n = list(1:2))
  })
})
