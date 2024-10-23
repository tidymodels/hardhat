test_that("`levels` argument is validated", {
  expect_snapshot(error = TRUE, {
    new_default_formula_blueprint(levels = 1)
  })
  expect_snapshot(error = TRUE, {
    new_default_formula_blueprint(levels = list(1))
  })
  expect_snapshot(error = TRUE, {
    new_default_formula_blueprint(levels = list("a" = 1))
  })
})
