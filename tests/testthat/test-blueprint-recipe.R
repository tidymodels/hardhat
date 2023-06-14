test_that("`recipe` argument is validated", {
  expect_snapshot(error = TRUE, {
    new_recipe_blueprint(recipe = 1)
  })
})

test_that("`recipe` argument allows `NULL`", {
  x <- new_recipe_blueprint(recipe = NULL)
  expect_null(x$recipe)
})
