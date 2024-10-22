test_that("print - formula", {
  expect_snapshot({
    mold(Species ~ Sepal.Length, iris)$blueprint
    mold(~Sepal.Length, iris)$blueprint
  })
})

test_that("print - default", {
  expect_snapshot({
    mold(iris[, c("Sepal.Length"), drop = FALSE], iris$Species)$blueprint
  })
})

test_that("print - recipe", {
  skip_if_not_installed("recipes")

  expect_snapshot({
    mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)$blueprint
  })
})
