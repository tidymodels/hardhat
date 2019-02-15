context("test-print")

test_that("print - formula", {
  x <- mold(Species ~ Sepal.Length, iris)

  expect_known_output(
    print(x$preprocessor),
    test_path("out/print-preprocessor-formula-1.txt")
  )

  xx <- mold(~ Sepal.Length, iris)

  expect_known_output(
    print(x$preprocessor),
    test_path("out/print-preprocessor-formula-2.txt")
  )

})

test_that("print - default", {
  x <- mold(iris[, c("Sepal.Length"), drop = FALSE], iris$Species)

  expect_known_output(
    print(x$preprocessor),
    test_path("out/print-preprocessor-default-1.txt")
  )

})

library(recipes)

test_that("print - recipe", {
  recipe(Species ~ Sepal.Length, iris)
  x <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_known_output(
    print(x$preprocessor),
    test_path("out/print-preprocessor-recipe-1.txt")
  )

})
