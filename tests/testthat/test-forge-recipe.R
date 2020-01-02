context("test-forge-recipe")

library(recipes)

test_that("simple forge works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(iris, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("asking for the outcome works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(iris, x$blueprint, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    forge(iris2, x$blueprint, outcomes = TRUE),
    "The following required columns"
  )

})

test_that("outcomes steps get processed", {

  x <- mold(
    recipe(Sepal.Width ~ Sepal.Length, data = iris) %>%
      step_log(Sepal.Width),
    iris
  )

  processed <- forge(iris, x$blueprint, outcomes = TRUE)

  expect_equal(
    processed$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )

})

test_that("missing predictor columns fail appropriately", {

  x <- mold(
    recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_error(
    forge(iris[,1, drop = FALSE], x$blueprint),
    "Sepal.Width"
  )

  expect_error(
    forge(iris[,3, drop = FALSE], x$blueprint),
    "'Sepal.Length', 'Sepal.Width'"
  )

})

test_that("novel predictor levels are caught", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  x <- mold(recipe(y ~ f, dat), dat)

  expect_warning(
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

})

test_that("novel predictor levels can be ignored and handled by recipes", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  blueprint <- default_recipe_blueprint(allow_novel_levels = TRUE)

  rec1 <- recipe(y ~ f, dat)
  rec2 <- step_novel(rec1, f)

  x1 <- mold(rec1, dat, blueprint = blueprint)
  x2 <- mold(rec2, dat, blueprint = blueprint)

  # Recipes will silently handle the novel level
  expect_warning(
    xx <- forge(new, x1$blueprint),
    NA
  )

  expect_equal(
    xx$predictors$f[[5]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

  # `step_novel()` let's us handle the novel level differently
  expect_warning(
    xx <- forge(new, x2$blueprint),
    NA
  )

  expect_equal(
    xx$predictors$f[[5]],
    factor("new", levels = c("a", "b", "c", "d", "new"))
  )
})

test_that("novel predictor levels without any data are silently removed", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  # The 'e' level exists, but there is no data for it!
  new <- new[1:4,]

  x <- mold(recipe(y ~ f, dat), dat)

  expect_silent(
    xx <- forge(new, x$blueprint)
  )

  expect_equal(
    colnames(xx$predictors),
    colnames(x$predictors)
  )

})

test_that("novel outcome levels are caught", {

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  x <- mold(recipe(f ~ y, dat), dat)

  expect_warning(
    xx <- forge(new, x$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

})

test_that("original predictor and outcome classes / names are recorded", {

  x <- mold(
    recipe(Sepal.Length ~ Species, data = iris) %>%
      step_dummy(Species),
    iris
  )

  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    "Species"
  )

  expect_equal(
    colnames(x$blueprint$ptypes$outcomes),
    "Sepal.Length"
  )

  expect_equal(
    get_data_classes(x$blueprint$ptypes$predictors),
    list(Species = "factor")
  )

  expect_equal(
    get_data_classes(x$blueprint$ptypes$outcomes),
    list(Sepal.Length = "numeric")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  # Silent recovery
  expect_error(
    x_iris2 <- forge(iris2, x$blueprint),
    NA
  )

  expect_is(
    x_iris2$predictors$Species,
    "factor"
  )

  xx <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    xx_iris2 <- forge(iris2, xx$blueprint, outcomes = TRUE),
    NA
  )

  expect_is(
    xx_iris2$outcomes$Species,
    "factor"
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    forge(iris2, x$blueprint),
    NA
  )

  xx <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  expect_error(
    forge(iris2, xx$blueprint, outcomes = TRUE),
    NA
  )

})

test_that("an `extras` slot exists for `roles`", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(iris, x$blueprint)

  expect_equal(
    xx$extras,
    list(roles = NULL)
  )

})

test_that("only original non standard role columns are required", {

  # columns created by step_bs() shouldnt be required,
  # but are returned in `extras`
  x <- recipe(Species ~ ., iris) %>%
    step_bs(Sepal.Length, role = "dummy", deg_free = 3) %>%
    mold(iris)

  expect_error(
    xx <- forge(iris, x$blueprint),
    NA
  )

  expect_equal(
    colnames(xx$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )

})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  rec <- recipe(~ Sepal.Width, data = iris)
  x <- mold(rec, iris)
  expect_equal(forge(iris, x$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  rec <- recipe(~ Sepal.Width, data = iris)
  x <- mold(rec, iris)

  forged <- forge(iris, x$blueprint, outcomes = TRUE)
  outcomes <- forged$outcomes

  expect_equal(nrow(outcomes), 150)
  expect_equal(ncol(outcomes), 0)
})

test_that("Predictors with multiple roles are only included once before baking (#120)", {
  rec <- recipe(Species ~ ., iris) # Implict "predictor" role too
  rec <- add_role(rec, Sepal.Length, new_role = "test1")
  rec <- add_role(rec, Sepal.Length, new_role = "test2")

  x <- mold(rec, iris)

  forged <- forge(iris, x$blueprint)

  expect_true("Sepal.Length" %in% colnames(forged$predictors))
  expect_true("Sepal.Length" %in% colnames(forged$extras$roles$test1))
  expect_true("Sepal.Length" %in% colnames(forged$extras$roles$test2))
})
