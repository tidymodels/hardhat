context("test-mold-recipes")

library(recipes)

test_that("can mold recipes", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "data.frame")
  expect_is(x$outcomes[[1]], "factor")
  expect_is(x$blueprint, "default_recipe_blueprint")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x$blueprint))
})

test_that("can mold recipes with intercepts", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris,
    blueprint = default_recipe_blueprint(intercept = TRUE)
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
})

test_that("can pass `fresh` through to `prep()`", {
  iris1 <- iris[1:50,]
  iris2 <- iris[51:100,]

  rec <- recipe(Species ~ Sepal.Length, data = iris1)
  rec <- step_center(rec, Sepal.Length)

  prepped_rec <- prep(rec, iris1)
  non_fresh_mean <- prepped_rec$steps[[1]]$means

  x <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = FALSE))
  mold_non_fresh_mean <- x$blueprint$recipe$steps[[1]]$means

  y <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = TRUE))
  mold_fresh_mean <- y$blueprint$recipe$steps[[1]]$means

  fresh_mean <- c(Sepal.Length = mean(iris2$Sepal.Length))

  expect_equal(non_fresh_mean, mold_non_fresh_mean)
  expect_false(identical(non_fresh_mean, mold_fresh_mean))
  expect_equal(mold_fresh_mean, fresh_mean)
})

test_that("`data` is validated", {

  expect_error(
    mold(recipe(Species ~ Sepal.Length, data = iris), 1),
    "`data` must be a data.frame or a matrix"
  )

})

test_that("`extras` holds a slot for `roles`", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  expect_equal(x$extras, list(roles = NULL))

  expect_equal(x$blueprint$extra_role_ptypes, NULL)
})

test_that("non-standard roles columns are stored", {

  rec <- recipe(Species ~ ., iris) %>%
    update_role(Sepal.Width, new_role = "dummy")

  x <- mold(rec, iris)

  expect_equal(
    x$blueprint$extra_role_ptypes,
    list(dummy = tibble::tibble(Sepal.Width = double()))
  )

  expect_equal(
    x$extras$roles,
    list(dummy = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )

})

test_that("only original non-standard columns are in the extra roles ptype", {

  # same custom role, but not step_bs() columns arent original columns
  rec <- recipe(Species ~ ., iris) %>%
    update_role(Sepal.Width, new_role = "dummy") %>%
    step_bs(Sepal.Length, role = "dummy", deg_free = 3)

  x <- mold(rec, iris)

  # extra roles ptype only has original columns
  expect_equal(
    colnames(x$blueprint$extra_role_ptypes$dummy),
    "Sepal.Width"
  )

  # all `dummy` columns are returned in the `extras` slot
  expect_equal(
    colnames(x$extras$roles$dummy),
    c("Sepal.Width", paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_"))
  )

})

test_that("multiple extra roles types can be stored", {

  rec <- recipe(Species ~ ., iris) %>%
    step_bs(Sepal.Length, role = "dummy", deg_free = 3) %>%
    step_bs(Sepal.Width, role = "dummy2", deg_free = 3)

  x <- mold(rec, iris)

  # these are not original columns
  expect_equal(
    x$blueprint$extra_role_ptypes,
    NULL
  )

  expect_equal(
    names(x$extras$roles),
    c("dummy", "dummy2")
  )

})

test_that("`NA` roles are skipped over", {

  rec <- recipe(iris) %>%
    update_role(Sepal.Length, new_role = "predictor") %>%
    update_role(Species, new_role = "outcome") %>%
    update_role(Sepal.Width, new_role = "custom")

  x <- mold(rec, iris)

  expect_equal(
    colnames(x$predictors),
    "Sepal.Length"
  )

  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    "Sepal.Length"
  )

  expect_equal(
    colnames(x$outcomes),
    "Species"
  )

  expect_equal(
    colnames(x$blueprint$ptypes$outcomes),
    "Species"
  )

  expect_equal(
    colnames(x$extras$roles$custom),
    "Sepal.Width"
  )

  expect_equal(
    colnames(x$blueprint$extra_role_ptypes$custom),
    "Sepal.Width"
  )

})
