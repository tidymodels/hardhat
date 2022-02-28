context("test-mold-recipes")

library(recipes)

test_that("can mold recipes", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  expect_is(x1$predictors, "data.frame")
  expect_is(x2$predictors, "dgCMatrix")
  expect_is(x3$predictors, "matrix")

  expect_is(x1$outcomes, "data.frame")
  expect_is(x2$outcomes, "data.frame")
  expect_is(x3$outcomes, "data.frame")

  expect_equal(colnames(x1$predictors), "Sepal.Length")
  expect_equal(colnames(x2$predictors), "Sepal.Length")
  expect_equal(colnames(x3$predictors), "Sepal.Length")
  expect_is(x1$outcomes[[1]], "factor")
  expect_is(x1$blueprint, "default_recipe_blueprint")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x1$blueprint))
})

test_that("can mold recipes with intercepts", {
  rec <- recipe(Species ~ Sepal.Length, data = iris)

  x1 <- mold(
    rec, iris,
    blueprint = default_recipe_blueprint(intercept = TRUE)
  )
  x2 <- mold(
    rec, iris,
    blueprint = default_recipe_blueprint(
      intercept = TRUE,
      composition = "dgCMatrix"
    )
  )
  x3 <- mold(
    rec, iris,
    blueprint = default_recipe_blueprint(
      intercept = TRUE,
      composition = "matrix"
    )
  )

  expect_true("(Intercept)" %in% colnames(x1$predictors))
  expect_true("(Intercept)" %in% colnames(x2$predictors))
  expect_true("(Intercept)" %in% colnames(x3$predictors))
})

test_that("can pass `fresh` through to `prep()`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  iris1 <- iris[1:50, ]
  iris2 <- iris[51:100, ]

  rec <- recipe(Species ~ Sepal.Length, data = iris1)
  rec <- step_center(rec, Sepal.Length)

  prepped_rec <- prep(rec, iris1)
  non_fresh_mean <- prepped_rec$steps[[1]]$means

  x1 <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = FALSE))
  mold_non_fresh_mean1 <- x1$blueprint$recipe$steps[[1]]$means
  x2 <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = FALSE, composition = "dgCMatrix"))
  mold_non_fresh_mean2 <- x2$blueprint$recipe$steps[[1]]$means

  y1 <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = TRUE))
  mold_fresh_mean1 <- y1$blueprint$recipe$steps[[1]]$means
  y2 <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint(fresh = TRUE, composition = "dgCMatrix"))
  mold_fresh_mean2 <- y2$blueprint$recipe$steps[[1]]$means

  fresh_mean <- c(Sepal.Length = mean(iris2$Sepal.Length))

  expect_equal(non_fresh_mean, mold_non_fresh_mean1)
  expect_false(identical(non_fresh_mean, mold_fresh_mean1))
  expect_equal(non_fresh_mean, mold_non_fresh_mean2)
  expect_false(identical(non_fresh_mean, mold_fresh_mean2))
  expect_equal(mold_fresh_mean1, fresh_mean)
  expect_equal(mold_fresh_mean2, fresh_mean)
})

test_that("`fresh` defaults to `TRUE`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  iris1 <- iris[1:50, ]
  iris2 <- iris[51:100, ]

  rec <- recipe(Species ~ Sepal.Length, data = iris1)
  rec <- step_center(rec, Sepal.Length)

  prepped_rec <- prep(rec, iris1)
  non_fresh_mean <- prepped_rec$steps[[1]]$means
  fresh_mean <- c(Sepal.Length = mean(iris2$Sepal.Length))

  x1 <- mold(prepped_rec, iris2, blueprint = default_recipe_blueprint())
  mold_fresh_mean1 <- x1$blueprint$recipe$steps[[1]]$means
  x2 <- mold(prepped_rec, iris2, blueprint = sparse_bp)
  mold_fresh_mean2 <- x2$blueprint$recipe$steps[[1]]$means

  expect_false(identical(non_fresh_mean, mold_fresh_mean1))
  expect_identical(mold_fresh_mean1, fresh_mean)
  expect_false(identical(non_fresh_mean, mold_fresh_mean2))
  expect_identical(mold_fresh_mean2, fresh_mean)
})

test_that("`data` is validated", {
  expect_error(
    mold(recipe(Species ~ Sepal.Length, data = iris), 1),
    "`data` must be a data.frame or a matrix"
  )
})

test_that("`extras` holds a slot for `roles`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  expect_equal(x1$extras, list(roles = NULL))
  expect_equal(x2$extras, list(roles = NULL))
  expect_equal(x3$extras, list(roles = NULL))

  expect_equal(x1$blueprint$extra_role_ptypes, NULL)
  expect_equal(x2$blueprint$extra_role_ptypes, NULL)
  expect_equal(x3$blueprint$extra_role_ptypes, NULL)
})

test_that("non-standard roles columns are stored", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipe(Species ~ ., iris) %>%
    update_role(Sepal.Width, new_role = "dummy")

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  expect_equal(
    x1$blueprint$extra_role_ptypes,
    list(dummy = tibble::tibble(Sepal.Width = double()))
  )
  expect_equal(
    x1$blueprint$extra_role_ptypes,
    x2$blueprint$extra_role_ptypes
  )
  expect_equal(
    x1$blueprint$extra_role_ptypes,
    x3$blueprint$extra_role_ptypes
  )

  expect_equal(
    x1$extras$roles,
    list(dummy = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
  expect_equal(
    x1$extras$roles,
    x2$extras$roles
  )
  expect_equal(
    x1$extras$roles,
    x3$extras$roles
  )
})

test_that("only original non-standard columns are in the extra roles ptype", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  # same custom role, but not step_bs() columns arent original columns
  rec <- recipe(Species ~ ., iris) %>%
    update_role(Sepal.Width, new_role = "dummy") %>%
    step_bs(Sepal.Length, role = "dummy", deg_free = 3)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  # extra roles ptype only has original columns
  expect_equal(
    colnames(x1$blueprint$extra_role_ptypes$dummy),
    "Sepal.Width"
  )
  expect_equal(
    colnames(x2$blueprint$extra_role_ptypes$dummy),
    "Sepal.Width"
  )

  # all `dummy` columns are returned in the `extras` slot
  expect_equal(
    colnames(x1$extras$roles$dummy),
    c("Sepal.Width", paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_"))
  )
  expect_equal(
    colnames(x2$extras$roles$dummy),
    c("Sepal.Width", paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_"))
  )
})

test_that("multiple extra roles types can be stored", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  rec <- recipe(Species ~ ., iris) %>%
    step_bs(Sepal.Length, role = "dummy", deg_free = 3) %>%
    step_bs(Sepal.Width, role = "dummy2", deg_free = 3)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  # these are not original columns
  expect_equal(
    x1$blueprint$extra_role_ptypes,
    NULL
  )
  expect_equal(
    x2$blueprint$extra_role_ptypes,
    NULL
  )

  expect_equal(
    names(x1$extras$roles),
    c("dummy", "dummy2")
  )
  expect_equal(
    names(x2$extras$roles),
    c("dummy", "dummy2")
  )
})

test_that("`NA` roles are skipped over", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  rec <- recipe(iris) %>%
    update_role(Sepal.Length, new_role = "predictor") %>%
    update_role(Species, new_role = "outcome") %>%
    update_role(Sepal.Width, new_role = "custom")

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  expect_equal(
    colnames(x1$predictors),
    "Sepal.Length"
  )
  expect_equal(
    colnames(x2$predictors),
    "Sepal.Length"
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$predictors),
    "Sepal.Length"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$predictors),
    "Sepal.Length"
  )

  expect_equal(
    colnames(x1$outcomes),
    "Species"
  )
  expect_equal(
    colnames(x2$outcomes),
    "Species"
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$outcomes),
    "Species"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$outcomes),
    "Species"
  )

  expect_equal(
    colnames(x1$extras$roles$custom),
    "Sepal.Width"
  )
  expect_equal(
    colnames(x2$extras$roles$custom),
    "Sepal.Width"
  )

  expect_equal(
    colnames(x1$blueprint$extra_role_ptypes$custom),
    "Sepal.Width"
  )
  expect_equal(
    colnames(x2$blueprint$extra_role_ptypes$custom),
    "Sepal.Width"
  )
})

test_that("Missing y value returns a 0 column tibble for `outcomes`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  expect_equal(nrow(x1$outcomes), 150)
  expect_equal(ncol(x1$outcomes), 0)
  expect_equal(x1$outcomes, x2$outcomes)
})

test_that("Missing y value returns a 0 column / 0 row tibble for `ptype`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  expect_equal(x1$blueprint$ptypes$outcomes, tibble())
  expect_equal(x2$blueprint$ptypes$outcomes, tibble())
})
