test_that("can mold recipes", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  expect_s3_class(x1$predictors, "data.frame")
  expect_s4_class(x2$predictors, "dgCMatrix")
  expect_matrix(x3$predictors)

  expect_s3_class(x1$outcomes, "data.frame")
  expect_s3_class(x2$outcomes, "data.frame")
  expect_s3_class(x3$outcomes, "data.frame")

  expect_equal(colnames(x1$predictors), "Sepal.Length")
  expect_equal(colnames(x2$predictors), "Sepal.Length")
  expect_equal(colnames(x3$predictors), "Sepal.Length")
  expect_s3_class(x1$outcomes[[1]], "factor")
  expect_s3_class(x1$blueprint, "default_recipe_blueprint")

  # Training data should _not_ be in the recipe
  expect_error(recipes::juice(x1$blueprint))
})

test_that("can mold recipes with intercepts", {
  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)

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

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris1)
  rec <- recipes::step_center(rec, Sepal.Length)

  prepped_rec <- recipes::prep(rec, iris1)
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

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris1)
  rec <- recipes::step_center(rec, Sepal.Length)

  prepped_rec <- recipes::prep(rec, iris1)
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
    mold(recipes::recipe(Species ~ Sepal.Length, data = iris), 1),
    "`data` must be a data.frame or a matrix"
  )
})

test_that("`extras` holds a slot for `roles`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
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

test_that("non-standard roles ptypes are stored by default", {
  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy")

  x <- mold(rec, iris)

  expect_identical(
    x$blueprint$extra_role_ptypes,
    list(dummy = tibble::tibble(Sepal.Width = double()))
  )

  expect_identical(
    x$extras$roles,
    list(dummy = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
})

test_that("case weights is not considered a required extra role by default", {
  # If we have this, we also have case weight support
  skip_if_no_update_role_requirements()

  iris$weight <- frequency_weights(seq_len(nrow(iris)))

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy")

  x <- mold(rec, iris)

  expect_identical(
    x$blueprint$extra_role_ptypes,
    list(dummy = tibble::tibble(Sepal.Width = double()))
  )

  expect_identical(
    x$extras$roles$dummy,
    tibble::tibble(Sepal.Width = iris$Sepal.Width)
  )
  expect_identical(
    x$extras$roles$case_weights,
    tibble::tibble(weight = iris$weight)
  )
})

test_that("case weights can be updated to be a required extra role", {
  skip_if_no_update_role_requirements()

  iris$weight <- frequency_weights(seq_len(nrow(iris)))

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy")
  rec <- recipes::update_role_requirements(rec, "case_weights", bake = TRUE)

  x <- mold(rec, iris)

  expect_identical(
    x$blueprint$extra_role_ptypes$dummy,
    tibble::tibble(Sepal.Width = double())
  )
  expect_identical(
    x$blueprint$extra_role_ptypes$case_weights,
    tibble::tibble(weight = frequency_weights(integer()))
  )

  expect_identical(
    x$extras$roles$dummy,
    tibble::tibble(Sepal.Width = iris$Sepal.Width)
  )
  expect_identical(
    x$extras$roles$case_weights,
    tibble::tibble(weight = iris$weight)
  )
})

test_that("only original non-standard columns are in the extra roles ptype", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  # same custom role, but note step_bs() columns aren't original columns
  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy")
  rec <- recipes::step_bs(rec, Sepal.Length, role = "dummy", deg_free = 3)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  # extra roles ptype only has original columns
  expect_identical(
    colnames(x1$blueprint$extra_role_ptypes$dummy),
    "Sepal.Width"
  )
  expect_identical(
    colnames(x2$blueprint$extra_role_ptypes$dummy),
    "Sepal.Width"
  )

  # all `dummy` columns are returned in the `extras` slot
  expect_identical(
    colnames(x1$extras$roles$dummy),
    c("Sepal.Width", paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_"))
  )
  expect_identical(
    colnames(x2$extras$roles$dummy),
    c("Sepal.Width", paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_"))
  )
})

test_that("`extra_role_ptypes` and `extras` only hold roles that actually exist in the data", {
  df <- tibble(y = 1, x = 1, z = 2)

  rec <- recipes::recipe(y ~ ., df)
  rec <- recipes::update_role(rec, x, new_role = "dummy")

  # For example `default_bake_role_requirements()` specifies that `NA` is
  # a required role, but there aren't any columns with that role in `df`
  x <- mold(rec, df)

  expect_named(x$blueprint$extra_role_ptypes, "dummy")
  expect_named(x$extras$roles, "dummy")
})

test_that("multiple extra roles types can be stored", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::step_bs(rec, Sepal.Length, role = "dummy", deg_free = 3)
  rec <- recipes::step_bs(rec, Sepal.Width, role = "dummy2", deg_free = 3)

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

  # They are automatically are returned in the `mold()` result
  expect_equal(
    names(x1$extras$roles),
    c("dummy", "dummy2")
  )
  expect_equal(
    names(x2$extras$roles),
    c("dummy", "dummy2")
  )
})

test_that("`NA` roles are treated as extra roles", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  rec <- recipes::recipe(iris)
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "predictor")
  rec <- recipes::update_role(rec, Species, new_role = "outcome")
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "custom")

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
    colnames(x1$extras$roles$`NA`),
    c("Petal.Length", "Petal.Width")
  )
  expect_equal(
    colnames(x2$extras$roles$`NA`),
    c("Petal.Length", "Petal.Width")
  )

  expect_equal(
    colnames(x1$blueprint$extra_role_ptypes$custom),
    "Sepal.Width"
  )
  expect_equal(
    colnames(x2$blueprint$extra_role_ptypes$custom),
    "Sepal.Width"
  )

  expect_equal(
    colnames(x1$blueprint$extra_role_ptypes$`NA`),
    c("Petal.Length", "Petal.Width")
  )
  expect_equal(
    colnames(x2$blueprint$extra_role_ptypes$`NA`),
    c("Petal.Length", "Petal.Width")
  )
})

test_that("roles that aren't required are not retained as `extra_role_ptypes`, but are in the mold result", {
  skip_if_no_update_role_requirements()

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "dummy2")
  rec <- recipes::update_role_requirements(rec, "dummy1", bake = FALSE)

  x <- mold(rec, iris)

  expect_equal(
    x$blueprint$extra_role_ptypes,
    list(dummy2 = tibble::tibble(Sepal.Length = double()))
  )

  expect_equal(
    x$extras$roles$dummy1,
    tibble::tibble(Sepal.Width = iris$Sepal.Width)
  )
  expect_equal(
    x$extras$roles$dummy2,
    tibble::tibble(Sepal.Length = iris$Sepal.Length)
  )
})

test_that("Missing y value returns a 0 column tibble for `outcomes`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  expect_equal(nrow(x1$outcomes), 150)
  expect_equal(ncol(x1$outcomes), 0)
  expect_equal(x1$outcomes, x2$outcomes)
})

test_that("Missing y value returns a 0 column / 0 row tibble for `ptype`", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  expect_equal(x1$blueprint$ptypes$outcomes, tibble())
  expect_equal(x2$blueprint$ptypes$outcomes, tibble())
})

test_that("`mold()` is compatible with hardhat 0.2.0 blueprints", {
  path <- test_path("data", "hardhat-0.2.0-pre-mold-recipe.rds")
  object <- readRDS(path)

  data <- object$data
  blueprint <- object$blueprint

  rec <- recipes::recipe(y ~ ., data = data)
  rec <- recipes::step_mutate(rec, z = 1)

  out <- mold(rec, data = data, blueprint = blueprint)

  expect <- tibble::tibble(x = data$x, z = 1)
  expect_identical(out$predictors, expect)
})
