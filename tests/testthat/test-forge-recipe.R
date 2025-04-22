test_that("simple forge works", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  rec <- recipes::step_normalize(rec, Sepal.Length)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  xx1 <- forge(iris, x1$blueprint)
  xx2 <- forge(iris, x2$blueprint)
  xx3 <- forge(iris, x3$blueprint)

  expect_s3_class(
    xx1$predictors,
    "tbl_df"
  )
  expect_s4_class(
    xx2$predictors,
    "dgCMatrix"
  )
  expect_matrix(
    xx3$predictors
  )

  expect_equal(
    colnames(xx1$predictors),
    "Sepal.Length"
  )
  expect_equal(
    colnames(xx2$predictors),
    "Sepal.Length"
  )
  expect_equal(
    colnames(xx3$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx1$outcomes,
    NULL
  )
  expect_equal(
    xx2$outcomes,
    NULL
  )
  expect_equal(
    xx3$outcomes,
    NULL
  )
})

test_that("asking for the outcome works", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_recipe_blueprint(composition = "matrix")

  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  rec <- recipes::step_normalize(rec, Sepal.Length)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  x3 <- mold(rec, iris, blueprint = matrix_bp)

  xx1 <- forge(iris, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(iris, x2$blueprint, outcomes = TRUE)
  xx3 <- forge(iris, x2$blueprint, outcomes = TRUE)

  expect_equal(
    xx1$outcomes,
    tibble::tibble(Species = iris$Species)
  )
  expect_equal(
    xx2$outcomes,
    tibble::tibble(Species = iris$Species)
  )
  expect_equal(
    xx3$outcomes,
    tibble::tibble(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  iris2 <- iris
  iris2$Species <- NULL

  expect_snapshot(
    error = TRUE,
    forge(iris2, x1$blueprint, outcomes = TRUE)
  )

  expect_snapshot(
    error = TRUE,
    forge(iris2, x2$blueprint, outcomes = TRUE)
  )
})

test_that("outcomes steps get processed", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(Sepal.Width ~ Sepal.Length, data = iris)
  rec <- recipes::step_log(rec, Sepal.Width)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  xx1 <- forge(iris, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(iris, x2$blueprint, outcomes = TRUE)

  expect_equal(
    xx1$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )
  expect_equal(
    xx2$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )
})

test_that("missing predictor columns fail appropriately", {
  skip_if_not_installed("recipes")

  x <- mold(
    recipes::recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_snapshot(
    error = TRUE,
    forge(iris[, 1, drop = FALSE], x$blueprint)
  )

  expect_snapshot(
    error = TRUE,
    forge(iris[, 3, drop = FALSE], x$blueprint)
  )
})

test_that("novel predictor levels are caught", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  x1 <- mold(recipes::recipe(y ~ f, dat), dat)
  x2 <- mold(
    recipes::step_dummy(recipes::recipe(y ~ f, dat), f),
    dat,
    blueprint = sparse_bp
  )

  expect_snapshot({
    xx1 <- forge(new, x1$blueprint)
  })

  expect_snapshot({
    xx2 <- forge(new, x2$blueprint)
  })

  expect_equal(
    xx1$predictors[[5, 1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )
  expect_equal(
    unname(xx2$predictors[5, ]),
    rep(NA_real_, 3)
  )
})

test_that("novel predictor levels can be ignored and handled by recipes", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  bp1 <- default_recipe_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_recipe_blueprint(
    allow_novel_levels = TRUE,
    composition = "dgCMatrix"
  )

  rec1 <- recipes::recipe(y ~ f, dat)
  rec2 <- recipes::step_novel(rec1, f)
  rec3 <- recipes::step_dummy(rec2, f)

  x1 <- mold(rec1, dat, blueprint = bp1)
  x2 <- mold(rec2, dat, blueprint = bp1)
  x3 <- mold(rec3, dat, blueprint = bp2)

  # Recipes will silently handle the novel level
  expect_snapshot({
    xx1 <- forge(new, x1$blueprint)
  })

  expect_equal(
    xx1$predictors$f[[5]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

  # `step_novel()` let's us handle the novel level differently
  expect_snapshot({
    xx2 <- forge(new, x2$blueprint)
  })

  expect_equal(
    xx2$predictors$f[[5]],
    factor("new", levels = c("a", "b", "c", "d", "new"))
  )

  # Silent
  expect_snapshot({
    xx3 <- forge(new, x3$blueprint)
  })

  expect_equal(
    colnames(xx3$predictors),
    c("f_b", "f_c", "f_d", "f_new")
  )
})

test_that("novel predictor levels without any data are silently removed", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  # The 'e' level exists, but there is no data for it!
  new <- new[1:4, ]

  rec1 <- recipes::recipe(y ~ f, dat)
  rec2 <- recipes::step_dummy(rec1, f)

  x1 <- mold(rec1, dat)
  x2 <- mold(rec2, dat, blueprint = sparse_bp)

  expect_silent(
    xx1 <- forge(new, x1$blueprint)
  )

  expect_equal(
    colnames(xx1$predictors),
    colnames(x1$predictors)
  )

  expect_silent(
    xx2 <- forge(new, x2$blueprint)
  )

  expect_equal(
    colnames(xx2$predictors),
    c("f_b", "f_c", "f_d")
  )
})

test_that("novel outcome levels are caught", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  rec1 <- recipes::recipe(f ~ y, dat)

  x1 <- mold(rec1, dat)
  x2 <- mold(rec1, dat, blueprint = sparse_bp)

  expect_snapshot({
    xx1 <- forge(new, x1$blueprint, outcomes = TRUE)
  })

  expect_equal(
    xx1$outcomes[[5, 1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

  expect_snapshot({
    xx2 <- forge(new, x2$blueprint, outcomes = TRUE)
  })

  expect_equal(
    xx2$outcomes[[5, 1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )
})

test_that("original predictor and outcome classes / names are recorded", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  rec <- recipes::recipe(Sepal.Length ~ Species, data = iris)
  rec <- recipes::step_dummy(rec, Species)

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  expect_equal(
    colnames(x1$blueprint$ptypes$predictors),
    "Species"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$predictors),
    "Species"
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$outcomes),
    "Sepal.Length"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$outcomes),
    "Sepal.Length"
  )

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$predictors),
    list(Species = "factor")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$predictors),
    list(Species = "factor")
  )

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$outcomes),
    list(Sepal.Length = "numeric")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$outcomes),
    list(Sepal.Length = "numeric")
  )
})

test_that("new data classes are caught", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)
  rec <- recipes::recipe(Sepal.Length ~ Species, iris)

  x1 <- mold(rec, iris)
  x2 <- mold(recipes::step_dummy(rec, Species), iris, blueprint = sparse_bp)

  # Silent recovery
  expect_no_error(xx1 <- forge(iris2, x1$blueprint))
  expect_no_error(xx2 <- forge(iris2, x2$blueprint))

  expect_s3_class(
    xx1$predictors$Species,
    "factor"
  )
  expect_s4_class(
    xx2$predictors,
    "dgCMatrix"
  )

  x3 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)
  x4 <- mold(
    recipes::recipe(Species ~ Sepal.Length, iris),
    iris,
    blueprint = sparse_bp
  )

  expect_no_error(xx3 <- forge(iris2, x3$blueprint, outcomes = TRUE))
  expect_no_error(xx4 <- forge(iris2, x4$blueprint, outcomes = TRUE))

  expect_s3_class(
    xx3$outcomes$Species,
    "factor"
  )
  expect_s3_class(
    xx4$outcomes$Species,
    "factor"
  )
})

test_that("new data classes can interchange integer/numeric", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x1 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)
  x2 <- mold(
    recipes::recipe(Species ~ Sepal.Length, iris),
    iris,
    blueprint = sparse_bp
  )

  expect_no_error(forge(iris2, x1$blueprint))
  expect_no_error(forge(iris2, x2$blueprint))

  rec <- recipes::recipe(Sepal.Length ~ Species, iris)
  x3 <- mold(rec, iris)
  x4 <- mold(recipes::step_dummy(rec, Species), iris, blueprint = sparse_bp)

  expect_no_error(
    forge(iris2, x3$blueprint, outcomes = TRUE)
  )
  expect_no_error(
    forge(iris2, x4$blueprint, outcomes = TRUE)
  )
})

test_that("an `extras` slot exists for `roles`", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  xx1 <- forge(iris, x1$blueprint)
  xx2 <- forge(iris, x2$blueprint)

  expect_equal(
    xx1$extras,
    list(roles = NULL)
  )
  expect_equal(
    xx2$extras,
    list(roles = NULL)
  )
})

test_that("non-standard roles generated in the recipe are returned by both `mold()` and `forge()`", {
  skip_if_not_installed("recipes")

  # This is a convention we follow to have consistency between `mold()` and `forge()`.
  # Both should have the same `.$extras$roles$<name>` slot names.

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::step_bs(rec, Sepal.Length, role = "dummy", deg_free = 3)

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  expect_identical(x$blueprint$extra_role_ptypes, NULL)

  # Same slots in both `mold()` and `forge()` results
  expect_identical(
    colnames(x$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )
  expect_identical(
    colnames(xx$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )
})

test_that("`forge()` returns a slot for non standard roles that aren't required at `bake()` time", {
  skip_if_not_installed("recipes")

  # This is a convention we follow to have consistency between `mold()` and `forge()`.
  # Both should have the same `.$extras$roles$<name>` slot names, even if there
  # wasn't any data there at `forge()` time (because it wasn't a required role).

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "id")
  rec <- recipes::update_role_requirements(rec, "id", bake = FALSE)
  rec <- recipes::step_bs(rec, Sepal.Length, role = "dummy", deg_free = 3)

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  expect_identical(x$blueprint$extra_role_ptypes, NULL)

  # Same slots in both `mold()` and `forge()` results
  expect_identical(
    colnames(x$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )
  expect_identical(
    colnames(xx$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )

  expect_identical(
    x$extras$roles$id,
    tibble(Sepal.Width = iris$Sepal.Width)
  )
  # There is a slot for this, but no data is returned here, because `"id"`
  # wasn't a required role, so `Sepal.Width` was removed before being passed
  # to `bake()`
  expect_identical(
    xx$extras$roles$id,
    tibble::new_tibble(x = list(), nrow = nrow(iris))
  )
})

test_that("required non standard roles can be dropped during the baking process", {
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "dummy2")
  rec <- recipes::step_rm(rec, Sepal.Length)

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  # `extra_role_ptypes` holds ptypes for both
  expect_identical(
    x$blueprint$extra_role_ptypes$dummy1,
    tibble::tibble(Sepal.Width = double())
  )
  expect_identical(
    x$blueprint$extra_role_ptypes$dummy2,
    tibble::tibble(Sepal.Length = double())
  )

  # But `Sepal.Length` got dropped along the way and isn't in the `term_info`
  # of the recipe, so it doesn't appear in the final result
  expect_identical(
    x$extras$roles,
    list(dummy1 = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
  expect_identical(
    xx$extras$roles,
    list(dummy1 = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
})

test_that("`forge()` will error if required non standard roles are missing", {
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")

  x <- mold(rec, iris)

  iris$Sepal.Width <- NULL

  expect_snapshot(error = TRUE, {
    forge(iris, x$blueprint)
  })
})

test_that("recipes will error if the role is declared as not required, but really was", {
  skip_if_not_installed("recipes")

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::update_role_requirements(rec, "dummy1", bake = FALSE)
  rec <- recipes::step_log(rec, Sepal.Width)

  x <- mold(rec, iris)

  # Error is specific to however `step_log()` handles this
  expect_error(forge(iris, x$blueprint))
})

test_that("`NA` roles are treated as extra roles that are required at `forge()` time", {
  skip_if_not_installed("recipes")

  # `Petal.Length`, `Petal.Width` have `NA` roles
  rec <- recipes::recipe(iris)
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "predictor")
  rec <- recipes::update_role(rec, Species, new_role = "outcome")
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "custom")

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  # They are returned in `forge()` as extras
  expect_identical(
    xx$extras$roles$`NA`,
    tibble(Petal.Length = iris$Petal.Length, Petal.Width = iris$Petal.Width)
  )

  # They are required at `forge()` time
  iris$Petal.Length <- NULL

  expect_snapshot(error = TRUE, {
    forge(iris, x$blueprint)
  })
})

test_that("`NA` roles can be declared as not required at `forge()` time", {
  skip_if_not_installed("recipes")

  # Petal.Length, Petal.Width have `NA` roles
  rec <- recipes::recipe(iris)
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "predictor")
  rec <- recipes::update_role(rec, Species, new_role = "outcome")
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "custom")
  rec <- recipes::update_role_requirements(rec, "NA", bake = FALSE)

  x <- mold(rec, iris)

  # No longer required at `forge()` time
  iris$Petal.Length <- NULL

  xx <- forge(iris, x$blueprint)

  # And `Petal.Width` (which is still in `iris`) won't be in the output
  expect_identical(
    xx$extras$roles$`NA`,
    tibble::new_tibble(x = list(), nrow = nrow(iris))
  )
})

test_that("`extras` only hold roles that actually exist in the data", {
  skip_if_not_installed("recipes")

  df <- tibble(y = 1, x = 1, z = 2)

  rec <- recipes::recipe(y ~ ., df)
  rec <- recipes::update_role(rec, x, new_role = "dummy")

  # For example `default_bake_role_requirements()` specifies that `NA` is
  # a required role, but there aren't any columns with that role in `df`
  x <- mold(rec, df)
  xx <- forge(df, x$blueprint)

  expect_named(xx$extras$roles, "dummy")
})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  expect_equal(forge(iris, x1$blueprint)$outcomes, NULL)
  expect_equal(forge(iris, x2$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  xx1 <- forge(iris, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(iris, x2$blueprint, outcomes = TRUE)

  expect_equal(nrow(xx1$outcomes), 150)
  expect_equal(ncol(xx1$outcomes), 0)
  expect_equal(nrow(xx2$outcomes), 150)
  expect_equal(ncol(xx2$outcomes), 0)
})

test_that("Predictors with multiple roles are only included once before baking (#120)", {
  skip_if_not_installed("recipes")
  skip_if_not_installed("Matrix")

  rec <- recipes::recipe(Species ~ ., iris) # Implicit "predictor" role too
  rec <- recipes::add_role(rec, Sepal.Length, new_role = "test1")
  rec <- recipes::add_role(rec, Sepal.Length, new_role = "test2")

  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  xx1 <- forge(iris, x1$blueprint)
  xx2 <- forge(iris, x2$blueprint)

  expect_true("Sepal.Length" %in% colnames(xx1$predictors))
  expect_true("Sepal.Length" %in% colnames(xx1$extras$roles$test1))
  expect_true("Sepal.Length" %in% colnames(xx1$extras$roles$test2))
  expect_true("Sepal.Length" %in% colnames(xx2$predictors))
  expect_true("Sepal.Length" %in% colnames(xx2$extras$roles$test1))
  expect_true("Sepal.Length" %in% colnames(xx2$extras$roles$test2))
})

test_that("`strings_as_factors` is used at `bake()` time", {
  skip_if_not_installed("recipes")

  df <- tibble(y = "a", x = "b")
  rec <- recipes::recipe(y ~ x, data = df)

  # Default is `TRUE`
  x <- mold(rec, df)
  out <- forge(df, x$blueprint, outcomes = TRUE)
  expect_identical(out$outcomes$y, factor("a"))
  expect_identical(out$predictors$x, factor("b"))

  x <- mold(
    rec,
    df,
    blueprint = default_recipe_blueprint(strings_as_factors = FALSE)
  )
  out <- forge(df, x$blueprint, outcomes = TRUE)
  expect_identical(out$outcomes$y, "a")
  expect_identical(out$predictors$x, "b")
})

test_that("`forge()` is compatible with hardhat 0.2.0 molded blueprints with a basic recipe", {
  skip_if_not_installed("recipes")

  path <- test_path("data", "hardhat-0.2.0-post-mold-recipe.rds")
  object <- readRDS(path)

  new_data <- object$new_data
  blueprint <- object$blueprint

  out <- forge(new_data, blueprint)

  expect <- recipes::bake(blueprint$recipe, new_data, recipes::all_predictors())
  expect_identical(out$predictors, expect)

  expect_identical(out$extras, list(roles = NULL))

  new_data$x <- NULL

  expect_snapshot(error = TRUE, {
    forge(new_data, blueprint)
  })
})

test_that("`forge()` is compatible with hardhat 0.2.0 molded blueprints with a recipe with a nonstandard role", {
  skip_if_not_installed("recipes")

  path <- test_path(
    "data",
    "hardhat-0.2.0-post-mold-recipe-nonstandard-role.rds"
  )
  object <- readRDS(path)

  new_data <- object$new_data
  blueprint <- object$blueprint

  out <- forge(new_data, blueprint)

  expect <- recipes::bake(blueprint$recipe, new_data, recipes::all_predictors())
  expect_identical(out$predictors, expect)

  expect <- recipes::bake(blueprint$recipe, new_data, id)
  expect_identical(out$extras, list(roles = list(id = expect)))

  new_data$id <- NULL

  expect_snapshot(error = TRUE, {
    forge(new_data, blueprint)
  })
})
