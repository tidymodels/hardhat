test_that("simple forge works", {
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
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(Species ~ Sepal.Length, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)

  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    forge(iris2, x1$blueprint, outcomes = TRUE),
    "The following required columns"
  )

  expect_error(
    forge(iris2, x2$blueprint, outcomes = TRUE),
    "The following required columns"
  )
})

test_that("outcomes steps get processed", {
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
  x <- mold(
    recipes::recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_error(
    forge(iris[, 1, drop = FALSE], x$blueprint),
    "Sepal.Width"
  )

  expect_error(
    forge(iris[, 3, drop = FALSE], x$blueprint),
    "'Sepal.Length', 'Sepal.Width'"
  )
})

test_that("novel predictor levels are caught", {
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
  x2 <- mold(recipes::step_dummy(recipes::recipe(y ~ f, dat), f), dat, blueprint = sparse_bp)

  expect_warning(
    xx1 <- forge(new, x1$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_warning(
    expect_warning(
      xx2 <- forge(new, x2$blueprint),
      "Novel levels found in column 'f': 'e'"
    ),
    "There are new levels in a factor: NA"
  )

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
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  bp1 <- default_recipe_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_recipe_blueprint(allow_novel_levels = TRUE, composition = "dgCMatrix")

  rec1 <- recipes::recipe(y ~ f, dat)
  rec2 <- recipes::step_novel(rec1, f)
  rec3 <- recipes::step_dummy(rec2, f)

  x1 <- mold(rec1, dat, blueprint = bp1)
  x2 <- mold(rec2, dat, blueprint = bp1)
  x3 <- mold(rec3, dat, blueprint = bp2)

  # Recipes will silently handle the novel level
  expect_warning(
    xx1 <- forge(new, x1$blueprint),
    NA
  )

  expect_equal(
    xx1$predictors$f[[5]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

  # `step_novel()` let's us handle the novel level differently
  expect_warning(
    xx2 <- forge(new, x2$blueprint),
    NA
  )

  expect_equal(
    xx2$predictors$f[[5]],
    factor("new", levels = c("a", "b", "c", "d", "new"))
  )

  expect_warning(
    xx3 <- forge(new, x3$blueprint),
    NA
  )

  expect_equal(
    colnames(xx3$predictors),
    c("f_b", "f_c", "f_d", "f_new")
  )
})

test_that("novel predictor levels without any data are silently removed", {
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

  expect_warning(
    xx1 <- forge(new, x1$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx1$outcomes[[5, 1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

  expect_warning(
    xx2 <- forge(new, x2$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx2$outcomes[[5, 1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )
})

test_that("original predictor and outcome classes / names are recorded", {
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
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)
  rec <- recipes::recipe(Sepal.Length ~ Species, iris)

  x1 <- mold(rec, iris)
  x2 <- mold(recipes::step_dummy(rec, Species), iris, blueprint = sparse_bp)

  # Silent recovery
  expect_error(
    xx1 <- forge(iris2, x1$blueprint),
    NA
  )
  expect_error(
    xx2 <- forge(iris2, x2$blueprint),
    NA
  )

  expect_s3_class(
    xx1$predictors$Species,
    "factor"
  )
  expect_s4_class(
    xx2$predictors,
    "dgCMatrix"
  )

  x3 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)
  x4 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris, blueprint = sparse_bp)

  expect_error(
    xx3 <- forge(iris2, x3$blueprint, outcomes = TRUE),
    NA
  )
  expect_error(
    xx4 <- forge(iris2, x4$blueprint, outcomes = TRUE),
    NA
  )

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
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x1 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)
  x2 <- mold(recipes::recipe(Species ~ Sepal.Length, iris), iris, blueprint = sparse_bp)

  expect_error(
    forge(iris2, x1$blueprint),
    NA
  )
  expect_error(
    forge(iris2, x2$blueprint),
    NA
  )

  rec <- recipes::recipe(Sepal.Length ~ Species, iris)
  x3 <- mold(rec, iris)
  x4 <- mold(recipes::step_dummy(rec, Species), iris, blueprint = sparse_bp)

  expect_error(
    forge(iris2, x3$blueprint, outcomes = TRUE),
    NA
  )
  expect_error(
    forge(iris2, x4$blueprint, outcomes = TRUE),
    NA
  )
})

test_that("an `extras` slot exists for `roles`", {
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

test_that("non-standard roles generated in the recipe must be listed as `bake_dependent_roles` to be returned", {
  # This is a convention we follow to make internal coding of `bake_dependent_roles` more straightforward.
  # You ONLY get back `bake_dependent_roles` in the `extras` slot of `forge()`.
  # I imagine this should be very rare.

  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::step_bs(rec, Sepal.Length, role = "dummy", deg_free = 3)

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  expect_identical(x$blueprint$extra_role_ptypes, NULL)

  # Even though `step_bs()` created some columns, we don't get them back in `forge()`
  expect_identical(
    xx$extras,
    list(roles = NULL)
  )

  bp <- default_recipe_blueprint(bake_dependent_roles = "dummy")
  x <- mold(rec, iris, blueprint = bp)
  xx <- forge(iris, x$blueprint)

  # Still none because they weren't in the original data
  expect_identical(x$blueprint$extra_role_ptypes, NULL)

  # We do get them here, because we specified them as bake dependent
  expect_identical(
    colnames(xx$extras$roles$dummy),
    paste("Sepal.Length", c("1", "2", "3"), sep = "_bs_")
  )
})

test_that("non-`bake_dependent_roles` should not be required in `forge()`, and won't be returned", {
  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "dummy2")

  x <- mold(rec, iris)
  xx <- forge(iris, x$blueprint)

  expect_named(xx$predictors, c("Petal.Length", "Petal.Width"))
  expect_null(xx$outcomes)

  expect_identical(
    xx$extras,
    list(roles = NULL)
  )

  bp <- default_recipe_blueprint(bake_dependent_roles = "dummy2")
  x <- mold(rec, iris, blueprint = bp)
  xx <- forge(iris, x$blueprint)

  expect_named(xx$predictors, c("Petal.Length", "Petal.Width"))
  expect_null(xx$outcomes)

  expect_identical(
    xx$extras$roles,
    list(dummy2 = tibble::tibble(Sepal.Length = iris$Sepal.Length))
  )
})

test_that("`bake_dependent_roles` can be dropped during the baking process", {
  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::update_role(rec, Sepal.Length, new_role = "dummy2")
  rec <- recipes::step_rm(rec, Sepal.Length)

  bp <- default_recipe_blueprint(bake_dependent_roles = c("dummy1", "dummy2"))
  x <- mold(rec, iris, blueprint = bp)
  xx <- forge(iris, x$blueprint)

  # `extra_role_ptypes` holds ptypes for both
  expect_identical(
    x$blueprint$extra_role_ptypes,
    list(
      dummy1 = tibble::tibble(Sepal.Width = double()),
      dummy2 = tibble::tibble(Sepal.Length = double())
    )
  )

  # But `Sepal.Length` got dropped along the way
  expect_identical(
    x$extras$roles,
    list(dummy1 = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
  expect_identical(
    xx$extras$roles,
    list(dummy1 = tibble::tibble(Sepal.Width = iris$Sepal.Width))
  )
})

test_that("recipes should error if `bake_dependent_roles` have been left out", {
  rec <- recipes::recipe(Species ~ ., iris)
  rec <- recipes::update_role(rec, Sepal.Width, new_role = "dummy1")
  rec <- recipes::step_log(rec, Sepal.Width)

  x <- mold(rec, iris)

  expect_error(forge(iris, x$blueprint))
})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
  rec <- recipes::recipe(~Sepal.Width, data = iris)
  x1 <- mold(rec, iris)
  x2 <- mold(rec, iris, blueprint = sparse_bp)
  expect_equal(forge(iris, x1$blueprint)$outcomes, NULL)
  expect_equal(forge(iris, x2$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
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
  rec <- recipes::recipe(Species ~ ., iris) # Implicit "predictor" role too
  rec <- recipes::add_role(rec, Sepal.Length, new_role = "test1")
  rec <- recipes::add_role(rec, Sepal.Length, new_role = "test2")

  bp <- default_recipe_blueprint(bake_dependent_roles = c("test1", "test2"))
  sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix", bake_dependent_roles = c("test1", "test2"))

  x1 <- mold(rec, iris, blueprint = bp)
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

test_that("`forge()` is compatible with hardhat 0.2.0 molded blueprints that don't have `bake_dependent_roles`", {
  path <- test_path("data", "hardhat-0.2.0-post-mold-recipe.rds")
  object <- readRDS(path)

  new_data <- object$new_data
  blueprint <- object$blueprint

  out <- forge(new_data, blueprint)

  expect <- recipes::bake(blueprint$recipe, new_data, recipes::all_predictors())
  expect_identical(out$predictors, expect)

  expect_identical(out$extras, list(roles = NULL))
})
