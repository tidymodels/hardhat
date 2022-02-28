test_that("unknown mold() inputs throw an error", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  expect_error(
    mold("hi"),
    "`x` is not a recognized type.\nOnly data.frame, matrix, recipe, and formula objects are allowed.\nA character was specified."
  )
  expect_error(
    mold("hi", blueprint = sparse_bp),
    "`x` is not a recognized type.\nOnly data.frame, matrix, recipe, and formula objects are allowed.\nA character was specified."
  )
  expect_error(
    mold("hi", blueprint = matrix_bp),
    "`x` is not a recognized type.\nOnly data.frame, matrix, recipe, and formula objects are allowed.\nA character was specified."
  )
})

test_that("can use x-y mold interface", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  x1 <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  x2 <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species, blueprint = sparse_bp)
  x3 <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species, blueprint = matrix_bp)

  expect_s3_class(x1$predictors, "tbl_df")
  expect_s4_class(x2$predictors, "dgCMatrix")
  expect_matrix(x3$predictors)

  expect_equal(colnames(x1$predictors), "Sepal.Length")
  expect_equal(colnames(x2$predictors), "Sepal.Length")
  expect_equal(colnames(x3$predictors), "Sepal.Length")

  expect_s3_class(x1$outcomes, "tbl_df")
  expect_s3_class(x2$outcomes, "tbl_df")
  expect_s3_class(x3$outcomes, "tbl_df")
  expect_equal(colnames(x1$outcomes), ".outcome")
  expect_equal(colnames(x2$outcomes), ".outcome")
  expect_equal(colnames(x3$outcomes), ".outcome")
  expect_s3_class(x1$blueprint, "default_xy_blueprint")
})

test_that("xy intercepts can be added", {
  x1 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = default_xy_blueprint(intercept = TRUE)
  )
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = default_xy_blueprint(intercept = TRUE, composition = "matrix")
  )

  expect_true("(Intercept)" %in% colnames(x1$predictors))
  expect_true("(Intercept)" %in% colnames(x2$predictors))
})

test_that("cannot pass anything in the dots", {
  expect_error(
    mold(
      iris[, "Sepal.Length", drop = FALSE],
      iris$Species,
      z = "in the dots"
    ),
    "`...` must not contain any input. 1 elements were found"
  )
  expect_error(
    mold(
      iris[, "Sepal.Length", drop = FALSE],
      iris$Species,
      blueprint = default_xy_blueprint(composition = "dgCMatrix"),
      z = "in the dots"
    ),
    "`...` must not contain any input. 1 elements were found"
  )
})

test_that("`NULL` y value returns a 0 column tibble for `outcomes`", {
  x <- mold(iris, y = NULL)

  expect_equal(nrow(x$outcomes), 150)
  expect_equal(ncol(x$outcomes), 0)
})

test_that("Missing y value returns a 0 column / 0 row tibble for `ptype`", {
  x <- mold(iris, y = NULL)
  expect_equal(x$blueprint$ptypes$outcomes, tibble())
})
