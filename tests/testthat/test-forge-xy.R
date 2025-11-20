test_that("simple forge works", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  x1 <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = sparse_bp
  )
  x3 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = matrix_bp
  )
  xx1 <- forge(iris, x1$blueprint)
  xx2 <- forge(iris, x2$blueprint)
  xx3 <- forge(iris, x3$blueprint)

  expect_s3_class(xx1$predictors, "tbl_df")
  expect_s4_class(xx2$predictors, "dgCMatrix")
  expect_matrix(xx3$predictors)

  expect_equal(colnames(xx1$predictors), "Sepal.Length")
  expect_equal(colnames(xx2$predictors), "Sepal.Length")
  expect_equal(colnames(xx3$predictors), "Sepal.Length")

  expect_equal(xx1$outcomes, NULL)
  expect_equal(xx2$outcomes, NULL)
  expect_equal(xx3$outcomes, NULL)
})

test_that("asking for the outcome works", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  x1 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE]
  )
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    blueprint = sparse_bp
  )
  x3 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    blueprint = matrix_bp
  )

  xx1 <- forge(iris, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(iris, x2$blueprint, outcomes = TRUE)
  xx3 <- forge(iris, x3$blueprint, outcomes = TRUE)

  expect_equal(
    xx1$outcomes,
    tibble::tibble(Species = iris$Species)
  )
  expect_equal(xx1$outcomes, xx3$outcomes)
  expect_equal(xx1$outcomes, xx3$outcomes)
})

test_that("asking for the outcome is special cased for vector `y` values", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  x1 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species
  )
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = sparse_bp
  )
  x3 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = matrix_bp
  )

  expect_equal(
    colnames(x1$blueprint$ptypes$outcomes),
    ".outcome"
  )
  expect_equal(
    colnames(x2$blueprint$ptypes$outcomes),
    ".outcome"
  )
  expect_equal(
    colnames(x3$blueprint$ptypes$outcomes),
    ".outcome"
  )

  iris2 <- iris
  iris2$.outcome <- iris2$Species
  iris2$Species <- NULL

  xx1 <- forge(iris2, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(iris2, x2$blueprint, outcomes = TRUE)
  xx3 <- forge(iris2, x3$blueprint, outcomes = TRUE)

  expect_equal(
    xx1$outcomes,
    tibble::tibble(.outcome = iris2$.outcome)
  )
  expect_equal(xx1$outcomes, xx3$outcomes)
  expect_equal(xx1$outcomes, xx3$outcomes)

  # This expects:
  # - standard message: "The following required columns"
  # - more detail: "`new_data` must include a column with the automatically generated name, '.outcome'"
  expect_snapshot(
    error = TRUE,
    forge(iris, x1$blueprint, outcomes = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    forge(iris, x2$blueprint, outcomes = TRUE)
  )
})

test_that("new_data can be a matrix", {
  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  iris_mat <- as.matrix(iris[, "Sepal.Length", drop = FALSE])

  expect_no_error(xx <- forge(iris_mat, x$blueprint))

  sep_len <- iris$Sepal.Length
  pred_tbl <- tibble::tibble(Sepal.Length = sep_len)

  expect_equal(
    xx$predictors,
    pred_tbl
  )
})

test_that("new_data can only be a data frame / matrix", {
  sparse_bp <- default_xy_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_xy_blueprint(composition = "matrix")

  x1 <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = sparse_bp
  )
  x3 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = matrix_bp
  )

  expect_snapshot(error = TRUE, forge("hi", x1$blueprint))
  expect_snapshot(error = TRUE, forge("hi", x2$blueprint))
  expect_snapshot(error = TRUE, forge("hi", x3$blueprint))
})

test_that("missing predictor columns fail appropriately", {
  bp <- default_xy_blueprint(composition = "dgCMatrix")
  x1 <- mold(
    iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE],
    iris$Species
  )
  x2 <- mold(
    iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE],
    iris$Species,
    blueprint = bp
  )

  expect_snapshot(
    error = TRUE,
    forge(iris[, 1, drop = FALSE], x1$blueprint)
  )
  expect_snapshot(
    error = TRUE,
    forge(iris[, 1, drop = FALSE], x2$blueprint)
  )

  expect_snapshot(
    error = TRUE,
    forge(iris[, 3, drop = FALSE], x1$blueprint)
  )
  expect_snapshot(
    error = TRUE,
    forge(iris[, 3, drop = FALSE], x2$blueprint)
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

  x <- mold(dat[, "f", drop = FALSE], dat$y)

  expect_snapshot({
    xx <- forge(new, x$blueprint)
  })

  expect_equal(
    xx$predictors[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
})

test_that("novel predictor levels can be ignored", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  blueprint <- default_xy_blueprint(allow_novel_levels = TRUE)

  x <- mold(dat[, "f", drop = FALSE], dat$y, blueprint = blueprint)

  # Silent
  expect_snapshot({
    xx <- forge(new, x$blueprint)
  })

  expect_equal(
    xx$predictors[[5, 1]],
    factor("e", c("a", "b", "c", "d", "e"))
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
  new <- new[1:4, ]

  x <- mold(dat[, "f", drop = FALSE], dat$y)

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

  bp <- default_xy_blueprint(composition = "dgCMatrix")
  x1 <- mold(
    x = dat[, "y", drop = FALSE],
    y = dat[, "f", drop = FALSE]
  )
  x2 <- mold(
    x = dat[, "y", drop = FALSE],
    y = dat[, "f", drop = FALSE],
    blueprint = bp
  )

  expect_snapshot({
    xx1 <- forge(new, x1$blueprint, outcomes = TRUE)
  })
  expect_snapshot({
    xx2 <- forge(new, x2$blueprint, outcomes = TRUE)
  })

  expect_equal(
    xx1$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
  expect_equal(
    xx2$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
})

test_that("original predictor and outcome classes are recorded", {
  bp <- default_xy_blueprint(composition = "dgCMatrix")
  x1 <- mold(
    iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE],
    iris$Species
  )
  x2 <- mold(
    iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE],
    iris$Species,
    blueprint = bp
  )

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$predictors),
    list(Sepal.Length = "numeric", Sepal.Width = "numeric")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$predictors),
    list(Sepal.Length = "numeric", Sepal.Width = "numeric")
  )

  expect_equal(
    get_data_classes(x1$blueprint$ptypes$outcomes),
    list(.outcome = "factor")
  )
  expect_equal(
    get_data_classes(x2$blueprint$ptypes$outcomes),
    list(.outcome = "factor")
  )
})

test_that("new data classes are caught", {
  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(iris[, "Species", drop = FALSE], iris$Sepal.Length)

  # Silent recovery
  expect_no_error(
    x_iris2 <- forge(iris2, x$blueprint)
  )

  expect_s3_class(
    x_iris2$predictors$Species,
    "factor"
  )

  xx <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  iris3 <- iris2
  iris3$.outcome <- iris2$Species
  iris3$Species <- NULL

  expect_no_error(
    xx_iris3 <- forge(iris3, xx$blueprint, outcomes = TRUE)
  )

  expect_s3_class(
    xx_iris3$outcomes$.outcome,
    "factor"
  )
})

test_that("new data classes can interchange integer/numeric", {
  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  bp <- default_xy_blueprint(composition = "dgCMatrix")
  x1 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species
  )
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris$Species,
    blueprint = bp
  )

  expect_no_error(forge(iris2, x1$blueprint))
  expect_no_error(forge(iris2, x2$blueprint))
})

test_that("intercept is not included as a predictor", {
  x1 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    blueprint = default_xy_blueprint(intercept = TRUE)
  )
  x2 <- mold(
    iris[, "Sepal.Length", drop = FALSE],
    iris[, "Species", drop = FALSE],
    blueprint = default_xy_blueprint(intercept = TRUE, composition = "matrix")
  )

  expect_false(
    "(Intercept)" %in% colnames(x1$blueprint$ptypes$predictors)
  )
  expect_false(
    "(Intercept)" %in% colnames(x2$blueprint$ptypes$predictors)
  )

  expect_no_error(xx1 <- forge(iris, x1$blueprint))
  expect_no_error(xx2 <- forge(iris, x2$blueprint))

  expect_equal(
    colnames(xx1$predictors),
    c("(Intercept)", "Sepal.Length")
  )
  expect_equal(
    colnames(xx2$predictors),
    c("(Intercept)", "Sepal.Length")
  )

  # again, with matrices
  y <- mold(
    as.matrix(iris[, "Sepal.Length", drop = FALSE]),
    iris$Sepal.Width,
    blueprint = default_xy_blueprint(intercept = TRUE)
  )

  expect_false(
    "(Intercept)" %in% colnames(y$blueprint$ptypes$predictors)
  )
})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  x <- mold(iris, y = NULL)
  expect_equal(forge(iris, x$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  x <- mold(iris, y = NULL)

  forged <- forge(iris, x$blueprint, outcomes = TRUE)
  outcomes <- forged$outcomes

  expect_equal(nrow(outcomes), 150)
  expect_equal(ncol(outcomes), 0)
})
