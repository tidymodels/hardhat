test_that("simple forge works", {
  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_formula_blueprint(composition = "matrix")

  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = sparse_bp)
  x3 <- mold(fac_1 ~ num_1, example_train, blueprint = matrix_bp)
  xx1 <- forge(example_train, x1$blueprint)
  xx2 <- forge(example_train, x2$blueprint)
  xx3 <- forge(example_train, x3$blueprint)

  expect_s3_class(xx1$predictors, "tbl_df")
  expect_s4_class(xx2$predictors, "dgCMatrix")
  expect_matrix(xx3$predictors)

  expect_equal(colnames(xx1$predictors), "num_1")
  expect_equal(colnames(xx2$predictors), "num_1")
  expect_equal(colnames(xx3$predictors), "num_1")

  expect_equal(xx1$outcomes, NULL)
  expect_equal(xx2$outcomes, NULL)
  expect_equal(xx3$outcomes, NULL)
})

test_that("can forge multivariate formulas", {
  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_formula_blueprint(composition = "matrix")

  x1 <- mold(num_1 + num_2 ~ num_3, example_train)
  x2 <- mold(num_1 + num_2 ~ num_3, example_train, blueprint = sparse_bp)
  x3 <- mold(num_1 + num_2 ~ num_3, example_train, blueprint = matrix_bp)
  xx1 <- forge(example_train, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(example_train, x2$blueprint, outcomes = TRUE)
  xx3 <- forge(example_train, x3$blueprint, outcomes = TRUE)

  expect_s3_class(xx1$outcomes, "tbl_df")
  expect_equal(colnames(xx1$outcomes), c("num_1", "num_2"))
  expect_equal(xx1$outcomes, xx3$outcomes)
  expect_equal(xx1$outcomes, xx3$outcomes)

  y1 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train)
  y2 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train, blueprint = sparse_bp)
  y3 <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train, blueprint = matrix_bp)
  yy1 <- forge(example_train, y1$blueprint, outcomes = TRUE)
  yy2 <- forge(example_train, y2$blueprint, outcomes = TRUE)
  yy3 <- forge(example_train, y3$blueprint, outcomes = TRUE)

  expect_equal(
    colnames(yy1$outcomes),
    c(
      "log(num_2)",
      "poly(num_2, degree = 2).1",
      "poly(num_2, degree = 2).2"
    )
  )
  expect_equal(yy1$outcomes, yy2$outcomes)
  expect_equal(yy1$outcomes, yy3$outcomes)
})

test_that("can forge new data without expanding factors into dummies", {
  x <- mold(
    num_1 ~ fac_1,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none")
  )

  xx <- forge(example_train, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "fac_1"
  )

  expect_s3_class(
    xx$predictors$fac_1,
    "factor"
  )
})

test_that("forging with `indicators = 'none'` works with numeric interactions", {
  x1 <- mold(
    fac_1 ~ num_2:num_1,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none")
  )

  xx1 <- forge(example_train, x1$blueprint)

  expect_equal(
    colnames(xx1$predictors),
    "num_2:num_1"
  )

  x2 <- mold(
    fac_1 ~ num_2:num_1,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none", composition = "dgCMatrix")
  )

  xx2 <- forge(example_train, x2$blueprint)

  expect_equal(
    colnames(xx2$predictors),
    "num_2:num_1"
  )
})

test_that("asking for the outcome works", {
  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  matrix_bp <- default_formula_blueprint(composition = "matrix")

  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = sparse_bp)
  x3 <- mold(fac_1 ~ num_1, example_train, blueprint = matrix_bp)
  xx1 <- forge(example_train, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(example_train, x2$blueprint, outcomes = TRUE)
  xx3 <- forge(example_train, x3$blueprint, outcomes = TRUE)

  expect_equal(
    xx1$outcomes,
    tibble::tibble(fac_1 = example_train$fac_1)
  )
  expect_equal(xx1$outcomes, xx2$outcomes)
  expect_equal(xx1$outcomes, xx3$outcomes)
})

test_that("asking for the outcome when it isn't there fails", {
  sparse_bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = sparse_bp)
  example_train2 <- example_train
  example_train2$fac_1 <- NULL

  expect_snapshot(
    error = TRUE,
    forge(example_train2, x1$blueprint, outcomes = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    forge(example_train2, x2$blueprint, outcomes = TRUE)
  )
})

test_that("can use special inline functions", {
  bp <- default_formula_blueprint(composition = "matrix")
  x1 <- mold(log(num_1) ~ poly(num_1, degree = 2), example_train)
  x2 <- mold(log(num_1) ~ poly(num_1, degree = 2), example_train, blueprint = bp)
  xx1 <- forge(example_train, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(example_train, x2$blueprint, outcomes = TRUE)

  # manually create poly df
  x_poly <- stats::poly(example_train$num_1, degree = 2)
  poly_df <- tibble::tibble(
    `poly(num_1, degree = 2)1` = x_poly[, 1],
    `poly(num_1, degree = 2)2` = x_poly[, 2]
  )

  # coerce to df for tolerance..tibbles don't have good tolerance
  expect_equal(
    as.data.frame(xx1$predictors),
    as.data.frame(poly_df)
  )
  expect_equal(
    xx2$predictors,
    as.matrix(poly_df)
  )

  expect_equal(
    xx1$outcomes,
    tibble::tibble(`log(num_1)` = log(example_train$num_1))
  )
  expect_equal(xx1$outcomes, xx2$outcomes)
})

test_that("new_data can be a matrix", {
  bp <- default_formula_blueprint(composition = "matrix")
  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = bp)
  example_train_mat <- as.matrix(example_train[, "num_1", drop = FALSE])

  expect_no_error(xx1 <- forge(example_train_mat, x1$blueprint))
  expect_no_error(xx2 <- forge(example_train_mat, x2$blueprint))

  sep_len <- example_train$num_1

  expect_equal(
    xx1$predictors,
    tibble::tibble(num_1 = sep_len)
  )
  expect_equal(
    unname(xx2$predictors),
    as.matrix(sep_len)
  )
})

test_that("new_data can only be a data frame / matrix", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = bp)

  expect_snapshot(
    error = TRUE,
    forge("hi", x1$blueprint)
  )
  expect_snapshot(
    error = TRUE,
    forge("hi", x2$blueprint)
  )
})

test_that("missing predictor columns fail appropriately", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ num_1 + num_2, example_train)
  x2 <- mold(fac_1 ~ num_1 + num_2, example_train, blueprint = bp)

  expect_snapshot(
    error = TRUE,
    forge(example_train[, 1, drop = FALSE], x1$blueprint)
  )
  expect_snapshot(
    error = TRUE,
    forge(example_train[, 1, drop = FALSE], x2$blueprint)
  )

  expect_snapshot(
    error = TRUE,
    forge(example_train[, 3, drop = FALSE], x1$blueprint)
  )
  expect_snapshot(
    error = TRUE,
    forge(example_train[, 3, drop = FALSE], x2$blueprint)
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

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(y ~ f, dat)
  x2 <- mold(y ~ f, dat, blueprint = bp)


  expect_snapshot(xx1 <- forge(new, x1$blueprint))
  expect_snapshot(xx2 <- forge(new, x2$blueprint))

  expect_equal(
    xx1$predictors[[5, 1]],
    NA_real_
  )
  expect_equal(
    unname(xx2$predictors[5, 1]),
    NA_real_
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

  bp1 <- default_formula_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_formula_blueprint(allow_novel_levels = TRUE, composition = "dgCMatrix")

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  # Silent
  expect_snapshot(xx1 <- forge(new, x1$blueprint))
  expect_snapshot(xx2 <- forge(new, x2$blueprint))

  expect_equal(
    xx1$predictors[[5, 5]],
    1
  )
  expect_equal(
    unname(xx2$predictors[5, 5]),
    1
  )
})

test_that("novel predictor levels without any data are silently removed", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")

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

  x1 <- mold(y ~ f, dat)
  x2 <- mold(y ~ f, dat, blueprint = bp)

  expect_silent(
    xx1 <- forge(new, x1$blueprint)
  )
  expect_silent(
    xx2 <- forge(new, x2$blueprint)
  )

  expect_equal(
    colnames(xx1$predictors),
    colnames(x1$predictors)
  )
  expect_equal(
    colnames(xx2$predictors),
    colnames(x2$predictors)
  )
})

test_that("novel predictor levels without any data are kept when novel levels are ignored", {
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

  bp1 <- default_formula_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_formula_blueprint(allow_novel_levels = TRUE, composition = "dgCMatrix")

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  expect_silent(
    xx1 <- forge(new, x1$blueprint)
  )
  expect_silent(
    xx2 <- forge(new, x2$blueprint)
  )

  expect_equal(
    colnames(xx1$predictors),
    c(colnames(x1$predictors), "fe")
  )
  expect_equal(
    colnames(xx2$predictors),
    c(colnames(x2$predictors), "fe")
  )
})

test_that("novel levels are handled correctly when the new column is a character", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = letters[1:5] # character!
  )

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(y ~ f, dat)
  x2 <- mold(y ~ f, dat, blueprint = bp)

  expect_snapshot(xx1 <- forge(new, x1$blueprint))
  expect_snapshot(xx2 <- forge(new, x2$blueprint))

  expect_equal(
    xx1$predictors[[5, 1]],
    NA_real_
  )
  expect_equal(
    unname(xx2$predictors[5, 1]),
    NA_real_
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4, ]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    xx1 <- forge(new2, x1$blueprint)
  )
  expect_silent(
    xx2 <- forge(new2, x2$blueprint)
  )
})

test_that("novel levels are handled correctly when the new column is a character and the original is ordered", {
  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = letters[1:5], # character!
    stringsAsFactors = FALSE
  )

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(y ~ f, dat)
  x2 <- mold(y ~ f, dat, blueprint = bp)

  expect_error(
    forge(new, x1$blueprint),
    class = "vctrs_error_cast_lossy"
  )
  expect_error(
    forge(new, x2$blueprint),
    class = "vctrs_error_cast_lossy"
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4, ]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    xx <- forge(new2, x1$blueprint)
  )
  expect_silent(
    xx <- forge(new2, x2$blueprint)
  )
})

test_that("novel levels are ignored correctly when the new column is a character", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = letters[1:5], # character!
    stringsAsFactors = FALSE
  )

  bp1 <- default_formula_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_formula_blueprint(allow_novel_levels = TRUE, composition = "dgCMatrix")

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  # Silent
  expect_snapshot(xx1 <- forge(new, x1$blueprint))
  expect_snapshot(xx2 <- forge(new, x2$blueprint))

  expect_equal(
    xx1$predictors[[5, 5]],
    1
  )
  expect_equal(
    unname(xx2$predictors[5, 5]),
    1
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4, ]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    yy1 <- forge(new2, x1$blueprint)
  )
  expect_silent(
    yy2 <- forge(new2, x2$blueprint)
  )

  expect_equal(
    colnames(yy1$predictors),
    colnames(xx1$predictors)[-5L]
  )
  expect_equal(
    colnames(yy2$predictors),
    colnames(xx2$predictors)[-5L]
  )
})

test_that("novel ordered factor predictor levels are never allowed", {
  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = ordered(letters[1:5])
  )

  bp1 <- default_formula_blueprint(indicators = "none", allow_novel_levels = FALSE)
  bp2 <- default_formula_blueprint(indicators = "none", allow_novel_levels = TRUE)

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  expect_error(
    forge(new, x1$blueprint),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    forge(new, x2$blueprint),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("ordered factor predictors with different level ordering is an error", {
  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  new <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4], levels = letters[c(1:2, 4, 3)])
  )

  bp1 <- default_formula_blueprint(indicators = "none", allow_novel_levels = FALSE)
  bp2 <- default_formula_blueprint(indicators = "none", allow_novel_levels = TRUE)

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  expect_error(
    forge(new, x1$blueprint),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    forge(new, x2$blueprint),
    class = "vctrs_error_incompatible_type"
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

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(f ~ y, dat)
  x2 <- mold(f ~ y, dat, blueprint = bp)

  expect_snapshot(xx1 <- forge(new, x1$blueprint, outcomes = TRUE))
  expect_snapshot(xx2 <- forge(new, x2$blueprint, outcomes = TRUE))

  expect_equal(
    xx1$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
  expect_equal(
    xx2$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
})

test_that("novel outcome levels are always caught, even if `allow_novel_levels = TRUE`", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = factor(letters[1:5])
  )

  bp1 <- default_formula_blueprint(allow_novel_levels = TRUE)
  bp2 <- default_formula_blueprint(allow_novel_levels = TRUE, composition = "matrix")

  x1 <- mold(f ~ y, dat, blueprint = bp1)
  x2 <- mold(f ~ y, dat, blueprint = bp2)

  expect_snapshot(xx1 <- forge(new, x1$blueprint, outcomes = TRUE))
  expect_snapshot(xx2 <- forge(new, x2$blueprint, outcomes = TRUE))

  expect_equal(
    xx1$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
  expect_equal(
    xx2$outcomes[[5, 1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )
})

test_that("missing predictor levels are restored silently", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  # Missing "d"
  new <- data.frame(
    y = 1:3,
    f = factor(letters[1:3])
  )

  # Missing "d" and "c"
  new2 <- data.frame(
    y = 1:2,
    f = factor(letters[1:2])
  )

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(y ~ f, dat)
  x2 <- mold(y ~ f, dat, blueprint = bp)

  # Silent
  expect_snapshot(xx1 <- forge(new, x1$blueprint))
  expect_snapshot(xx2 <- forge(new, x2$blueprint))

  expect_equal(
    colnames(xx1$predictors),
    c("fa", "fb", "fc", "fd")
  )
  expect_equal(
    colnames(xx2$predictors),
    c("fa", "fb", "fc", "fd")
  )

  # Silent
  expect_snapshot(yy1 <- forge(new2, x1$blueprint))
  expect_snapshot(yy2 <- forge(new2, x2$blueprint))

  expect_equal(
    colnames(yy1$predictors),
    c("fa", "fb", "fc", "fd")
  )
  expect_equal(
    colnames(yy2$predictors),
    c("fa", "fb", "fc", "fd")
  )
})

test_that("missing ordered factor levels are an error", {
  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  bp1 <- default_formula_blueprint(indicators = "none", allow_novel_levels = FALSE)
  bp2 <- default_formula_blueprint(indicators = "none", allow_novel_levels = TRUE)

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  x2 <- mold(y ~ f, dat, blueprint = bp2)

  new <- data.frame(
    y = 1:3,
    f = ordered(letters[1:3])
  )

  expect_error(
    forge(new, x1$blueprint),
    class = "vctrs_error_incompatible_type"
  )
  expect_error(
    forge(new, x2$blueprint),
    class = "vctrs_error_incompatible_type"
  )
})

test_that("can be both missing levels and have new levels", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:4,
    f = factor(letters[c(1:3, 5)])
  )

  bp1 <- default_formula_blueprint(indicators = "none")
  bp2 <- default_formula_blueprint(indicators = "none", composition = "matrix")

  x1 <- mold(y ~ f, dat, blueprint = bp1)
  expect_snapshot(error = TRUE, {
    mold(y ~ f, dat, blueprint = bp2)
  })

  # Warning for the extra level
  expect_snapshot(xx <- forge(new, x1$blueprint))

  expect_equal(
    levels(xx$predictors$f),
    levels(dat$f)
  )
})

test_that("can be both missing levels and have new levels that get ignored", {
  dat <- data.frame(
    y = 1:4,
    f = factor(letters[1:4])
  )

  new <- data.frame(
    y = 1:4,
    f = factor(letters[c(1:3, 5)])
  )

  blueprint <- default_formula_blueprint(indicators = "none", allow_novel_levels = TRUE)

  x <- mold(y ~ f, dat, blueprint = blueprint)

  # Silent
  expect_snapshot(xx <- forge(new, x$blueprint))

  # Order of levels comes from `new` first, then `dat`
  expect_equal(
    levels(xx$predictors$f),
    union(levels(new$f), levels(dat$f))
  )
})

test_that("`NA` factor data never triggers a novel level warning (#131)", {
  dat <- data.frame(
    y = 1:2,
    f = factor(c("x", "y"))
  )

  new <- data.frame(
    y = 1:3,
    f = factor(c("y", NA, "x"))
  )

  blueprint <- default_formula_blueprint(indicators = "none")

  x <- mold(y ~ f, dat, blueprint = blueprint)

  expect_snapshot(xx <- forge(new, x$blueprint))

  expect_identical(
    xx$predictors$f,
    new$f
  )
})

test_that("new data classes are caught", {
  example_train2 <- example_train
  example_train2$fac_1 <- as.character(example_train2$fac_1)

  x <- mold(num_1 ~ fac_1, example_train, blueprint = default_formula_blueprint(indicators = "none"))

  # Silently recover character -> factor
  expect_no_error(
    x_example_train2 <- forge(example_train2, x$blueprint)
  )

  expect_s3_class(
    x_example_train2$predictors$fac_1,
    "factor"
  )

  xx <- mold(fac_1 ~ num_1, example_train)

  expect_no_error(
    xx_example_train2 <- forge(example_train2, xx$blueprint, outcomes = TRUE)
  )

  expect_s3_class(
    xx_example_train2$outcomes$fac_1,
    "factor"
  )
})

test_that("new data classes can interchange integer/numeric", {
  example_train2 <- example_train
  example_train2$num_1 <- as.integer(example_train2$num_1)

  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(fac_1 ~ num_1, example_train)
  x2 <- mold(fac_1 ~ num_1, example_train, blueprint = bp)

  expect_no_error(forge(example_train2, x1$blueprint))
  expect_no_error(forge(example_train2, x2$blueprint))

  x3 <- mold(num_1 ~ fac_1, example_train)
  x4 <- mold(num_1 ~ fac_1, example_train, blueprint = bp)

  expect_no_error(
    forge(example_train2, x3$blueprint, outcomes = TRUE)
  )
  expect_no_error(
    forge(example_train2, x4$blueprint, outcomes = TRUE)
  )
})

test_that("intercepts can still be forged on when not using indicators (i.e. model.matrix())", {
  x <- mold(num_2 ~ fac_1, example_train, blueprint = default_formula_blueprint(intercept = TRUE, indicators = "none"))
  xx <- forge(example_train, x$blueprint)

  expect_true(
    "(Intercept)" %in% colnames(xx$predictors)
  )

  expect_s3_class(
    xx$predictors$fac_1,
    "factor"
  )
})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(~num_2, example_train)
  x2 <- mold(~num_2, example_train, blueprint = bp)
  expect_equal(forge(example_train, x1$blueprint)$outcomes, NULL)
  expect_equal(forge(example_train, x2$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  bp <- default_formula_blueprint(composition = "dgCMatrix")
  x1 <- mold(~num_2, example_train)
  x2 <- mold(~num_2, example_train, blueprint = bp)

  xx1 <- forge(example_train, x1$blueprint, outcomes = TRUE)
  xx2 <- forge(example_train, x2$blueprint, outcomes = TRUE)

  expect_equal(nrow(xx1$outcomes), 12)
  expect_equal(ncol(xx1$outcomes), 0)
  expect_equal(xx1$outcomes, xx2$outcomes)
})

# ------------------------------------------------------------------------------
# Character predictors

test_that("character predictors are treated as factors when `indicators` is not 'none'", {
  df <- data.frame(
    y = 1:2,
    x = c("a", "b"),
    z = c("c", "d"),
    stringsAsFactors = FALSE
  )

  bp1 <- default_formula_blueprint(indicators = "traditional")
  bp2 <- default_formula_blueprint(indicators = "one_hot")
  bp3 <- default_formula_blueprint(indicators = "traditional", composition = "matrix")
  bp4 <- default_formula_blueprint(indicators = "one_hot", composition = "matrix")

  res1 <- mold(y ~ x + z, df, blueprint = bp1)
  res2 <- mold(y ~ x + z, df, blueprint = bp2)
  res3 <- mold(y ~ x + z, df, blueprint = bp3)
  res4 <- mold(y ~ x + z, df, blueprint = bp4)

  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)
  x3 <- forge(df, res3$blueprint)
  x4 <- forge(df, res4$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("xa", "xb", "zd")
  )
  expect_identical(
    colnames(x3$predictors),
    c("xa", "xb", "zd")
  )

  expect_identical(
    colnames(x2$predictors),
    c("xa", "xb", "zc", "zd")
  )
  expect_identical(
    colnames(x4$predictors),
    c("xa", "xb", "zc", "zd")
  )
})

test_that("character predictors are left as characters when `indicators` is 'none'", {
  df <- data.frame(
    y = 1:2,
    x = c("a", "b"),
    z = c("c", "d"),
    stringsAsFactors = FALSE
  )

  bp <- default_formula_blueprint(indicators = "none")

  res <- mold(y ~ x + z, df, blueprint = bp)

  x <- forge(df, res$blueprint)

  expect_identical(
    colnames(x$predictors),
    c("x", "z")
  )

  expect_true(is.character(x$predictors$x))
  expect_true(is.character(x$predictors$z))
})

test_that("`new_data` uses the character predictor column's observed levels", {
  df1 <- tibble(x = c("a", "b", "c"))
  df2 <- tibble(x = c("a", "c"))

  bp1 <- default_formula_blueprint(indicators = "traditional")
  bp2 <- default_formula_blueprint(indicators = "one_hot")

  x <- mold(~ x, df1, blueprint = bp1)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, c("xa", "xb", "xc"))

  x <- mold(~ x, df1, blueprint = bp2)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, c("xa", "xb", "xc"))
})

test_that("`allow_novel_levels` works right with character predictors", {
  df1 <- tibble(x = c("a", "b", "c"))
  df2 <- tibble(x = c("a", "d"))

  bp <- default_formula_blueprint(allow_novel_levels = FALSE)
  x <- mold(~ x, df1, blueprint = bp)
  expect_snapshot({
    # Novel level warning about `d`
    out <- forge(df2, x$blueprint)
  })
  expect_named(out$predictors, c("xa", "xb", "xc"))

  bp <- default_formula_blueprint(allow_novel_levels = FALSE, indicators = "none")
  x <- mold(~ x, df1, blueprint = bp)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, "x")
  expect_identical(out$predictors$x, c("a", "d"))

  bp <- default_formula_blueprint(allow_novel_levels = TRUE)
  x <- mold(~ x, df1, blueprint = bp)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, c("xa", "xd", "xb", "xc"))
})

test_that("character vectors in `new_data` with 1 contrast level works properly with `indicators = traditional/one_hot` (#213)", {
  df1 <- tibble(x = c("a", "b"))
  df2 <- tibble(x = "a")

  bp <- default_formula_blueprint(indicators = "traditional")
  x <- mold(~ x, df1, blueprint = bp)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, c("xa", "xb"))
  expect_identical(out$predictors$xa, 1)
  expect_identical(out$predictors$xb, 0)

  bp <- default_formula_blueprint(indicators = "one_hot")
  x <- mold(~ x, df1, blueprint = bp)
  out <- forge(df2, x$blueprint)
  expect_named(out$predictors, c("xa", "xb"))
  expect_identical(out$predictors$xa, 1)
  expect_identical(out$predictors$xb, 0)
})

test_that("character vectors in `new_data` with 1 contrast level works properly with `indicators = none` (#213)", {
  df1 <- tibble(x = c("a", "b"))
  df2 <- tibble(x = "a")

  bp <- default_formula_blueprint(indicators = "none")
  x <- mold(~ x, df1, blueprint = bp)
  out <- forge(df2, x$blueprint)

  expect_named(out$predictors, "x")
  expect_identical(out$predictors$x, "a")
})

test_that("`new_data` can be converted losslessly from factor to character", {
  df <- data.frame(
    y = 1:2,
    x = c("a", "b"),
    z = c("c", "d"),
    stringsAsFactors = FALSE
  )

  new_df <- df
  new_df$x <- factor(new_df$x)

  bp1 <- default_formula_blueprint(indicators = "none")
  bp2 <- default_formula_blueprint(indicators = "traditional")
  bp3 <- default_formula_blueprint(indicators = "one_hot")
  bp4 <- default_formula_blueprint(indicators = "traditional", composition = "matrix")
  bp5 <- default_formula_blueprint(indicators = "one_hot", composition = "matrix")

  res1 <- mold(y ~ x + z, df, blueprint = bp1)
  res2 <- mold(y ~ x + z, df, blueprint = bp2)
  res3 <- mold(y ~ x + z, df, blueprint = bp3)
  res4 <- mold(y ~ x + z, df, blueprint = bp4)
  res5 <- mold(y ~ x + z, df, blueprint = bp5)

  x1 <- forge(new_df, res1$blueprint)
  x2 <- forge(new_df, res2$blueprint)
  x3 <- forge(new_df, res3$blueprint)
  x4 <- forge(new_df, res4$blueprint)
  x5 <- forge(new_df, res5$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("x", "z")
  )

  expect_true(is.character(x1$predictors$x))
  expect_true(is.character(x1$predictors$z))

  expect_identical(
    colnames(x2$predictors),
    c("xa", "xb", "zd")
  )
  expect_identical(
    colnames(x4$predictors),
    c("xa", "xb", "zd")
  )

  expect_identical(
    colnames(x3$predictors),
    c("xa", "xb", "zc", "zd")
  )
  expect_identical(
    colnames(x5$predictors),
    c("xa", "xb", "zc", "zd")
  )
})

# ------------------------------------------------------------------------------
# Factor encodings

test_that("traditional encoding and no intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "traditional"
  )
  bp2 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "traditional",
    composition = "dgCMatrix"
  )

  res1 <- mold(x ~ y + z, df, blueprint = bp1)
  res2 <- mold(x ~ y + z, df, blueprint = bp2)
  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("ya", "yb", "yc", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("ya", "yb", "yc", "zB")
  )
})

test_that("traditional encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "traditional"
  )
  bp2 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "traditional",
    composition = "dgCMatrix"
  )

  res1 <- mold(x ~ y + z, df, blueprint = bp1)
  res2 <- mold(x ~ y + z, df, blueprint = bp2)
  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("(Intercept)", "yb", "yc", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("(Intercept)", "yb", "yc", "zB")
  )
})

test_that("one-hot encoding and no intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "one_hot"
  )
  bp2 <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "one_hot",
    composition = "dgCMatrix"
  )

  res1 <- mold(x ~ y + z, df, blueprint = bp1)
  res2 <- mold(x ~ y + z, df, blueprint = bp2)
  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("ya", "yb", "yc", "zA", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("ya", "yb", "yc", "zA", "zB")
  )
})

test_that("one-hot encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp1 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "one_hot"
  )
  bp2 <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "one_hot",
    composition = "dgCMatrix"
  )

  res1 <- mold(x ~ y + z, df, blueprint = bp1)
  res2 <- mold(x ~ y + z, df, blueprint = bp2)
  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("(Intercept)", "ya", "yb", "yc", "zA", "zB")
  )
  expect_identical(
    colnames(x2$predictors),
    c("(Intercept)", "ya", "yb", "yc", "zA", "zB")
  )
})
