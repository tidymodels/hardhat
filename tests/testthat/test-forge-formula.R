context("test-forge-formula")

test_that("simple forge works", {
  x <- mold(fac_1 ~ num_1, example_train)
  xx <- forge(example_train, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "num_1"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("can forge multivariate formulas", {

  x <- mold(num_1 + num_2 ~ num_3, example_train)
  xx <- forge(example_train, x$blueprint, outcomes = TRUE)

  expect_is(xx$outcomes, "tbl_df")
  expect_equal(colnames(xx$outcomes), c("num_1", "num_2"))

  y <- mold(log(num_2) + poly(num_2, degree = 2) ~ fac_1, example_train)
  yy <- forge(example_train, y$blueprint, outcomes = TRUE)

  expect_equal(
    colnames(yy$outcomes),
    c(
      "log(num_2)",
      "poly(num_2, degree = 2).1",
      "poly(num_2, degree = 2).2"
    )
  )

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

  expect_is(
    xx$predictors$fac_1,
    "factor"
  )

})

test_that("forging with `indicators = 'none'` works with numeric interactions", {
  x <- mold(
    fac_1 ~ num_2:num_1,
    example_train,
    blueprint = default_formula_blueprint(indicators = "none")
  )

  xx <- forge(example_train, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "num_2:num_1"
  )

})

test_that("asking for the outcome works", {
  x <- mold(fac_1 ~ num_1, example_train)
  xx <- forge(example_train, x$blueprint, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    tibble::tibble(fac_1 = example_train$fac_1)
  )
})

test_that("asking for the outcome when it isn't there fails", {
  x <- mold(fac_1 ~ num_1, example_train)
  example_train2 <- example_train
  example_train2$fac_1 <- NULL

  expect_error(
    forge(example_train2, x$blueprint, outcomes = TRUE),
    "The following required columns"
  )
})

test_that("can use special inline functions", {
  x <- mold(log(num_1) ~ poly(num_1, degree = 2), example_train)
  xx <- forge(example_train, x$blueprint, outcomes = TRUE)

  # manually create poly df
  x_poly <- stats::poly(example_train$num_1, degree = 2)
  poly_df <- tibble::tibble(
    `poly(num_1, degree = 2)1` = x_poly[,1],
    `poly(num_1, degree = 2)2` = x_poly[,2]
  )

  # coerce to df for tolerance..tibbles don't have good tolerance
  expect_equal(
    as.data.frame(xx$predictors),
    as.data.frame(poly_df)
  )

  expect_equal(
    xx$outcomes,
    tibble::tibble(`log(num_1)` = log(example_train$num_1))
  )

})

test_that("new_data can be a matrix", {
  x <- mold(fac_1 ~ num_1, example_train)
  example_train_mat <- as.matrix(example_train[,"num_1", drop = FALSE])

  expect_error(
    xx <- forge(example_train_mat, x$blueprint),
    NA
  )

  sep_len <- example_train$num_1
  pred_tbl <- tibble::tibble(num_1 = sep_len)

  expect_equal(
    xx$predictors,
    pred_tbl
  )

})

test_that("new_data can only be a data frame / matrix", {
  x <- mold(fac_1 ~ num_1, example_train)

  expect_error(
    forge("hi", x$blueprint),
    "The class of `new_data`, 'character'"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(fac_1 ~ num_1 + num_2, example_train)

  expect_error(
    forge(example_train[,1, drop = FALSE], x$blueprint),
    "num_2"
  )

  expect_error(
    forge(example_train[,3, drop = FALSE], x$blueprint),
    "'num_1', 'num_2'"
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

  x <- mold(y ~ f, dat)

  expect_warning(
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$predictors[[5,1]],
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

  blueprint <- default_formula_blueprint(allow_novel_levels = TRUE)

  x <- mold(y ~ f, dat, blueprint = blueprint)

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

  expect_equal(
    xx$predictors[[5,5]],
    1
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

  x <- mold(y ~ f, dat)

  expect_silent(
    xx <- forge(new, x$blueprint)
  )

  expect_equal(
    colnames(xx$predictors),
    colnames(x$predictors)
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
  new <- new[1:4,]

  blueprint <- default_formula_blueprint(allow_novel_levels = TRUE)

  x <- mold(y ~ f, dat, blueprint = blueprint)

  expect_silent(
    xx <- forge(new, x$blueprint)
  )

  expect_equal(
    colnames(xx$predictors),
    c(colnames(x$predictors), "fe")
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

  x <- mold(y ~ f, dat)

  expect_warning(
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$predictors[[5,1]],
    NA_real_
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4,]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    xx <- forge(new2, x$blueprint)
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

  x <- mold(y ~ f, dat)

  expect_error(
    forge(new, x$blueprint),
    class = "vctrs_error_cast_lossy"
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4,]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    xx <- forge(new2, x$blueprint)
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

  blueprint <- default_formula_blueprint(allow_novel_levels = TRUE)

  x <- mold(y ~ f, dat, blueprint = blueprint)

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

  expect_equal(
    xx$predictors[[5,5]],
    1
  )

  # The 'e' level exists, but there is no data for it!
  new2 <- new[1:4,]

  # silently coerces to factor, and silently removes novel level
  expect_silent(
    yy <- forge(new2, x$blueprint)
  )

  expect_equal(
    colnames(yy$predictors),
    colnames(xx$predictors)[-5L]
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

  x <- mold(f ~ y, dat)

  expect_warning(
    xx <- forge(new, x$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$outcomes[[5,1]],
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

  blueprint <- default_formula_blueprint(allow_novel_levels = TRUE)

  x <- mold(f ~ y, dat, blueprint = blueprint)

  expect_warning(
    xx <- forge(new, x$blueprint, outcomes = TRUE),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    xx$outcomes[[5,1]],
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

  x <- mold(y ~ f, dat)

  expect_warning(
    x_new <- forge(new, x$blueprint),
    NA
  )

  expect_equal(
    colnames(x_new$predictors),
    c("fa", "fb", "fc", "fd")
  )

  expect_warning(
    x_new2 <- forge(new2, x$blueprint),
    NA
  )

  expect_equal(
    colnames(x_new2$predictors),
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

  x <- mold(y ~ f, dat, blueprint = default_formula_blueprint(indicators = "none"))

  # Warning for the extra level
  expect_warning(
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

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

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

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

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

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
  expect_error(
    x_example_train2 <- forge(example_train2, x$blueprint),
    NA
  )

  expect_is(
    x_example_train2$predictors$fac_1,
    "factor"
  )

  xx <- mold(fac_1 ~ num_1, example_train)

  expect_error(
    xx_example_train2 <- forge(example_train2, xx$blueprint, outcomes = TRUE),
    NA
  )

  expect_is(
    xx_example_train2$outcomes$fac_1,
    "factor"
  )

})

test_that("new data classes can interchange integer/numeric", {

  example_train2 <- example_train
  example_train2$num_1 <- as.integer(example_train2$num_1)

  x <- mold(fac_1 ~ num_1, example_train)

  expect_error(
    forge(example_train2, x$blueprint),
    NA
  )

  xx <- mold(num_1 ~ fac_1, example_train)

  expect_error(
    forge(example_train2, xx$blueprint, outcomes = TRUE),
    NA
  )

})

test_that("intercepts can still be forged on when not using indicators (i.e. model.matrix())", {

  x <- mold(num_2 ~ fac_1, example_train, blueprint = default_formula_blueprint(intercept = TRUE, indicators = "none"))
  xx <- forge(example_train, x$blueprint)

  expect_true(
    "(Intercept)" %in% colnames(xx$predictors)
  )

  expect_is(
    xx$predictors$fac_1,
    "factor"
  )

})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  x <- mold(~ num_2, example_train)
  expect_equal(forge(example_train, x$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  x <- mold(~ num_2, example_train)

  forged <- forge(example_train, x$blueprint, outcomes = TRUE)
  outcomes <- forged$outcomes

  expect_equal(nrow(outcomes), 12)
  expect_equal(ncol(outcomes), 0)
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

  res1 <- mold(y ~ x + z, df, blueprint = bp1)
  res2 <- mold(y ~ x + z, df, blueprint = bp2)

  x1 <- forge(df, res1$blueprint)
  x2 <- forge(df, res2$blueprint)

  expect_identical(
    colnames(x1$predictors),
    c("xa", "xb", "zd")
  )

  expect_identical(
    colnames(x2$predictors),
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

  res1 <- mold(y ~ x + z, df, blueprint = bp1)
  res2 <- mold(y ~ x + z, df, blueprint = bp2)
  res3 <- mold(y ~ x + z, df, blueprint = bp3)

  x1 <- forge(new_df, res1$blueprint)
  x2 <- forge(new_df, res2$blueprint)
  x3 <- forge(new_df, res3$blueprint)

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
    colnames(x3$predictors),
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

  bp <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "traditional"
  )

  res <- mold(x ~ y + z, df, blueprint = bp)
  x <- forge(df, res$blueprint)

  expect_identical(
    names(x$predictors),
    c("ya", "yb", "yc", "zB")
  )
})

test_that("traditional encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "traditional"
  )

  res <- mold(x ~ y + z, df, blueprint = bp)
  x <- forge(df, res$blueprint)

  expect_identical(
    names(x$predictors),
    c("(Intercept)", "yb", "yc", "zB")
  )
})

test_that("one-hot encoding and no intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp <- default_formula_blueprint(
    intercept = FALSE,
    indicators = "one_hot"
  )

  res <- mold(x ~ y + z, df, blueprint = bp)
  x <- forge(df, res$blueprint)

  expect_identical(
    names(x$predictors),
    c("ya", "yb", "yc", "zA", "zB")
  )
})

test_that("one-hot encoding and intercept", {
  df <- data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

  bp <- default_formula_blueprint(
    intercept = TRUE,
    indicators = "one_hot"
  )

  res <- mold(x ~ y + z, df, blueprint = bp)
  x <- forge(df, res$blueprint)

  expect_identical(
    names(x$predictors),
    c("(Intercept)", "ya", "yb", "yc", "zA", "zB")
  )
})
