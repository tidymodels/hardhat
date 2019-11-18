context("test-forge-formula")

test_that("simple forge works", {
  x <- mold(Species ~ Sepal.Length, iris)
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

test_that("can forge multivariate formulas", {

  x <- mold(Sepal.Length + Sepal.Width ~ Petal.Length, iris)
  xx <- forge(iris, x$blueprint, outcomes = TRUE)

  expect_is(xx$outcomes, "tbl_df")
  expect_equal(colnames(xx$outcomes), c("Sepal.Length", "Sepal.Width"))

  y <- mold(log(Sepal.Width) + poly(Sepal.Width, degree = 2) ~ Species, iris)
  yy <- forge(iris, y$blueprint, outcomes = TRUE)

  expect_equal(
    colnames(yy$outcomes),
    c(
      "log(Sepal.Width)",
      "poly(Sepal.Width, degree = 2).1",
      "poly(Sepal.Width, degree = 2).2"
    )
  )

})

test_that("can forge new data without expanding factors into dummies", {

  x <- mold(Sepal.Length ~ Species, iris, blueprint = default_formula_blueprint(indicators = FALSE))
  xx <- forge(iris, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "Species"
  )

  expect_is(
    xx$predictors$Species,
    "factor"
  )

})

test_that("forging with `indicators = FALSE` works with numeric interactions", {

  x <- mold(Species ~ Sepal.Width:Sepal.Length, iris, blueprint = default_formula_blueprint(indicators = FALSE))
  xx <- forge(iris, x$blueprint)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Width:Sepal.Length"
  )

})

test_that("asking for the outcome works", {
  x <- mold(Species ~ Sepal.Length, iris)
  xx <- forge(iris, x$blueprint, outcomes = TRUE)

  expect_equal(
    xx$outcomes,
    tibble::tibble(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {
  x <- mold(Species ~ Sepal.Length, iris)
  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    forge(iris2, x$blueprint, outcomes = TRUE),
    "The following required columns"
  )
})

test_that("can use special inline functions", {
  x <- mold(log(Sepal.Length) ~ poly(Sepal.Length, degree = 2), iris)
  xx <- forge(iris, x$blueprint, outcomes = TRUE)

  # manually create poly df
  x_poly <- stats::poly(iris$Sepal.Length, degree = 2)
  poly_df <- tibble::tibble(
    `poly(Sepal.Length, degree = 2)1` = x_poly[,1],
    `poly(Sepal.Length, degree = 2)2` = x_poly[,2]
  )

  # coerce to df for tolerance..tibbles don't have good tolerance
  expect_equal(
    as.data.frame(xx$predictors),
    as.data.frame(poly_df)
  )

  expect_equal(
    xx$outcomes,
    tibble::tibble(`log(Sepal.Length)` = log(iris$Sepal.Length))
  )

})

test_that("new_data can be a matrix", {
  x <- mold(Species ~ Sepal.Length, iris)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- forge(iris_mat, x$blueprint),
    NA
  )

  sep_len <- iris$Sepal.Length
  pred_tbl <- tibble::tibble(Sepal.Length = sep_len)

  expect_equal(
    xx$predictors,
    pred_tbl
  )

})

test_that("new_data can only be a data frame / matrix", {
  x <- mold(Species ~ Sepal.Length, iris)

  expect_error(
    forge("hi", x$blueprint),
    "The class of `new_data`, 'character'"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(Species ~ Sepal.Length + Sepal.Width, iris)

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

test_that("novel ordered factor predictor levels have order maintained", {

  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  new <- data.frame(
    y = 1:5,
    f = ordered(letters[c(1:2, 5, 3:4)], levels = letters[c(1:2, 5, 3:4)])
  )

  x <- mold(y ~ f, dat, blueprint = default_formula_blueprint(indicators = FALSE))

  expect_warning(
    xx <- forge(new, x$blueprint),
    "Novel levels found in column 'f': 'e'"
  )

  expect_equal(
    levels(xx$predictors$f),
    levels(dat$f)
  )

  expect_is(
    xx$predictors$f,
    "ordered"
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

test_that("missing ordered factor levels are handled correctly", {

  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  x <- mold(y ~ f, dat, blueprint = default_formula_blueprint(indicators = FALSE))

  # Ordered - strictly wrong order
  # Silently recover order!
  new <- data.frame(
    y = 1:2,
    f = ordered(letters[1:4], levels = rev(letters[1:4]))
  )

  expect_warning(
    xx <- forge(new, x$blueprint),
    NA
  )

  # Order is restored
  expect_equal(
    levels(xx$predictors$f),
    letters[1:4]
  )

  # Ordered - missing levels
  new2 <- data.frame(
    y = 1:3,
    f = ordered(letters[1:3], levels = rev(letters[1:3]))
  )

  # Silently recover missing levels in the right order
  expect_warning(
    xx2 <- forge(new2, x$blueprint),
    NA
  )

  expect_equal(
    levels(xx2$predictors$f),
    letters[1:4]
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

  x <- mold(y ~ f, dat, blueprint = default_formula_blueprint(indicators = FALSE))

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

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(Sepal.Length ~ Species, iris, blueprint = default_formula_blueprint(indicators = FALSE))

  # Silently recover character -> factor
  expect_error(
    x_iris2 <- forge(iris2, x$blueprint),
    NA
  )

  expect_is(
    x_iris2$predictors$Species,
    "factor"
  )

  xx <- mold(Species ~ Sepal.Length, iris)

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

  x <- mold(Species ~ Sepal.Length, iris)

  expect_error(
    forge(iris2, x$blueprint),
    NA
  )

  xx <- mold(Sepal.Length ~ Species, iris)

  expect_error(
    forge(iris2, xx$blueprint, outcomes = TRUE),
    NA
  )

})

test_that("intercepts can still be forged on when not using indicators (i.e. model.matrix())", {

  x <- mold(Sepal.Width ~ Species, iris, blueprint = default_formula_blueprint(intercept = TRUE, indicators = FALSE))
  xx <- forge(iris, x$blueprint)

  expect_true(
    "(Intercept)" %in% colnames(xx$predictors)
  )

  expect_is(
    xx$predictors$Species,
    "factor"
  )

})

test_that("Missing y value still returns `NULL` if no outcomes are asked for", {
  x <- mold(~ Sepal.Width, iris)
  expect_equal(forge(iris, x$blueprint)$outcomes, NULL)
})

test_that("Missing y value returns 0 column tibble if outcomes are asked for", {
  x <- mold(~ Sepal.Width, iris)

  forged <- forge(iris, x$blueprint, outcomes = TRUE)
  outcomes <- forged$outcomes

  expect_equal(nrow(outcomes), 150)
  expect_equal(ncol(outcomes), 0)
})
