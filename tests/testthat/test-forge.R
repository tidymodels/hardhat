# ------------------------------------------------------------------------------
context("test-forge-formula")

test_that("simple forge works", {
  x <- mold(Species ~ Sepal.Length, iris)
  xx <- forge(x$preprocessor, iris)

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
  xx <- forge(x$preprocessor, iris, outcome = TRUE)

  expect_is(xx$outcomes, "tbl_df")
  expect_equal(colnames(xx$outcomes), c("Sepal.Length", "Sepal.Width"))

  y <- mold(log(Sepal.Width) + poly(Sepal.Width, degree = 2) ~ Species, iris)
  yy <- forge(y$preprocessor, iris, outcome = TRUE)

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

  x <- mold(Sepal.Length ~ Species, iris, indicators = FALSE)
  xx <- forge(x$preprocessor, iris)

  expect_equal(
    colnames(xx$predictors),
    "Species"
  )

  expect_is(
    xx$predictors$Species,
    "factor"
  )

})

test_that("asking for the outcome works", {
  x <- mold(Species ~ Sepal.Length, iris)
  xx <- forge(x$preprocessor, iris, outcome = TRUE)

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
    forge(x$preprocessor, iris2, outcome = TRUE),
    "`new_data` is missing the following"
  )
})

test_that("can use special inline functions", {
  x <- mold(log(Sepal.Length) ~ poly(Sepal.Length, degree = 2), iris)
  xx <- forge(x$preprocessor, iris, outcome = TRUE)

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
    xx <- forge(x$preprocessor, iris_mat),
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
    forge(x$preprocessor, "hi"),
    "new_data should be a"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(Species ~ Sepal.Length + Sepal.Width, iris)

  expect_error(
    forge(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    forge(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
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
    xx <- forge(x$preprocessor, new),
    "The following factor levels"
  )

  expect_equal(
    xx$predictors[[5,1]],
    NA_real_
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

  x <- mold(y ~ f, dat, indicators = FALSE)

  expect_warning(
    xx <- forge(x$preprocessor, new),
    "The following factor levels"
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
    xx <- forge(x$preprocessor, new, outcome = TRUE),
    "The following factor levels"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )

})

test_that("missing predictor levels are added", {

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
    forge(x$preprocessor, new),
    glue::glue(
      "The following original factor levels are missing for column, ",
      "'f', and have been restored: 'd'."
    )
  )

  expect_warning(
    forge(x$preprocessor, new2),
    glue::glue(
      "The following original factor levels are missing for column, ",
      "'f', and have been restored: 'c', 'd'."
    )
  )

})

test_that("missing ordered factor levels are handled correctly", {

  dat <- data.frame(
    y = 1:4,
    f = ordered(letters[1:4])
  )

  # Ordered - strictly wrong order
  new <- data.frame(
    y = 1:2,
    f = ordered(letters[1:4], levels = rev(letters[1:4]))
  )

  # Ordered - missing levels
  new2 <- data.frame(
    y = 1:3,
    f = ordered(letters[1:3], levels = rev(letters[1:3]))
  )

  x <- mold(y ~ f, dat)

  expect_warning(
    forge(x$preprocessor, new),
    glue::glue(
      "Column, 'f', is an ordered factor, ",
      "but the levels in `new_data` are misordered and have been reordered."
    )
  )

  expect_warning(
    forge(x$preprocessor, new2),
    glue::glue(
      "The following original factor levels are missing for column, ",
      "'f', and have been restored: 'd'."
    )
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

  x <- mold(y ~ f, dat)

  expect_warning(
    xx <- forge(x$preprocessor, new),
    "The following factor levels are new for column"
  )

  expect_warning(
    xx <- forge(x$preprocessor, new),
    "The following original factor levels are missing"
  )

})

test_that("original predictor and outcome classes are recorded", {

  x <- mold(log(Sepal.Length) ~ log(Sepal.Width), iris)

  expect_equal(
    x$preprocessor$predictor_classes,
    list(Sepal.Width = "numeric")
  )

  expect_equal(
    x$preprocessor$outcome_classes,
    list(Sepal.Length = "numeric")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(Sepal.Length ~ Species, iris)

  expect_error(
    forge(x$preprocessor, iris2),
    "`Species`: `character` should be `factor`"
  )

  xx <- mold(Species ~ Sepal.Length, iris)

  expect_error(
    forge(xx$preprocessor, iris2),
    NA
  )

  expect_error(
    forge(xx$preprocessor, iris2, outcome = TRUE),
    "`Species`: `character` should be `factor`"
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(Species ~ Sepal.Length, iris)

  expect_error(
    forge(x$preprocessor, iris2),
    NA
  )

  xx <- mold(Sepal.Length ~ Species, iris)

  expect_error(
    forge(xx$preprocessor, iris2, outcome = TRUE),
    NA
  )

})

# ------------------------------------------------------------------------------
context("test-forge-xy")

test_that("simple forge works", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  xx <- forge(x$preprocessor, iris)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("asking for the outcome fails for default preprocessor", {
  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(x$preprocessor, iris, outcome = TRUE),
    "`outcome` cannot be specified"
  )
})

test_that("new_data can be a matrix", {

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)
  iris_mat <- as.matrix(iris[,"Sepal.Length", drop = FALSE])

  expect_error(
    xx <- forge(x$preprocessor, iris_mat),
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
  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(x$preprocessor, "hi"),
    "new_data should be a"
  )

})

test_that("missing predictor columns fail appropriately", {
  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_error(
    forge(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    forge(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
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

  expect_warning(
    xx <- forge(x$preprocessor, new),
    "The following factor levels"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, c("a", "b", "c", "d"))
  )

})

# cannot have outcome = TRUE for xy method, so no need to do a check
# for novel outcome levels being caught here

test_that("original predictor and outcome classes are recorded", {

  x <- mold(iris[, c("Sepal.Length", "Sepal.Width"), drop = FALSE], iris$Species)

  expect_equal(
    x$preprocessor$predictor_classes,
    list(Sepal.Length = "numeric", Sepal.Width = "numeric")
  )

  expect_equal(
    x$preprocessor$outcome_classes,
    list(.outcome = "factor")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(iris[, "Species", drop = FALSE], iris$Sepal.Length)

  expect_error(
    forge(x$preprocessor, iris2),
    "`Species`: `character` should be `factor`"
  )

  xx <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(xx$preprocessor, iris2),
    NA
  )

  # Can't do outcome = TRUE so can't check outcome class here

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(iris[, "Sepal.Length", drop = FALSE], iris$Species)

  expect_error(
    forge(x$preprocessor, iris2),
    NA
  )

})

# ------------------------------------------------------------------------------
context("test-forge-recipe")

library(recipes)

test_that("simple forge works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(x$preprocessor, iris)

  expect_equal(
    colnames(xx$predictors),
    "Sepal.Length"
  )

  expect_equal(
    xx$outcomes,
    NULL
  )
})

test_that("asking for the outcome works", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  xx <- forge(x$preprocessor, iris, outcome = TRUE)

  expect_equal(
    xx$outcomes,
    data.frame(Species = iris$Species)
  )
})

test_that("asking for the outcome when it isn't there fails", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  iris2 <- iris
  iris2$Species <- NULL

  expect_error(
    forge(x$preprocessor, iris2, outcome = TRUE),
    "`new_data` is missing the following"
  )

})

test_that("outcomes steps get processed", {

  x <- mold(
    recipe(Sepal.Width ~ Sepal.Length, data = iris) %>%
      step_log(Sepal.Width),
    iris
  )

  processed <- forge(x$preprocessor, iris, outcome = TRUE)

  expect_equal(
    processed$outcomes$Sepal.Width,
    log(iris$Sepal.Width)
  )

})

test_that("missing predictor columns fail appropriately", {

  x <- mold(
    recipe(Species ~ Sepal.Length + Sepal.Width, data = iris),
    iris
  )

  expect_error(
    forge(x$preprocessor, iris[,1, drop = FALSE]),
    "Sepal.Width"
  )

  expect_error(
    forge(x$preprocessor, iris[,3, drop = FALSE]),
    "Sepal.Length, and Sepal.Width"
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

  x <- mold(recipe(y ~ f, dat), dat)

  expect_warning(
    xx <- forge(x$preprocessor, new),
    "The following factor levels"
  )

  expect_equal(
    xx$predictors[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
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

  x <- mold(recipe(f ~ y, dat), dat)

  expect_warning(
    xx <- forge(x$preprocessor, new, outcome = TRUE),
    "The following factor levels"
  )

  expect_equal(
    xx$outcomes[[5,1]],
    factor(NA_real_, levels = c("a", "b", "c", "d"))
  )

})

test_that("original predictor and outcome classes are recorded", {

  x <- mold(
    recipe(Species ~ Sepal.Length, data = iris),
    iris
  )

  expect_equal(
    x$preprocessor$predictor_classes,
    list(Sepal.Length = "numeric")
  )

  expect_equal(
    x$preprocessor$outcome_classes,
    list(Species = "factor")
  )

})

test_that("new data classes are caught", {

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  x <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  expect_error(
    forge(x$preprocessor, iris2),
    "`Species`: `character` should be `factor`"
  )

  xx <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    forge(xx$preprocessor, iris2),
    NA
  )

  expect_error(
    forge(xx$preprocessor, iris2, outcome = TRUE),
    "`Species`: `character` should be `factor`"
  )

})

test_that("new data classes can interchange integer/numeric", {

  iris2 <- iris
  iris2$Sepal.Length <- as.integer(iris2$Sepal.Length)

  x <- mold(recipe(Species ~ Sepal.Length, iris), iris)

  expect_error(
    forge(x$preprocessor, iris2),
    NA
  )

  xx <- mold(recipe(Sepal.Length ~ Species, iris), iris)

  expect_error(
    forge(xx$preprocessor, iris2, outcome = TRUE),
    NA
  )

})
