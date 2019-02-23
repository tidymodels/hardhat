context("test-mold-formulas")

test_that("can mold simple formulas", {

  x <- mold(Species ~ Sepal.Length, iris)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), "Species")
  expect_is(x$engine, "default_formula_engine")
})

test_that("can mold multivariate formulas", {

  x <- mold(Sepal.Length + Sepal.Width ~ Petal.Length, iris)

  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), c("Sepal.Length", "Sepal.Width"))

  xx <- mold(log(Sepal.Width) + poly(Sepal.Width, degree = 2) ~ Species, iris)

  expect_equal(
    colnames(xx$outcomes),
    c(
      "log(Sepal.Width)",
      "poly(Sepal.Width, degree = 2).1",
      "poly(Sepal.Width, degree = 2).2"
    )
  )

})

test_that("factor predictors with no intercept are fully expanded", {

  x <- mold(Sepal.Length ~ Species, iris, intercept = TRUE)
  xx <- mold(Sepal.Length ~ Species, iris, intercept = FALSE)

  expect_equal(
    colnames(x$predictors),
    c("(Intercept)", "Speciesversicolor", "Speciesvirginica")
  )

  expect_equal(
    colnames(xx$predictors),
    c("Speciessetosa", "Speciesversicolor", "Speciesvirginica")
  )

})

test_that("can mold and not expand dummies", {

  x <- mold(Sepal.Length ~ Species, iris, indicators = FALSE)

  expect_equal(colnames(x$predictors), "Species")
  expect_is(x$predictors$Species, "factor")
  expect_equal(x$engine$indicators, FALSE)
})

test_that("errors are thrown if `indicator = FALSE` and factor interactions exist", {

  expect_error(
    mold(~ Species, iris, indicators = FALSE),
    NA
  )

  expect_error(
    mold(Sepal.Length ~ Species:Sepal.Width, iris, indicators = FALSE),
    "Interaction terms involving factors"
  )

  # Checking various types of generated interactions

  expect_error(
    mold(Sepal.Length ~ Species:Sepal.Width, iris, indicators = FALSE),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ Species * Sepal.Width, iris, indicators = FALSE),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ (Species + Sepal.Width) ^ 2, iris, indicators = FALSE),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ Species %in% Sepal.Width, iris, indicators = FALSE),
    "'Species'"
  )

  # Both factor issues are reported

  iris2 <- iris
  iris2$Species2 <- iris2$Species

  expect_error(
    mold(~ Species:Species2, iris2, indicators = FALSE),
    "'Species', 'Species2'."
  )

})

test_that("errors are thrown if `indicator = FALSE` and factors are used in inline functions", {

  expect_error(
    mold(~ paste0(Species), iris, indicators = FALSE),
    "Functions involving factors"
  )

  expect_error(
    mold(~ paste0(Species), iris, indicators = FALSE),
    "'Species'"
  )

  expect_error(
    mold(~ Species %>% paste0(), iris, indicators = FALSE)
  )

  expect_error(
    mold(~ paste0(Species + Species), iris, indicators = FALSE),
    "'Species'"
  )

  iris2 <- iris
  iris2$Species2 <- iris2$Species

  expect_error(
    mold(~ paste0(Species) + paste0(Species2), iris2, indicators = FALSE),
    "'Species', 'Species2'."
  )

})

test_that("`indicators = FALSE` works fine in strange formulas", {

  x <- mold(~ 1, iris, indicators = FALSE, intercept = TRUE)

  expect_equal(
    colnames(x$predictors),
    "(Intercept)"
  )

})

test_that("formula intercepts can be added", {

  x <- mold(
    Species ~ Sepal.Length,
    iris,
    intercept = TRUE
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
  expect_equal(attr(x$engine$terms$predictors, "intercept"), 1)

  # Don't want intercept in original predictors
  expect_false("(Intercept)" %in% x$engine$info$predictors$names)
})

test_that("can mold formulas with special terms", {

  x <- mold(Species ~ Sepal.Length:Sepal.Width + I(Sepal.Length^2), iris)
  y <- mold(Species ~ poly(Sepal.Length, degree = 2), iris)

  expect_equal(
    colnames(x$predictors),
    c("I(Sepal.Length^2)", "Sepal.Length:Sepal.Width")
  )

  expect_equal(
    x$engine$info$predictors$names,
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("formulas with non-existant columns are caught", {

  expect_error(
    mold(Species ~ y, iris),
    "object 'y' not found"
  )

})

test_that("global environment variables cannot be used", {

  expect_error(
    {
      y <- 1
      mold(Species ~ y, iris)
    },
    "object 'y' not found"
  )

})

test_that("cannot manually remove intercept in the formula itself", {

  expect_error(
    mold(Species ~ y + 0, iris),
    "`formula` must not contain"
  )

  expect_error(
    mold(Species ~ y - 1, iris),
    "`formula` must not contain"
  )

})

test_that("intercepts can still be added when not using indicators (i.e. model.matrix())", {

  x <- mold(Sepal.Width ~ Species, iris, intercept = TRUE, indicators = FALSE)

  expect_true(
    "(Intercept)" %in% colnames(x$predictors)
  )

  expect_is(
    x$predictors$Species,
    "factor"
  )

})

test_that("`data` is validated", {

  expect_error(
    mold(Species ~ Sepal.Width, 1),
    "`data` must be a data.frame or a matrix"
  )

})

test_that("full interaction syntax is supported", {

  expect_equal(
    mold(~ Species*Sepal.Width, iris)$predictors,
    mold(~ Species + Sepal.Width + Species:Sepal.Width, iris)$predictors
  )

  expect_equal(
    mold(~ Species*Sepal.Width - Species:Sepal.Width, iris)$predictors,
    mold(~ Species + Sepal.Width, iris)$predictors
  )

  expect_equal(
    mold(~ (Sepal.Width + Sepal.Length + Petal.Length) ^ 2, iris)$predictors,
    mold(~ Sepal.Width +
           Sepal.Length +
           Petal.Length +
           Sepal.Width:Sepal.Length +
           Sepal.Width:Petal.Length +
           Sepal.Length:Petal.Length,
         iris)$predictors
  )

  expect_equal(
    mold(~ Sepal.Width + Sepal.Length %in% Sepal.Width, iris)$predictors,
    mold(~ Sepal.Width + Sepal.Width:Sepal.Length, iris)$predictors
  )

})

test_that("`indicators = FALSE` runs numeric interactions", {

  x <- mold(~ Sepal.Length:Sepal.Width, iris, indicators = FALSE)

  expect_equal(
    colnames(x$predictors),
    "Sepal.Length:Sepal.Width"
  )

})

test_that("LHS of the formula cannot contain interactions", {

  expect_error(
    mold(Sepal.Length:Sepal.Width ~ Sepal.Width, iris),
    "The following interaction terms were found: 'Sepal.Length:Sepal.Width'"
  )

  expect_error(
    mold(Sepal.Length*Sepal.Width ~ Sepal.Width, iris),
    "The following interaction terms were found: 'Sepal.Length:Sepal.Width'"
  )

  expect_error(
    mold(Sepal.Length %in% Sepal.Width ~ Sepal.Width, iris),
    "The following interaction terms were found: 'Sepal.Length:Sepal.Width'"
  )

  expect_error(
    mold((Sepal.Length + Sepal.Width)^2 ~ Sepal.Width, iris),
    "The following interaction terms were found: 'Sepal.Length:Sepal.Width'"
  )

  expect_error(
    mold(Sepal.Length:Sepal.Width + Species:Sepal.Length ~ Sepal.Width, iris),
    "The following interaction terms were found: 'Sepal.Length:Sepal.Width', 'Sepal.Length:Species'"
  )

})

test_that("original predictor and outcome classes are recorded", {

  x <- mold(log(Sepal.Length) ~ log(Sepal.Width), iris)

  expect_equal(
    x$engine$info$predictors$classes,
    list(Sepal.Width = "numeric")
  )

  expect_equal(
    x$engine$info$outcomes$classes,
    list(Sepal.Length = "numeric")
  )

})
