context("test-mold-formulas")

test_that("can mold simple formulas", {

  x <- mold(Species ~ Sepal.Length, iris)

  expect_equal(colnames(x$predictors), "Sepal.Length")
  expect_is(x$outcomes, "tbl_df")
  expect_equal(colnames(x$outcomes), "Species")
  expect_is(x$blueprint, "default_formula_blueprint")
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

  x <- mold(Sepal.Length ~ Species, iris, blueprint = default_formula_blueprint(intercept = TRUE))
  xx <- mold(Sepal.Length ~ Species, iris, blueprint = default_formula_blueprint(intercept = FALSE))

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

  x <- mold(Sepal.Length ~ Species, iris, blueprint = default_formula_blueprint(indicators = FALSE))

  expect_equal(colnames(x$predictors), "Species")
  expect_is(x$predictors$Species, "factor")
  expect_equal(x$blueprint$indicators, FALSE)
})

test_that("errors are thrown if `indicator = FALSE` and factor interactions exist", {

  expect_error(
    mold(~ Species, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    NA
  )

  expect_error(
    mold(Sepal.Length ~ Species:Sepal.Width, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    "Interaction terms involving factors"
  )

  # Checking various types of generated interactions

  expect_error(
    mold(Sepal.Length ~ Species:Sepal.Width, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ Species * Sepal.Width, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ (Species + Sepal.Width) ^ 2, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    "'Species'"
  )

  expect_error(
    mold(Sepal.Length ~ Species %in% Sepal.Width, iris, blueprint = default_formula_blueprint(indicators = FALSE)),
    "'Species'"
  )

  # Both factor issues are reported

  iris2 <- iris
  iris2$Species2 <- iris2$Species

  expect_error(
    mold(~ Species:Species2, iris2, blueprint = default_formula_blueprint(indicators = FALSE)),
    "'Species', 'Species2'."
  )

})

test_that("errors are thrown if `indicator = FALSE` and factors are used in inline functions", {

  blueprint_no_indicators <- default_formula_blueprint(indicators = FALSE)

  expect_error(
    mold(~ paste0(Species), iris, blueprint = blueprint_no_indicators),
    "Functions involving factors"
  )

  expect_error(
    mold(~ paste0(Species), iris, blueprint = blueprint_no_indicators),
    "'Species'"
  )

  expect_error(
    mold(~ Species %>% paste0(), iris, blueprint = blueprint_no_indicators)
  )

  expect_error(
    mold(~ paste0(Species + Species), iris, blueprint = blueprint_no_indicators),
    "'Species'"
  )

  iris2 <- iris
  iris2$Species2 <- iris2$Species

  expect_error(
    mold(~ paste0(Species) + paste0(Species2), iris2, blueprint = blueprint_no_indicators),
    "'Species', 'Species2'."
  )

})

test_that("`indicators = FALSE` works fine in strange formulas", {

  x <- mold(~ NULL, iris, blueprint = default_formula_blueprint(indicators = FALSE, intercept = TRUE))

  expect_equal(
    colnames(x$predictors),
    "(Intercept)"
  )

})

test_that("formula intercepts can be added", {

  x <- mold(
    Species ~ Sepal.Length,
    iris,
    blueprint = default_formula_blueprint(intercept = TRUE)
  )

  expect_true("(Intercept)" %in% colnames(x$predictors))
  expect_equal(attr(x$blueprint$terms$predictors, "intercept"), 1)

  # Don't want intercept in original predictors
  expect_false("(Intercept)" %in% colnames(x$blueprint$ptypes$predictors))
})

test_that("can mold formulas with special terms", {

  x <- mold(Species ~ Sepal.Length:Sepal.Width + I(Sepal.Length^2), iris)
  y <- mold(Species ~ poly(Sepal.Length, degree = 2), iris)

  expect_equal(
    colnames(x$predictors),
    c("I(Sepal.Length^2)", "Sepal.Length:Sepal.Width")
  )

  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("formulas with non-existent columns are caught", {

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
    mold(Species ~ 0 + y, iris),
    "`formula` must not contain"
  )

  expect_error(
    mold(Species ~ y - 1, iris),
    "`formula` must not contain"
  )

})

test_that("RHS with _only_ intercept related terms are caught", {

  expect_error(
    mold(~ 0, iris),
    "`formula` must not contain the intercept removal term"
  )

  expect_error(
    mold(~ 1, iris),
    "`formula` must not contain the intercept term"
  )

  expect_error(
    mold(~ -1, iris),
    "`formula` must not contain the intercept removal term"
  )

})

test_that("`NULL` can be used to represent empty RHS formulas", {

  expect_error(
    x <- mold(~NULL, iris),
    NA
  )

  expect_equal(nrow(x$predictors), 150)
  expect_equal(nrow(x$outcomes), 150)

  expect_error(
    x2 <- mold(~NULL, iris, blueprint = default_formula_blueprint(intercept = TRUE)),
    NA
  )

  expect_equal(colnames(x2$predictors), "(Intercept)")

})

test_that("intercepts can still be added when not using indicators (i.e. model.matrix())", {

  x <- mold(Sepal.Width ~ Species, iris, blueprint = default_formula_blueprint(intercept = TRUE, indicators = FALSE))

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

  x <- mold(~ Sepal.Length:Sepal.Width, iris, blueprint = default_formula_blueprint(indicators = FALSE))

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
    get_data_classes(x$blueprint$ptypes$predictors),
    list(Sepal.Width = "numeric")
  )

  expect_equal(
    get_data_classes(x$blueprint$ptypes$outcomes),
    list(Sepal.Length = "numeric")
  )

})

test_that("`.` notation works as expected", {

  x <- mold(Species ~ ., iris)

  # no Species columns in predictors
  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  # species is the outcome
  expect_equal(
    colnames(x$blueprint$ptypes$outcomes),
    "Species"
  )

})

test_that("`.` notation fails on the LHS", {

  # Get this for free from base R
  # get_all_vars() does not like `.` on the LHS
  expect_error(
    mold(. ~ Species, iris),
    "object '.' not found"
  )

})

test_that("`.` notation with variable as predictor and outcome", {

  x <- mold(Sepal.Width ~ . + Sepal.Width, iris)

  # Sepal.Width IS a predictor
  expect_true(
    "Sepal.Width" %in% colnames(x$blueprint$ptypes$predictors)
  )

  # Sepal.Width IS the outcome
  expect_equal(
    colnames(x$blueprint$ptypes$outcomes),
    "Sepal.Width"
  )

  xx <- mold(Sepal.Width ~ . + log(Sepal.Width), iris)

  # Sepal.Width IS a predictor
  expect_true(
    "Sepal.Width" %in% colnames(x$blueprint$ptypes$predictors)
  )

  # Sepal.Width IS the outcome
  expect_equal(
    colnames(x$blueprint$ptypes$outcomes),
    "Sepal.Width"
  )

})

test_that("`.` notation with no outcome works fine", {

  # Uses all columns of iris
  x <- mold(~ ., iris)

  # Sepal.Length IS a predictor
  expect_equal(
    ncol(x$predictors),
    7
  )

  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

})

test_that("`-var` still registers var as a predictor", {

  # This is expected, and is the same as base R
  x <- mold(Sepal.Width ~ . - Sepal.Length, iris)

  # Sepal.Length IS a predictor
  expect_true(
    "Sepal.Length" %in% colnames(x$blueprint$ptypes$predictors)
  )

})

test_that("Missing y value returns a 0 column tibble for `outcomes`", {
  x1 <- mold(~ Sepal.Width, iris)
  x2 <- mold(NULL ~ Sepal.Width, iris)

  outcomes1 <- x1$outcomes
  outcomes2 <- x2$outcomes

  expect_equal(nrow(outcomes1), 150)
  expect_equal(ncol(outcomes1), 0)

  expect_equal(nrow(outcomes2), 150)
  expect_equal(ncol(outcomes2), 0)
})

test_that("Missing y value returns a 0 column / 0 row tibble for `ptype`", {
  x <- mold(~ Sepal.Width, iris)
  expect_equal(x$blueprint$ptypes$outcomes, tibble())
})

test_that("Missing y value still has outcome `terms` present", {
  x <- mold(~ Sepal.Width, iris)

  expect_equal(
    rlang::f_rhs(x$blueprint$terms$outcomes),
    rlang::expr(NULL + 0)
  )
})
