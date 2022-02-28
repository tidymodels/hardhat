test_that("an offset slot is NULL when there isn't one", {
  x <- mold(Species ~ Sepal.Width, iris)

  expect_true(rlang::has_name(x$extras, "offset"))
  expect_equal(x$extras$offset, NULL)
})

test_that("the offset slot is only present for the formula method", {
  x <- mold(iris[, "Sepal.Width", drop = FALSE], iris$Species)

  expect_false(rlang::has_name(x$extras, "offset"))
})

test_that("can use offsets", {
  x <- mold(Species ~ offset(Sepal.Width), iris)

  expect_s3_class(x$extras$offset, "tbl_df")
  expect_equal(colnames(x$extras$offset), ".offset")
  expect_equal(x$extras$offset$.offset, iris$Sepal.Width)
})

test_that("(offset) is not recognized as an offset", {
  iris$`(offset)` <- iris$Sepal.Length

  mf <- model.frame(Species ~ `(offset)`, iris)

  # Base R
  expect_equal(
    model.offset(mf),
    iris$Sepal.Length
  )

  # Us
  expect_equal(
    model_offset(terms(mf), mf),
    NULL
  )
})

test_that("Only numeric columns can be offsets", {
  expect_error(
    mold(~ Sepal.Width + offset(Species), iris),
    "Column, 'offset"
  )
})

test_that("multiple offsets can be used", {
  x <- mold(Species ~ offset(Sepal.Width) + offset(Sepal.Length), iris)

  expect_equal(
    x$extras$offset$.offset,
    iris$Sepal.Width + iris$Sepal.Length
  )
})

test_that("a 0 col tibble is returned for predictors if only offsets are used", {
  x <- mold(Species ~ offset(Sepal.Length), iris)

  expect_equal(
    x$predictors,
    tibble::tibble(.rows = 150)
  )
})

test_that("intercepts and offsets can be intermingled", {
  x <- mold(Species ~ offset(Sepal.Length), iris, blueprint = default_formula_blueprint(intercept = TRUE))

  expect_equal(
    colnames(x$predictors),
    "(Intercept)"
  )
})

# This test is because model.matrix() is where the offsets are removed
# from the model.frame() result, but with indicators = "none" we don't run
# that so we have to handle it specially ourselves.
test_that("offsets columns are removed from predictors with `indicators = 'none'`", {
  x <- mold(
    Species ~ offset(Sepal.Length),
    iris,
    blueprint = default_formula_blueprint(indicators = "none")
  )

  expect_equal(
    ncol(x$predictors),
    0
  )
})

test_that("offsets are NULL in forge() result if not used", {
  x <- mold(Species ~ Sepal.Length, iris)
  xx <- forge(iris, x$blueprint)

  expect_true(rlang::has_name(xx$extras, "offset"))

  expect_equal(xx$extras$offset, NULL)
})

test_that("offsets show up in forged results", {
  x <- mold(Species ~ offset(Sepal.Length), iris)
  xx <- forge(iris, x$blueprint)

  expect_equal(
    xx$extras$offset,
    tibble::tibble(.offset = iris$Sepal.Length)
  )
})

test_that("offset columns are stored as predictors", {
  x <- mold(Species ~ offset(Sepal.Length), iris)

  expect_equal(
    colnames(x$blueprint$ptypes$predictors),
    "Sepal.Length"
  )

  iris2 <- iris
  iris2$Sepal.Length <- NULL

  expect_error(
    forge(iris2, x$blueprint),
    "Sepal.Length"
  )
})

test_that("inline offset wrapped in a function is not recognized as an offset (same as base)", {
  x <- mold(Species ~ log(offset(Sepal.Length)), iris)

  mf <- model.frame(Species ~ log(offset(Sepal.Length)), iris)
  trms <- attr(mf, "terms")

  expect_equal(x$extras$offset, NULL)

  expect_equal(
    attr(x$blueprint$terms$predictors, "offset"),
    attr(trms, "offset")
  )
})
