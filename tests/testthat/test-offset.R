context("test-offset")

test_that("an offset slot is NULL when there isn't one", {

  x <- mold(Species ~ Sepal.Width, iris)
  xx <- mold(iris[, "Sepal.Width", drop = FALSE], iris$Species)

  expect_equal(x$offset, NULL)
  expect_equal(xx$offset, NULL)

})

test_that("can use offsets", {

  x <- mold(Species ~ offset(Sepal.Width), iris)

  expect_is(x$offset, "tbl_df")
  expect_equal(colnames(x$offset), ".offset")
  expect_equal(x$offset$.offset, iris$Sepal.Width)
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
    model_offset(mf),
    NULL
  )

})

test_that("Only numeric columns can be offsets", {

  expect_error(
    mold(~Sepal.Width + offset(Species), iris),
    "Column, 'offset"
  )

})

test_that("multiple offsets can be used", {

  x <- mold(Species ~ offset(Sepal.Width) + offset(Sepal.Length), iris)

  expect_equal(
    x$offset$.offset,
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

  x <- mold(Species ~ offset(Sepal.Length), iris, intercept = TRUE)

  expect_equal(
    colnames(x$predictors),
    "(Intercept)"
  )

})

# This test is because model.matrix() is where the offsets are removed
# from the model.frame() result, but with indicators = FALSE we don't run
# that so we have to handle it specially ourselves.
test_that("offsets columns are removed from predictors with `indicators = FALSE`", {

  x <- mold(Species ~ offset(Sepal.Length), iris, indicators = FALSE)

  expect_equal(
    ncol(x$predictors),
    0
  )

})

test_that("offsets are NULL in forge() result if not used", {

  x <- mold(Species ~ Sepal.Length, iris)
  xx <- forge(x$preprocessor, iris)

  expect_equal(
    xx$offset,
    NULL
  )

})

test_that("offsets show up in forged results", {

  x <- mold(Species ~ offset(Sepal.Length), iris)
  xx <- forge(x$preprocessor, iris)

  expect_equal(
    xx$offset,
    tibble::tibble(.offset = iris$Sepal.Length)
  )

})

test_that("offset columns are stored as predictors", {

  x <- mold(Species ~ offset(Sepal.Length), iris)

  expect_equal(
    x$preprocessor$predictors$names,
    "Sepal.Length"
  )

  iris2 <- iris
  iris2$Sepal.Length <- NULL

  expect_error(
    forge(x$preprocessor, iris2),
    "Sepal.Length"
  )

})
