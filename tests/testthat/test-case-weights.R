# ------------------------------------------------------------------------------
# importance_weights

test_that("importance_weights() coerces to double", {
  expect_type(importance_weights(1L), "double")
})

test_that("importance_weights() retains names when coercing to double", {
  expect_named(importance_weights(c(x = 1L)), "x")
})

test_that("importance_weights() doesn't allow negative weights", {
  expect_snapshot(error = TRUE, importance_weights(-1))
})

test_that("importance_weights() allows missing values", {
  expect_true(is.na(importance_weights(NA)))
})

test_that("importance_weights() allows zero", {
  expect_identical(importance_weights(0), new_importance_weights(0))
})

test_that("can create importance-weights", {
  x <- new_importance_weights(1)
  expect_s3_class(x, "hardhat_importance_weights")
  expect_s3_class(x, "hardhat_case_weights")
  expect_type(x, "double")
})

test_that("importance-weights constructor checks for double data", {
  expect_snapshot(error = TRUE, new_importance_weights(1L))
})

test_that("can check for importance-weights class", {
  x <- importance_weights(1)
  expect_true(is_importance_weights(x))
})

test_that("common type of importance-weights <-> importance-weights exists", {
  x <- importance_weights(1)
  expect_identical(vec_ptype2(x, x), vec_ptype(x))
})

test_that("can cast importance-weights -> importance-weights", {
  x <- importance_weights(1)
  expect_identical(vec_cast(x, x), x)
})

test_that("can cast importance-weights -> double (its storage type)", {
  x <- importance_weights(1)
  expect_identical(vec_cast(x, double()), 1)
})

test_that("can't cast importance-weights -> integer (too lenient, likely fractional weights)", {
  x <- importance_weights(1)
  expect_snapshot(error = TRUE, vec_cast(x, integer()))
})

test_that("casting to double retains names", {
  x <- importance_weights(c(x = 1))
  expect_named(vec_cast(x, double()), "x")
})

test_that("as.double() works", {
  x <- importance_weights(1)
  expect_identical(as.double(x), 1)
})

test_that("as.integer() fails (too lenient, likely fractional weights)", {
  x <- importance_weights(1)
  expect_snapshot(error = TRUE, as.integer(x))
})

test_that("vec_ptype_full() and vec_ptype_abbr() methods are right", {
  expect_identical(
    vec_ptype_full(new_importance_weights()),
    "importance_weights"
  )
  expect_identical(vec_ptype_abbr(new_importance_weights()), "imp_wts")
})

# ------------------------------------------------------------------------------
# frequency_weights

test_that("frequency_weights() coerces to integer", {
  expect_type(frequency_weights(1), "integer")
  expect_snapshot(error = TRUE, frequency_weights(1.5))
})

test_that("frequency_weights() retains names when coercing to integer", {
  expect_named(frequency_weights(c(x = 1)), "x")
})

test_that("frequency_weights() doesn't allow negative weights", {
  expect_snapshot(error = TRUE, frequency_weights(-1L))
})

test_that("frequency_weights() allows missing values", {
  expect_true(is.na(frequency_weights(NA)))
})

test_that("frequency_weights() allows zero", {
  expect_identical(frequency_weights(0L), new_frequency_weights(0L))
})

test_that("can create frequency-weights", {
  x <- new_frequency_weights(1L)
  expect_s3_class(x, "hardhat_frequency_weights")
  expect_s3_class(x, "hardhat_case_weights")
  expect_type(x, "integer")
})

test_that("frequency-weights constructor checks for integer data", {
  expect_snapshot(error = TRUE, new_frequency_weights(1))
})

test_that("can check for frequency-weights class", {
  x <- frequency_weights(1L)
  expect_true(is_frequency_weights(x))
})

test_that("common type of frequency-weights <-> frequency-weights exists", {
  x <- frequency_weights(1L)
  expect_identical(vec_ptype2(x, x), vec_ptype(x))
})

test_that("can cast frequency-weights -> frequency-weights", {
  x <- frequency_weights(1L)
  expect_identical(vec_cast(x, x), x)
})

test_that("can cast frequency-weights -> integer (its storage type)", {
  x <- frequency_weights(1L)
  expect_identical(vec_cast(x, integer()), 1L)
})

test_that("can cast frequency-weights -> double (never lossy, #193)", {
  x <- frequency_weights(1L)
  expect_identical(vec_cast(x, double()), 1)
})

test_that("casting to integer retains names", {
  x <- frequency_weights(c(x = 1L))
  expect_named(vec_cast(x, integer()), "x")
  expect_named(vec_cast(x, double()), "x")
})

test_that("as.integer() works", {
  x <- frequency_weights(1L)
  expect_identical(as.integer(x), 1L)
})

test_that("as.double() works (never lossy, #193)", {
  x <- frequency_weights(1L)
  expect_identical(as.double(x), 1)
})

test_that("vec_ptype_full() and vec_ptype_abbr() methods are right", {
  expect_identical(vec_ptype_full(new_frequency_weights()), "frequency_weights")
  expect_identical(vec_ptype_abbr(new_frequency_weights()), "freq_wts")
})

# ------------------------------------------------------------------------------
# case_weights

test_that("can create a case-weights subclass", {
  x <- new_case_weights(1, class = "subclass")
  expect_s3_class(x, "subclass")
  expect_s3_class(x, "hardhat_case_weights")
  expect_type(x, "double")
})

test_that("can test for case-weights class", {
  x <- new_case_weights(1, class = "subclass")
  expect_true(is_case_weights(x))
})

test_that("can add attributes", {
  x <- new_case_weights(1, foo = "bar", class = "subclass")
  expect_identical(attr(x, "foo"), "bar")
})

test_that("`x` must be integer or double", {
  expect_snapshot(error = TRUE, new_case_weights("x", class = "subclass"))
})
