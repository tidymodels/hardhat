context("one-hot and regular encodings")

dat_tr <-
  data.frame(
    x = 1:12,
    y = factor(rep(letters[1:3], each = 4)),
    z = factor(rep(LETTERS[1:2], 6))
  )

dat_te <-
  data.frame(
    x = 20:25,
    y = factor(rep(letters[1:3], 2)),
    z = factor(rep(LETTERS[1:2], each = 3))
  )

## -----------------------------------------------------------------------------

check_y <- function(dat, bp) {
  y_cols <- sum(grepl("^y", names(dat)))
  if (bp$one_hot) {
    good <- y_cols == 3
  } else {
    good <- y_cols == 2
  }
  good
}

check_z <- function(dat, bp) {
  y_cols <- sum(grepl("^z", names(dat)))
  if (bp$one_hot) {
    good <- y_cols == 2
  } else {
    good <- y_cols == 1
  }
  good
}

check_int <- function(dat, bp) {
  has_int <- (any(names(dat) == "(Intercept)"))
  if (bp$intercept) {
    good <- has_int
  } else {
    good <- !has_int
  }
  good
}

## -----------------------------------------------------------------------------

test_that("defaults: regular encoding and no intercept", {
  res <- mold(x ~ y + z, dat_tr)
  res_te <- forge(dat_te, res$blueprint)

  expect_true(check_y(res$predictors,   bp = res$blueprint))
  expect_true(check_z(res$predictors,   bp = res$blueprint))
  expect_true(check_int(res$predictors, bp = res$blueprint))

  expect_true(check_y(res_te$predictors,   bp = res$blueprint))
  expect_true(check_z(res_te$predictors,   bp = res$blueprint))
  expect_true(check_int(res_te$predictors, bp = res$blueprint))
})


## -----------------------------------------------------------------------------

test_that("regular encoding and intercept", {
  bp <-
    default_formula_blueprint(
      intercept = TRUE,
      indicators = TRUE,
      one_hot = FALSE
    )
  res <- mold(x ~ y + z, dat_tr, blueprint = bp)
  res_te <- forge(dat_te, res$blueprint)

  expect_true(check_y(res$predictors,   bp = res$blueprint))
  expect_true(check_z(res$predictors,   bp = res$blueprint))
  expect_true(check_int(res$predictors, bp = res$blueprint))

  expect_true(check_y(res_te$predictors,   bp = res$blueprint))
  expect_true(check_z(res_te$predictors,   bp = res$blueprint))
  expect_true(check_int(res_te$predictors, bp = res$blueprint))
})


## -----------------------------------------------------------------------------

test_that("one-hot encoding and no intercept", {
  bp <-
    default_formula_blueprint(
      intercept = FALSE,
      indicators = TRUE,
      one_hot = TRUE
    )
  res <- mold(x ~ y + z, dat_tr, blueprint = bp)
  res_te <- forge(dat_te, res$blueprint)

  expect_true(check_y(res$predictors,   bp = res$blueprint))
  expect_true(check_z(res$predictors,   bp = res$blueprint))
  expect_true(check_int(res$predictors, bp = res$blueprint))

  expect_true(check_y(res_te$predictors,   bp = res$blueprint))
  expect_true(check_z(res_te$predictors,   bp = res$blueprint))
  expect_true(check_int(res_te$predictors, bp = res$blueprint))
})


## -----------------------------------------------------------------------------

test_that("one-hot encoding and intercept", {
  bp <-
    default_formula_blueprint(
      intercept = TRUE,
      indicators = TRUE,
      one_hot = TRUE
    )
  res <- mold(x ~ y + z, dat_tr, blueprint = bp)
  res_te <- forge(dat_te, res$blueprint)

  expect_true(check_y(res$predictors,   bp = res$blueprint))
  expect_true(check_z(res$predictors,   bp = res$blueprint))
  expect_true(check_int(res$predictors, bp = res$blueprint))

  expect_true(check_y(res_te$predictors,   bp = res$blueprint))
  expect_true(check_z(res_te$predictors,   bp = res$blueprint))
  expect_true(check_int(res_te$predictors, bp = res$blueprint))
})


