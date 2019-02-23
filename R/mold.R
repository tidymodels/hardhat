#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' The return values of each engine are all consistent with one another, but the
#' nuances of exactly what is being done for each engine vary enough to warrant
#' separate help files for each. Click through to each one below to learn
#' about each engine and see a large amount of engine specific examples:
#'
#' * XY Method - [default_xy_engine()]
#'
#' * Formula Method - [default_formula_engine()]
#'
#' * Recipes Method - [default_recipe_engine()]
#'
#' @param x A data frame, matrix, formula, or [recipes::recipe()]. If this is a
#' data.frame or matrix, it should contain the predictors.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing 4 elements:
#'
#'  - `predictors`: A tibble containing the molded predictors to be used in the
#'  model.
#'
#'  - `outcome`: A tibble containing the molded outcomes to be used in the
#'  model.
#'
#'  - `engine`: A `"hardhat_engine"` object for use when making predictions.
#'
#'  - `offset`: A tibble with a single column named `".offset"` if an offset
#'  was specified in the formula method. Otherwise, `NULL`.
#'
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  abort_unknown_mold_class(x)
}

#' @export
mold.data.frame <- function(x, y, intercept = FALSE, engine = NULL, ...) {

  if (is.null(engine)) {
    engine <- default_xy_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(engine, intercept = intercept, ...)

  mold_impl(engine, x, y)
}

#' @export
mold.matrix <- mold.data.frame

#' @export
mold.formula <- function(formula, data, intercept = FALSE,
                         indicators = TRUE, engine = NULL, ...) {

  if (is.null(engine)) {
    engine <- default_formula_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(
    engine = engine,
    formula = formula,
    intercept = intercept,
    indicators = indicators,
    ...
  )

  mold_impl(engine, data)
}

#' @export
mold.recipe <- function(x, data, intercept = FALSE, engine = NULL, ...) {

  validate_recipes_available()

  if (is.null(engine)) {
    engine <- default_recipe_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(
    engine = engine,
    recipe = x,
    intercept = intercept,
    ...
  )

  mold_impl(engine, data)
}

# ------------------------------------------------------------------------------

mold_impl <- function(engine, ...) {
  UseMethod("mold_impl")
}

mold_impl.xy_engine <- function(engine, x, y, ...) {

  c(engine, x, y) %<-% engine$mold$clean(
    engine = engine,
    x = x,
    y = y
  )

  c(engine, predictors, outcomes) %<-% engine$mold$process(
    engine = engine,
    x = x,
    y = y
  )

  mold_impl_common(engine, predictors, outcomes)

}

mold_impl.formula_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(
    engine = engine,
    data = data
  )

  c(engine, predictors, outcomes) %<-% engine$mold$process(
    engine = engine,
    data = data
  )

  mold_impl_common(engine, predictors, outcomes)

}

mold_impl.recipe_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(
    engine = engine,
    data = data
  )

  c(engine, predictors, outcomes) %<-% engine$mold$process(
    engine = engine,
    data = data
  )

  mold_impl_common(engine, predictors, outcomes)

}

mold_impl_common <- function(engine, predictors, outcomes) {

  info <- info_lst(predictors = predictors$info, outcomes = outcomes$info)

  engine <- update_engine(engine, info = info)

  mold_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    engine = engine,
    offset = predictors$offset
  )

}

# ------------------------------------------------------------------------------

# Would it also be useful to add an `extras` element here?
# So specific implementations can return other processed
# information as needed? For engine specific things, they
# can just add information to the engine, but maybe there
# are other things (like the offsets) that you might want
# to return? (oooh, maybe offset counts as an `extra`
# for the formula method?)

mold_list <- function(predictors, outcomes, engine, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    engine = engine,
    offset = offset
  )
}

