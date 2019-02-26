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
#' @param y A data frame, matrix, or vector. This should contain the outcomes.
#'
#' @param engine A preprocessing `engine`. If left as `NULL`, then a default
#' engine is chosen based on the type of `x`.
#'
#' @param formula A formula specifying the terms of the model.
#'
#' @param data A data frame or matrix containing the predictors and the outcomes.
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

#' @rdname mold
#' @export
mold.data.frame <- function(x, y, engine = NULL, ...) {

  validate_empty_dots(...)

  if (is.null(engine)) {
    engine <- default_xy_engine()
  }

  validate_is_xy_engine(engine)

  mold_impl(engine, x, y)
}

#' @rdname mold
#' @export
mold.matrix <- mold.data.frame

#' @rdname mold
#' @export
mold.formula <- function(formula, data, engine = NULL, ...) {

  validate_empty_dots(...)

  if (is.null(engine)) {
    engine <- default_formula_engine()
  }

  validate_is_formula_engine(engine)

  engine <- update_engine(
    engine = engine,
    formula = formula
  )

  mold_impl(engine, data)
}

#' @rdname mold
#' @export
mold.recipe <- function(x, data, engine = NULL, ...) {

  validate_empty_dots(...)

  validate_recipes_available()

  if (is.null(engine)) {
    engine <- default_recipe_engine()
  }

  validate_is_recipe_engine(engine)

  engine <- update_engine(
    engine = engine,
    recipe = x
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

  c(engine, predictors, outcomes, info, extras) %<-% engine$mold$process(
    engine = engine,
    x = x,
    y = y
  )

  engine <- update_engine(engine, info = info)

  out$mold$final(predictors, outcomes, engine, extras)

}

mold_impl.formula_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(
    engine = engine,
    data = data
  )

  c(engine, predictors, outcomes, info, extras) %<-% engine$mold$process(
    engine = engine,
    data = data
  )

  engine <- update_engine(engine, info = info)

  out$mold$final(predictors, outcomes, engine, extras)

}

mold_impl.recipe_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(
    engine = engine,
    data = data
  )

  c(engine, predictors, outcomes, info, extras) %<-% engine$mold$process(
    engine = engine,
    data = data
  )

  engine <- update_engine(engine, info = info)

  out$mold$final(predictors, outcomes, engine, extras)

}

