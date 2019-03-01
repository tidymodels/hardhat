#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model. It does this through the use of various
#' _engines_ that understand how to preprocess data that come in various
#' forms, such as a formula or a recipe.
#'
#' All engines have consistent return values with the others, but each is
#' unique enough to have its own help page. Click through below to learn
#' how to use each one in conjunction with `mold()`.
#'
#' * XY Method - [default_xy_engine()]
#'
#' * Formula Method - [default_formula_engine()]
#'
#' * Recipes Method - [default_recipe_engine()]
#'
#' @param x An object. See the method specific implementations linked in the
#' Description for more information.
#'
#' @param ... Not used.
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
#'  - `engine`: A method specific `"hardhat_engine"` object for use when
#'  making predictions.
#'
#'  - `extras`: Either `NULL` if the engine returns no extra information,
#'  or a named list containing the extra information.
#'
#' @examples
#' # See the method specific documentation linked in Description
#' # for the details of each engine, and more examples.
#'
#' # XY
#' mold(iris[, "Sepal.Width", drop = FALSE], iris$Species)
#'
#' # Formula
#' mold(Species ~ Sepal.Width, iris)
#'
#' # Recipe
#' library(recipes)
#' mold(recipe(Species ~ Sepal.Width, iris), iris)
#'
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  abort_unknown_mold_class(x)
}

#' @rdname default_xy_engine
#' @export
mold.data.frame <- function(x, y, engine = NULL, ...) {

  validate_empty_dots(...)

  if (is.null(engine)) {
    engine <- default_xy_engine()
  }

  validate_is_xy_engine(engine)

  mold_impl(engine, x, y)
}

#' @rdname default_xy_engine
#' @export
mold.matrix <- mold.data.frame

#' @rdname default_formula_engine
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

#' @rdname default_recipe_engine
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

