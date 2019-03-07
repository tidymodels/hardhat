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
mold.data.frame <- function(x, y, ..., engine = NULL) {

  validate_empty_dots(...)

  if (is.null(engine)) {
    engine <- default_xy_engine()
  }

  validate_is_xy_engine(engine)

  c(engine, predictors, outcomes, ptypes, extras) %<-% run_mold(engine, x, y)

  engine <- update_engine(engine, ptypes = ptypes)

  out$mold$final(predictors, outcomes, engine, extras)
}

#' @rdname default_xy_engine
#' @export
mold.matrix <- mold.data.frame

#' @rdname default_formula_engine
#' @export
mold.formula <- function(formula, data, ..., engine = NULL) {

  validate_empty_dots(...)

  if (is.null(engine)) {
    engine <- default_formula_engine()
  }

  validate_is_formula_engine(engine)

  engine <- update_engine(engine = engine, formula = formula)

  c(engine, predictors, outcomes, ptypes, extras) %<-% run_mold(engine, data)

  engine <- update_engine(engine, ptypes = ptypes)

  out$mold$final(predictors, outcomes, engine, extras)
}

#' @rdname default_recipe_engine
#' @export
mold.recipe <- function(x, data, ..., engine = NULL) {

  validate_empty_dots(...)

  validate_recipes_available()

  if (is.null(engine)) {
    engine <- default_recipe_engine()
  }

  validate_is_recipe_engine(engine)

  engine <- update_engine(engine = engine, recipe = x)

  c(engine, predictors, outcomes, ptypes, extras) %<-% run_mold(engine, data)

  engine <- update_engine(engine, ptypes = ptypes)

  out$mold$final(predictors, outcomes, engine, extras)
}

# ------------------------------------------------------------------------------

#' Call `mold$clean()` and `mold$process()`
#'
#' This is a purely developer facing function, that is _only_ used if you are
#' creating a completely new engine inheriting only from [new_engine()], and
#' not from one of the more common: [new_xy_engine()], [new_recipe_engine()],
#' [new_formula_engine()].
#'
#' @param engine A preprocessing engine.
#'
#' @param ... Not used. Required for extensibility.
#'
#' @details
#'
#' Because `mold()` has different interfaces (like XY and formula),
#' which require different arguments (`x` and `y` vs `data`), their
#' corresponding engines also have different arguments for the
#' `engine$mold$clean()` and `engine$mold$process()` functions. The sole
#' job of `run_mold()` is simply to call these two functions with the right
#' arguments.
#'
#' The only time you need to implement a method for `run_mold()` is if you
#' are creating a `new_engine()` that does not follow one of the three core
#' engine types. In that special case, create a method for `run_mold()` with
#' your engine type, and pass through whatever arguments are necessary to call
#' your engine specific `clean()` and `process()` functions.
#'
#' If you go this route, you will also need to create a `mold()` method if `x`
#' is not a data frame / matrix, recipe, or formula. If `x` is one of
#' those types, then `run_mold()` will be called for you by the
#' existing `mold()` method, you just have to supply the `run_mold()` method
#' for your engine.
#'
#' @export
run_mold <- function(engine, ...) {
  UseMethod("run_mold")
}

#' @export
run_mold.xy_engine <- function(engine, x, y, ...) {

  c(engine, x, y) %<-% engine$mold$clean(
    engine = engine,
    x = x,
    y = y
  )

  engine$mold$process(engine = engine, x = x, y = y)
}

#' @export
run_mold.formula_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(engine = engine, data = data)

  engine$mold$process(engine = engine, data = data)
}

#' @export
run_mold.recipe_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(engine = engine, data = data)

  engine$mold$process(engine = engine, data = data)
}

