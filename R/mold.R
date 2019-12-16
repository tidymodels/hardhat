#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model. It does this through the use of various
#' _blueprints_ that understand how to preprocess data that come in various
#' forms, such as a formula or a recipe.
#'
#' All blueprints have consistent return values with the others, but each is
#' unique enough to have its own help page. Click through below to learn
#' how to use each one in conjunction with `mold()`.
#'
#' * XY Method - [default_xy_blueprint()]
#'
#' * Formula Method - [default_formula_blueprint()]
#'
#' * Recipes Method - [default_recipe_blueprint()]
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
#'  - `blueprint`: A method specific `"hardhat_blueprint"` object for use when
#'  making predictions.
#'
#'  - `extras`: Either `NULL` if the blueprint returns no extra information,
#'  or a named list containing the extra information.
#'
#' @examples
#' # See the method specific documentation linked in Description
#' # for the details of each blueprint, and more examples.
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

#' @rdname default_xy_blueprint
#' @export
mold.data.frame <- function(x, y, ..., blueprint = NULL) {

  validate_empty_dots(...)

  if (is.null(blueprint)) {
    blueprint <- default_xy_blueprint()
  }

  validate_is_xy_blueprint(blueprint)

  molded <- run_mold(blueprint, x, y)

  blueprint <- molded$blueprint
  predictors <- molded$predictors
  outcomes <- molded$outcomes
  ptypes <- molded$ptypes
  extras <- molded$extras

  blueprint <- update_blueprint(blueprint, ptypes = ptypes)

  out$mold$final(predictors, outcomes, blueprint, extras)
}

#' @rdname default_xy_blueprint
#' @export
mold.matrix <- mold.data.frame

#' @rdname default_formula_blueprint
#' @export
mold.formula <- function(formula, data, ..., blueprint = NULL) {

  validate_empty_dots(...)

  if (is.null(blueprint)) {
    blueprint <- default_formula_blueprint()
  }

  validate_is_formula_blueprint(blueprint)

  blueprint <- update_blueprint(blueprint = blueprint, formula = formula)

  molded <- run_mold(blueprint, data)

  blueprint <- molded$blueprint
  predictors <- molded$predictors
  outcomes <- molded$outcomes
  ptypes <- molded$ptypes
  extras <- molded$extras

  blueprint <- update_blueprint(blueprint, ptypes = ptypes)

  out$mold$final(predictors, outcomes, blueprint, extras)
}

#' @rdname default_recipe_blueprint
#' @export
mold.recipe <- function(x, data, ..., blueprint = NULL) {

  validate_empty_dots(...)

  validate_recipes_available()

  if (is.null(blueprint)) {
    blueprint <- default_recipe_blueprint()
  }

  validate_is_recipe_blueprint(blueprint)

  blueprint <- update_blueprint(blueprint = blueprint, recipe = x)

  molded <- run_mold(blueprint, data)

  blueprint <- molded$blueprint
  predictors <- molded$predictors
  outcomes <- molded$outcomes
  ptypes <- molded$ptypes
  extras <- molded$extras

  blueprint <- update_blueprint(blueprint, ptypes = ptypes)

  out$mold$final(predictors, outcomes, blueprint, extras)
}

# ------------------------------------------------------------------------------

#' Call `mold$clean()` and `mold$process()`
#'
#' This is a purely developer facing function, that is _only_ used if you are
#' creating a completely new blueprint inheriting only from [new_blueprint()], and
#' not from one of the more common: [new_xy_blueprint()], [new_recipe_blueprint()],
#' [new_formula_blueprint()].
#'
#' @param blueprint A preprocessing blueprint.
#'
#' @param ... Not used. Required for extensibility.
#'
#' @return
#'
#' The preprocessed result, as a named list.
#'
#' @details
#'
#' Because `mold()` has different interfaces (like XY and formula),
#' which require different arguments (`x` and `y` vs `data`), their
#' corresponding blueprints also have different arguments for the
#' `blueprint$mold$clean()` and `blueprint$mold$process()` functions. The sole
#' job of `run_mold()` is simply to call these two functions with the right
#' arguments.
#'
#' The only time you need to implement a method for `run_mold()` is if you
#' are creating a `new_blueprint()` that does not follow one of the three core
#' blueprint types. In that special case, create a method for `run_mold()` with
#' your blueprint type, and pass through whatever arguments are necessary to call
#' your blueprint specific `clean()` and `process()` functions.
#'
#' If you go this route, you will also need to create a `mold()` method if `x`
#' is not a data frame / matrix, recipe, or formula. If `x` is one of
#' those types, then `run_mold()` will be called for you by the
#' existing `mold()` method, you just have to supply the `run_mold()` method
#' for your blueprint.
#'
#' @export
run_mold <- function(blueprint, ...) {
  UseMethod("run_mold")
}

#' @export
run_mold.xy_blueprint <- function(blueprint, x, y, ...) {
  cleaned <- blueprint$mold$clean(blueprint = blueprint, x = x, y = y)

  blueprint <- cleaned$blueprint
  x <- cleaned$x
  y <- cleaned$y

  blueprint$mold$process(blueprint = blueprint, x = x, y = y)
}

#' @export
run_mold.formula_blueprint <- function(blueprint, data, ...) {
  cleaned <- blueprint$mold$clean(blueprint = blueprint, data = data)

  blueprint <- cleaned$blueprint
  data <- cleaned$data

  blueprint$mold$process(blueprint = blueprint, data = data)
}

#' @export
run_mold.recipe_blueprint <- function(blueprint, data, ...) {
  cleaned <- blueprint$mold$clean(blueprint = blueprint, data = data)

  blueprint <- cleaned$blueprint
  data <- cleaned$data

  blueprint$mold$process(blueprint = blueprint, data = data)
}

