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
#'  - `outcomes`: A tibble containing the molded outcomes to be used in the
#'  model.
#'
#'  - `blueprint`: A method specific `"hardhat_blueprint"` object for use when
#'  making predictions.
#'
#'  - `extras`: Either `NULL` if the blueprint returns no extra information,
#'  or a named list containing the extra information.
#'
#' @examplesIf rlang::is_installed(c("recipes"))
#' # See the method specific documentation linked in Description
#' # for the details of each blueprint, and more examples.
#'
#' # XY
#' mold(iris["Sepal.Width"], iris$Species)
#'
#' # Formula
#' mold(Species ~ Sepal.Width, iris)
#'
#' # Recipe
#' library(recipes)
#' mold(recipe(Species ~ Sepal.Width, iris), iris)
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  stop_input_type(
    x = x,
    what = "a data frame, matrix, recipe, or formula"
  )
}

#' @rdname default_xy_blueprint
#' @export
mold.data.frame <- function(x, y, ..., blueprint = NULL) {
  check_dots_empty0(...)

  if (is.null(blueprint)) {
    blueprint <- default_xy_blueprint()
  }

  check_xy_blueprint(blueprint)

  run_mold(blueprint, x = x, y = y)
}

#' @rdname default_xy_blueprint
#' @export
mold.matrix <- mold.data.frame

#' @rdname default_formula_blueprint
#' @export
mold.formula <- function(formula, data, ..., blueprint = NULL) {
  check_dots_empty0(...)

  if (is.null(blueprint)) {
    blueprint <- default_formula_blueprint()
  }

  check_formula_blueprint(blueprint)

  blueprint <- update_blueprint0(blueprint, formula = formula)

  run_mold(blueprint, data = data)
}

#' @rdname default_recipe_blueprint
#' @export
mold.recipe <- function(x, data, ..., blueprint = NULL) {
  check_dots_empty0(...)

  if (is.null(blueprint)) {
    blueprint <- default_recipe_blueprint()
  }

  check_recipe_blueprint(blueprint)

  blueprint <- update_blueprint0(blueprint, recipe = x)

  run_mold(blueprint, data = data)
}

# ------------------------------------------------------------------------------

#' `mold()` according to a blueprint
#'
#' @description
#' This is a developer facing function that is _only_ used if you are creating
#' your own blueprint subclass. It is called from [mold()] and dispatches off
#' the S3 class of the `blueprint`. This gives you an opportunity to mold the
#' data in a way that is specific to your blueprint.
#'
#' `run_mold()` will be called with different arguments depending on the
#' interface to `mold()` that is used:
#'
#' - XY interface:
#'   - `run_mold(blueprint, x = x, y = y)`
#'
#' - Formula interface:
#'   - `run_mold(blueprint, data = data)`
#'   - Additionally, the `blueprint` will have been updated to contain the
#'   `formula`.
#'
#' - Recipe interface:
#'   - `run_mold(blueprint, data = data)`
#'   - Additionally, the `blueprint` will have been updated to contain the
#'   `recipe`.
#'
#' If you write a blueprint subclass for [new_xy_blueprint()],
#' [new_recipe_blueprint()], or [new_formula_blueprint()] then your `run_mold()`
#' method signature must match whichever interface listed above will be used.
#'
#' If you write a completely new blueprint inheriting only from
#' [new_blueprint()] and write a new [mold()] method (because you aren't using
#' an xy, formula, or recipe interface), then you will have full control over
#' how `run_mold()` will be called.
#'
#' @param blueprint A preprocessing blueprint.
#'
#' @param ... Not used. Required for extensibility.
#'
#' @inheritParams validate_column_names
#'
#' @return
#' `run_mold()` methods return the object that is then immediately returned from
#' `mold()`. See the return value section of [mold()] to understand what the
#' structure of the return value should look like.
#'
#' @name run-mold
#' @order 1
#' @export
#' @examples
#' bp <- default_xy_blueprint()
#'
#' outcomes <- mtcars["mpg"]
#' predictors <- mtcars
#' predictors$mpg <- NULL
#'
#' run_mold(bp, x = predictors, y = outcomes)
run_mold <- function(blueprint, ...) {
  UseMethod("run_mold")
}

#' @export
run_mold.default <- function(blueprint, ...) {
  cli::cli_abort(
    "No {.fn run_mold} method provided for {.obj_type_friendly {blueprint}}."
  )
}
