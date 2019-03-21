#' @param recipe Either `NULL`, or an unprepped recipe. This argument is set
#' automatically at [mold()] time.
#'
#' @rdname new-blueprint
#' @export
new_recipe_blueprint <- function(mold,
                                 forge,
                                 intercept = FALSE,
                                 ptypes = NULL,
                                 recipe = NULL,
                                 ...,
                                 subclass = character()) {

  validate_recipes_available()

  if (rlang::is_missing(mold)) {
    abort_no_mold()
  }

  if (rlang::is_missing(forge)) {
    abort_no_forge()
  }

  mold <- check_mold_xy(mold)
  forge <- check_forge(forge)

  validate_mold_args(
    mold = mold,
    required_clean_args = c("blueprint", "data"),
    required_process_args = c("blueprint", "data")
  )

  validate_is_recipe_or_null(recipe)

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    ptypes = ptypes,
    recipe = recipe,
    ...,
    subclass = c(subclass, "recipe_blueprint")
  )

}

#' @export
refresh_blueprint.recipe_blueprint <- function(blueprint) {
  do.call(new_recipe_blueprint, as.list(blueprint))
}

is_recipe_blueprint <- function(x) {
  inherits(x, "recipe_blueprint")
}

validate_is_recipe_blueprint <- function(blueprint) {
  validate_is(blueprint, is_recipe_blueprint, "recipe_blueprint")
}

# ------------------------------------------------------------------------------

is_recipe <- function(x) {
  inherits(x, "recipe")
}

validate_is_recipe_or_null <- function(recipe) {
  validate_is_or_null(recipe, is_recipe, "recipe")
}
