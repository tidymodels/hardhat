#' @param recipe Either `NULL`, or an unprepped recipe. This argument is set
#'   automatically at [mold()] time.
#'
#' @param fresh Should already trained operations be re-trained when `prep()` is
#'   called?
#'
#' @rdname new-blueprint
#' @export
new_recipe_blueprint <- function(intercept = FALSE,
                                 allow_novel_levels = FALSE,
                                 fresh = TRUE,
                                 composition = "tibble",
                                 ptypes = NULL,
                                 recipe = NULL,
                                 ...,
                                 subclass = character()) {
  validate_recipes_available()

  validate_is_bool(fresh)
  validate_is_recipe_or_null(recipe)

  new_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    composition = composition,
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
