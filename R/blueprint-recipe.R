#' @param recipe Either `NULL`, or an unprepped recipe. This argument is set
#'   automatically at [mold()] time.
#'
#' @param fresh Should already trained operations be re-trained when `prep()` is
#'   called?
#'
#' @param prep_specific_roles A character vector of recipes "roles"
#'   corresponding to columns that are only required at `prep()` time, and are
#'   not required at `bake()` time. These columns are excluded from checks done
#'   in [forge()] to validate the column structure of `new_data`, will not be
#'   passed to `bake()` even if they existed in `new_data`, and will not be
#'   returned in the `forge()$extras$roles` slot. Any custom role can be
#'   specified (such as `"case_weights"`), but `"outcome"` and `"predictor"`
#'   roles are not allowed. See the documentation of [recipes::add_role()] for
#'   more information about roles.
#'
#' @rdname new-blueprint
#' @export
new_recipe_blueprint <- function(mold,
                                 forge,
                                 intercept = FALSE,
                                 allow_novel_levels = FALSE,
                                 fresh = TRUE,
                                 prep_specific_roles = character(),
                                 composition = "tibble",
                                 ptypes = NULL,
                                 recipe = NULL,
                                 ...,
                                 subclass = character()) {
  validate_recipes_available()

  validate_is_function_set(mold)
  validate_mold_args(
    mold = mold,
    required_clean_args = c("blueprint", "data"),
    required_process_args = c("blueprint", "data")
  )

  validate_is_bool(fresh)
  validate_prep_specific_roles(prep_specific_roles)

  validate_is_recipe_or_null(recipe)

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    prep_specific_roles = prep_specific_roles,
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

validate_prep_specific_roles <- function(prep_specific_roles) {
  validate_is_character(prep_specific_roles, "prep_specific_roles")

  if (anyNA(prep_specific_roles)) {
    abort("`prep_specific_roles` can't contain missing values.")
  }

  if (any(prep_specific_roles %in% c("outcome", "predictor"))) {
    abort("`prep_specific_roles` can't be 'outcome' or 'predictor'.")
  }

  invisible(prep_specific_roles)
}
