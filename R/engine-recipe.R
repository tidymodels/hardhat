new_recipe_engine <- function(mold,
                              forge,
                              intercept = FALSE,
                              info = NULL,
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
    required_clean_args = c("engine", "data"),
    required_process_args = c("engine", "data")
  )

  validate_forge_args(forge)

  validate_is_recipe_or_null(recipe)

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    recipe = recipe,
    ...,
    subclass = c(subclass, "recipe_engine")
  )

}

#' @export
refresh_engine.recipe_engine <- function(engine) {
  do.call(new_recipe_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

is_recipe <- function(x) {
  inherits(x, "recipe")
}

validate_is_recipe_or_null <- function(recipe) {
  validate_is_or_null(recipe, is_recipe, "recipe")
}
