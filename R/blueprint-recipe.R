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
  check_bool(fresh)
  check_recipe(recipe, allow_null = TRUE)

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

check_recipe_blueprint <- function(x,
                                   ...,
                                   arg = caller_arg(x),
                                   call = caller_env()) {
  check_inherits(x, "recipe_blueprint", arg = arg, call = call)
}

# ------------------------------------------------------------------------------

is_recipe <- function(x) {
  inherits(x, "recipe")
}

check_recipe <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is_recipe(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x = x,
    what = "a recipe",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
