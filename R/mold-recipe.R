#' Mold - Recipes Method
#'
#' @description
#'
#' For a recipe, `mold()` does the following:
#'
#' - Calls [recipes::prep()] to prep the recipe.
#'
#' - Calls [recipes::juice()] to extract the outcomes and predictors. These
#' are returned as tibbles.
#'
#' - If `intercept = TRUE`, adds an intercept column to the predictors.
#'
#' @inheritParams mold
#' @inheritParams mold.formula
#'
#' @param x An unprepped recipe created from [recipes::recipe()].
#'
#' @inherit mold return
#'
#' @examples
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Recipes example
#'
#' # Create a recipe that logs a predictor
#' rec <- recipe(Species ~ Sepal.Length + Sepal.Width, iris) %>%
#'    step_log(Sepal.Length)
#'
#' processed <- mold(rec, iris)
#'
#' # Sepal.Length has been logged
#' processed$predictors
#'
#' processed$outcomes
#'
#' # The underlying engine is a prepped recipe
#' processed$preprocessor$engine
#'
#' # ---------------------------------------------------------------------------
#' # With an intercept
#'
#' # You can add a predictor with `intercept = TRUE`
#' processed <- mold(rec, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' # But you also could have used a recipe step
#' rec2 <- step_intercept(rec)
#'
#' mold(rec2, iris)$predictors
#'
#' @rdname mold-recipe
#' @export
mold.recipe <- function(x, data, intercept = FALSE, engine = NULL, ...) {

  validate_recipes_available()

  if (is.null(engine)) {
    engine <- new_default_recipe_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(
    engine = engine,
    recipe = x,
    intercept = intercept,
    ...
  )

  mold_impl(engine, data)
}

# ------------------------------------------------------------------------------

mold_impl.recipe_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(engine, data)
  c(engine, predictors, outcomes) %<-% engine$mold$process(engine, data)

  info <- info_lst(predictors = predictors$info, outcomes = outcomes$info)

  engine <- update_engine(engine, info = info)

  mold_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    engine = engine,
    offset = predictors$offset
    # extras = extras
  )

}

# ------------------------------------------------------------------------------

get_original_predictor_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "predictor"]

  original_data <- data[, original_names, drop = FALSE]

  predictors_info(
    names = original_names,
    classes = get_data_classes(original_data),
    levels = get_levels(original_data)
  )

}

get_original_outcome_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "outcome"]

  original_data <- data[, original_names, drop = FALSE]

  outcomes_info(
    names = original_names,
    classes = get_data_classes(original_data),
    levels = get_levels(original_data)
  )

}
