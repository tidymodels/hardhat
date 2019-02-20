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
mold.recipe <- function(x, data, intercept = FALSE, ...) {

  validate_recipes_available()

  data <- check_is_data_like(data)

  prepped_recipe <- recipes::prep(x, training = data)

  predictors <- mold_recipe_predictors(prepped_recipe, data, intercept)
  outcomes <- mold_recipe_outcomes(prepped_recipe, data)

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  preprocessor <- new_recipes_preprocessor(
    engine = prepped_recipe,
    intercept = intercept,
    info = info_lst(
      predictors = predictors$info,
      outcomes = outcomes$info
    )
  )

  mold_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    preprocessor = preprocessor
  )

}

# ------------------------------------------------------------------------------

mold_recipe_predictors <- function(rec, data, intercept) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::juice(rec, all_predictors())

  predictors <- maybe_add_intercept_column(predictors, intercept)

  info <- get_original_predictor_info(rec, data)

  list(
    data = predictors,
    info = info
  )

}

mold_recipe_outcomes <- function(rec, data) {

  all_outcomes <- recipes::all_outcomes

  outcomes <- recipes::juice(rec, all_outcomes())

  info <- get_original_outcome_info(rec, data)

  list(
    data = outcomes,
    info = info
  )

}

# ------------------------------------------------------------------------------

get_original_predictor_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]

  predictors_info(
    names = colnames(data[, original_predictors, drop = FALSE]),
    classes = get_data_classes(data[, original_predictors, drop = FALSE]),
    levels = get_levels(data[, original_predictors, drop = FALSE])
  )

}

get_original_outcome_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "outcome"]

  outcomes_info(
    names = colnames(data[, original_predictors, drop = FALSE]),
    classes = get_data_classes(data[, original_predictors, drop = FALSE]),
    levels = get_levels(data[, original_predictors, drop = FALSE])
  )

}
