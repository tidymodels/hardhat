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

  # recipes bug?
  all_predictors <- recipes::all_predictors
  all_outcomes <- recipes::all_outcomes

  # "composition" output is always tibble
  predictors <- recipes::juice(prepped_recipe, all_predictors())
  outcomes <- recipes::juice(prepped_recipe, all_outcomes())

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  all_names <- get_original_recipe_names(data, prepped_recipe)
  all_levels <- get_original_recipe_levels(data, prepped_recipe)
  all_data_classes <- get_original_recipe_data_classes(data, prepped_recipe)

  preprocessor <- new_recipes_preprocessor(
    engine = prepped_recipe,
    intercept = intercept,
    predictors = predictors_lst(
      names = all_names$predictors,
      classes = all_data_classes$predictors,
      levels = all_levels$predictors
    ),
    outcomes = outcomes_lst(
      names = all_names$outcomes,
      classes = all_data_classes$outcomes,
      levels = all_levels$outcomes
    )
  )

  predictors <- maybe_add_intercept_column(predictors, intercept)

  mold_list(predictors, outcomes, preprocessor)
}

# ------------------------------------------------------------------------------

get_original_recipe_names <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictors = colnames(x[, original_predictors, drop = FALSE]),
    outcomes = colnames(x[, original_outcomes, drop = FALSE])
  )

}

get_original_recipe_levels <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictors = get_levels(x[, original_predictors, drop = FALSE]),
    outcomes = get_levels(x[, original_outcomes, drop = FALSE])
  )

}

get_original_recipe_data_classes <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictors = get_data_classes(x[, original_predictors, drop = FALSE]),
    outcomes = get_data_classes(x[, original_outcomes, drop = FALSE])
  )

}
