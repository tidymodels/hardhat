#' Forge - Recipes Method
#'
#' @description
#'
#' For the recipes preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Calls [recipes::bake()] on the `new_data` using the prepped recipe
#' used during training.
#'
#' - Potentially adds an intercept column onto `new_data`,
#' if the corresponding call to [mold()] used one.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"recipes_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#'
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Recipes Example
#'
#' # Create a recipe for the preprocessing
#' rec <- recipe(Sepal.Width ~ Sepal.Length + Species, iris) %>%
#'    step_log(Sepal.Length) %>%
#'    step_dummy(Species)
#'
#' # Call mold() with the training data
#' processed <- mold(rec, train)
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome!
#' # This logged the Sepal.Length column of `new_data`
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' @rdname forge-recipe
#' @export
forge.recipes_preprocessor <- function(preprocessor, new_data,
                                       outcomes = FALSE, ...) {

  validate_recipes_available()
  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink(preprocessor, new_data, outcomes)
  new_data <- scream(preprocessor, new_data, outcomes)

  # Can't move this inside forge_recipe_*(), would be double baking
  new_data <- recipes::bake(
    object = preprocessor$engine,
    new_data = new_data
  )

  .predictors <- forge_recipes_predictors(preprocessor, new_data)
  .outcomes <- forge_recipes_outcomes(preprocessor, new_data, outcomes)

  forge_list(.predictors$data, .outcomes$data)
}

# ------------------------------------------------------------------------------

forge_recipes_predictors <- function(preprocessor, new_data) {

  engine <- preprocessor$engine
  roles <- engine$term_info$role
  processed_nms <- engine$term_info$variable[roles == "predictor"]

  .predictors <- new_data[, processed_nms, drop = FALSE]

  .predictors <- maybe_add_intercept_column(
    x = .predictors,
    intercept = preprocessor$intercept
  )

  list(data = .predictors)

}

forge_recipes_outcomes <- function(preprocessor, new_data, outcomes) {

  if (!outcomes) {
    .outcomes <- list(data = NULL)
    return(.outcomes)
  }

  engine <- preprocessor$engine
  roles <- engine$term_info$role
  processed_nms <- engine$term_info$variable[roles == "outcome"]

  .outcomes <- new_data[, processed_nms, drop = FALSE]

  list(data = .outcomes)

}
