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

  baked_list <- bake_recipe_engine(
    preprocessor = preprocessor,
    new_data = new_data,
    outcomes = outcomes
  )

  baked_list
}

# ------------------------------------------------------------------------------

bake_recipe_engine <- function(preprocessor, new_data, outcomes) {

  if (outcomes) {
    baked_list <- bake_with_outcome(preprocessor, new_data)
  }
  else {
    baked_list <- bake_without_outcome(preprocessor, new_data)
  }

  baked_list$predictors <- maybe_add_intercept_column(
    x = baked_list$predictors,
    intercept = preprocessor$intercept
  )

  baked_list
}

bake_with_outcome <- function(preprocessor, new_data) {

  engine <- preprocessor$engine

  roles <- engine$term_info$role
  processed_predictor_nms <- engine$term_info$variable[roles == "predictor"]
  processed_outcome_nms <- engine$term_info$variable[roles == "outcome"]

  # optimize and don't double bake
  preprocessed_new_data <- recipes::bake(
    object = engine,
    new_data = new_data
  )

  predictors <- preprocessed_new_data[, processed_predictor_nms, drop = FALSE]
  outcomes <- preprocessed_new_data[, processed_outcome_nms, drop = FALSE]

  forge_list(
    predictors = predictors,
    outcomes = outcomes
  )
}

bake_without_outcome <- function(preprocessor, new_data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::bake(
    object = preprocessor$engine,
    new_data = new_data,
    all_predictors()
  )

  forge_list(
    predictors = predictors
  )
}
