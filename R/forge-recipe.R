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
NULL

forge_impl.recipe_engine <- function(engine, new_data, outcomes) {

  validate_recipes_available()

  c(engine, new_data) %<-% engine$forge$clean(engine, new_data, outcomes)
  c(engine, predictors, outcomes) %<-% engine$forge$process(engine, new_data, outcomes)

  forge_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    offset = predictors$offset
    # extras = extras # maybe this would be useful?
  )
}
