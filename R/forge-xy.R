#' Forge - XY Method
#'
#' @description
#'
#' For the default preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Calls on the default preprocessor engine to potentially add an intercept
#' column onto `new_data`, if the corresponding call to [mold()] used one.
#'
#' @details
#'
#' The one special thing about the XY method of `forge()` is the behavior of
#' `outcomes = TRUE` when a _vector_ `y` value was provided to the original
#' call to [mold()] (which generated the preprocessor). In that case, `mold()`
#' converted `y` into a tibble, with a default name of `.outcome`. This is the
#' column that `forge()` will look for in `new_data` to preprocess. See the
#' examples section for a demonstration of this.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"default_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' train_x <- train[, "Sepal.Length", drop = FALSE]
#' train_y <- train[, "Species", drop = FALSE]
#'
#' test_x <- test[, "Sepal.Length", drop = FALSE]
#' test_y <- test[, "Species", drop = FALSE]
#'
#' # ---------------------------------------------------------------------------
#' # XY Example
#'
#' # First, call mold() with the training data
#' processed <- mold(train_x, train_y)
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test_x)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept
#'
#' processed <- mold(train_x, train_y, intercept = TRUE)
#'
#' forge(processed$preprocessor, test_x)
#'
#' # ---------------------------------------------------------------------------
#' # XY Method and forge(outcomes = TRUE)
#'
#' # You can request that the new outcome columns are preprocessed as well, but
#' # they have to be present in `new_data`!
#'
#' processed <- mold(train_x, train_y)
#'
#' # Can't do this!
#' # forge(processed$preprocessor, test_x, outcomes = TRUE)
#'
#' # Need to use the full test set, including `y`
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # With the XY method, if the Y value used in `mold()` is a vector,
#' # then a column name of `.outcome` is automatically generated.
#' # This name is what forge() looks for in `new_data`.
#'
#' # Y is a vector!
#' y_vec <- train_y$Species
#'
#' processed_vec <- mold(train_x, y_vec)
#'
#' # This throws an informative error that tell you
#' # to include an `".outcome"` column in `new_data`.
#' \dontrun{
#' forge(processed_vec$preprocessor, iris, outcomes = TRUE)
#' }
#'
#' test2 <- test
#' test2$.outcome <- test2$Species
#' test2$Species <- NULL
#'
#' # This works, and returns a tibble in the $outcomes slot
#' forge(processed_vec$preprocessor, test2, outcomes = TRUE)
#'
#' @rdname forge-xy
#' @export
forge.default_preprocessor <- function(preprocessor, new_data,
                                       outcomes = FALSE, ...) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")

  new_data <- shrink(preprocessor, new_data, outcomes)
  new_data <- scream(preprocessor, new_data, outcomes)

  baked_list <- bake_default_engine(preprocessor, new_data, outcomes)

  baked_list
}

# ------------------------------------------------------------------------------

bake_default_engine <- function(preprocessor, new_data, outcomes) {

  if (outcomes) {
    baked_list <- bake_default_with_outcome(preprocessor, new_data)
  }
  else {
    baked_list <- bake_default_without_outcome(preprocessor, new_data)
  }

  baked_list$predictors <- preprocessor$engine$process(
    new_data = baked_list$predictors,
    intercept = preprocessor$intercept
  )

  baked_list
}

bake_default_with_outcome <- function(preprocessor, new_data) {

  original_predictor_columns <- preprocessor$info$predictors$names
  original_outcome_columns <- preprocessor$info$outcomes$names

  predictors <- new_data[, original_predictor_columns, drop = FALSE]
  outcomes <- new_data[, original_outcome_columns, drop = FALSE]

  forge_list(predictors, outcomes)
}

bake_default_without_outcome <- function(preprocessor, new_data) {

  original_predictor_columns <- preprocessor$info$predictors$names

  predictors <- new_data[, original_predictor_columns, drop = FALSE]

  forge_list(predictors)
}
