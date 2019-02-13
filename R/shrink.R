#' Subset only required columns
#'
#' @description
#'
#' `shrink()` performs a number of validation steps on `new_data`, and then
#' subsets it to only contain the required columns. `shrink()` does the
#' following validation:
#'
#' - Checks that all required predictor columns exist in `new_data`.
#'
#' - Checks that the class of each required predictor in `new_data` is the same
#' as the class used during training.
#'
#' - Checks that all `new_data` factor columns don't have any _new_ levels
#' when compared with the original data used in training. If there are new
#' levels, they are replaced with `NA` values and a warning is
#' thrown.
#'
#' @details
#'
#' If `outcome = TRUE`, then the validation steps are performed on the known
#' outcome column as well, and it is returned along with the predictors. If
#' [prepare()] was called with the XY interface, then no preprocessing was done
#' to `y` and `outcome` cannot be set to `TRUE` (if a vector was passed as `y`
#' during the fit, `shrink()` has no way of knowing what column in `new_data`
#' corresponds to the outcome).
#'
#' `shrink()` is called by [preprocess()] before the actual processing is done.
#'
#' @param preprocessor A `"preprocessor"`.
#'
#' @param new_data A data frame containing the new data that will be shrunk down
#' to only contain the predictors required for preprocessing and prediction.
#'
#' @param outcome A logical. Should the outcome be included in the shrunk
#' `new_data` as well?
#'
#' @return
#'
#' A tibble containing the required predictors (and potentially the
#' outcomes).
#'
#' @examples
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # prepare() is run at model fit time
#' # and a terms preprocessor is recorded
#' x <- prepare(log(Sepal.Width) ~ Species, train)
#'
#' # Pass that preprocessor to shrink(), along with new_data
#' # to get a tibble of required predictors back
#' shrink(x$preprocessor, test)
#'
#' # outcome = TRUE also returns the outcome,
#' # this can be useful if doing cross validation
#' shrink(x$preprocessor, test, outcome = TRUE)
#'
#' # shrink() validates that the classes are the same
#' # as the ones used in prepare(). The below call
#' # to shrink() will fail with an informative error.
#' test2 <- test
#' test2$Species <- as.character(test2$Species)
#'
#' \dontrun{
#' shrink(x$preprocessor, test2)
#' }
#'
#'
#' @export
#'
shrink <- function(preprocessor, new_data, outcome = FALSE) {

  new_data <- tibble::as_tibble(new_data)

  cols <- preprocessor$predictors
  validate_predictors(new_data, cols)

  validate_new_data_classes(new_data, preprocessor$predictor_classes)

  original_predictor_levels <- preprocessor$predictor_levels
  new_data <- check_new_data_factor_levels(original_predictor_levels, new_data)

  if (outcome) {

    outcome_cols <- preprocessor$outcomes
    validate_outcomes(new_data, outcome_cols)

    validate_new_data_classes(new_data, preprocessor$outcome_classes)

    original_outcome_levels <- preprocessor$outcome_levels
    new_data <- check_new_data_factor_levels(original_outcome_levels, new_data)

    cols <- c(outcome_cols, cols)
  }

  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, cols, drop = FALSE]

  new_data
}
