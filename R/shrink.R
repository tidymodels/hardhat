#' Subset only required columns
#'
#' `shrink()` first validates that all `predictors` exist in `new_data`, and
#' then subsets `new_data` to only contain those predictors. If `outcome` is
#' `TRUE`, then the original outcome columns are retained as well.
#'
#' `shrink()` is called by [preprocess()] before the processing is done.
#'
#' `shrink()` also checks that all of the `new_data` factor columns don't have
#' any _new_ levels when compared to the original data used in training. If
#' there are new levels, they are replaced with `NA` values and a warning is
#' thrown. If `outcome = TRUE`, it checks the outcome levels as well.
#'
#' @param preprocessor A `"preprocessor"`.
#'
#' @param new_data A data frame containing the new data that will be shrunk down
#' to only contain the predictors required for preprocessing and prediction.
#'
#' @param outcome A logical. Should the outcome be included in the shrunk
#' `new_data` as well?
#'
#' @export
#'
shrink <- function(preprocessor, new_data, outcome = FALSE) {

  cols <- preprocessor$predictors
  validate_predictors(new_data, cols)

  original_predictor_levels <- preprocessor$predictor_levels
  new_data <- check_new_data_factor_levels(original_predictor_levels, new_data)

  if (outcome) {

    outcome_cols <- preprocessor$outcomes
    validate_outcomes(new_data, outcome_cols)

    original_outcome_levels <- preprocessor$outcome_levels
    new_data <- check_new_data_factor_levels(original_outcome_levels, new_data)

    cols <- c(outcome_cols, cols)
  }

  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, cols, drop = FALSE]

  new_data
}
