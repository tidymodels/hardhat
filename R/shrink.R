#' Subset only required columns
#'
#' `shrink()` first validates that all `predictors` exist in `new_data`, and
#' then subsets `new_data` to only contain those predictors. If `outcome` is
#' `TRUE`, then the original outcome columns are retained as well.
#'
#' `shrink()` is called by [preprocess()] before the processing is done.
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

  cols <- extract_original_predictors(preprocessor)
  validate_predictors(new_data, cols)

  if (outcome) {
    outcome_cols <- extract_original_outcomes(preprocessor)
    validate_outcomes(new_data, outcome_cols)
    cols <- c(outcome_cols, cols)
  }

  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, cols, drop = FALSE]

  new_data
}
