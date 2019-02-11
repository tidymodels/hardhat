#' Subset only required predictors
#' 
#' `shrink()` first validates that all `predictors` exist in `new_data`, and
#' then subsets `new_data` to only contain those predictors.
#' 
#' @param object The model fit. If the default preprocessing was supplied, an 
#' `object$predictors` element should exist holding a character vector of the
#' predictor names. Otherwise, as long as `prepare()`
#' was called, then nothing else needs to be done.
#' @param new_data A data frame containing the new data that will be shrunk down
#' to only contain the predictors required for preprocessing and prediction.
#' 
#' @keywords internal
shrink <- function(object, new_data) {
  
  predictors <- extract_original_predictors(object)
  validate_predictors(new_data, predictors)
  
  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, predictors, drop = FALSE]
  
  new_data
}