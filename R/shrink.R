#' Subset only required columns
#'
#' @description
#'
#' `shrink()` subsets `new_data` to only contain the required columns.
#' `shrink()` also performs the following validation checks on `new_data`:
#'
#' - Checks that all required predictor columns exist in `new_data`.
#'
#' @details
#'
#' If `outcome = TRUE`, then the validation checks are performed on the known
#' outcome column as well, and it is returned along with the predictors. If
#' [mold()] was called with the XY interface, then no preprocessing was done
#' to `y` and `outcome` will have no effect (if a vector was passed as `y`
#' during the fit, `shrink()` has no way of knowing what column in `new_data`
#' corresponds to the outcome).
#'
#' `shrink()` is called by [forge()] before [scream()] and before the actual
#' processing is done.
#'
#' @param preprocessor A `"preprocessor"`.
#'
#' @param new_data A data frame at least containing the new predictors, and
#' potentially the outcomes if `outcome = TRUE`.
#'
#' @param outcome A logical. Should the outcome be included in the shrunk
#' `new_data` output as well?
#'
#' @return
#'
#' A tibble containing the required predictors (and potentially the
#' outcomes).
#'
#' @examples
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # mold() is run at model fit time
#' # and a terms preprocessor is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Pass that preprocessor to shrink(), along with new_data
#' # to get a tibble of required predictors back
#' shrink(x$preprocessor, test)
#'
#' # outcome = TRUE also returns the outcome,
#' # this can be useful if doing cross validation
#' shrink(x$preprocessor, test, outcome = TRUE)
#'
#' # forge() actually preprocesses the new_data, after
#' # a call to shrink(). Notice how now the Sepal.Width
#' # column is logged
#' forge(x$preprocessor, test, outcome = TRUE)$outcomes
#'
#' @export
shrink <- function(preprocessor, new_data, outcome = FALSE) {

  new_data <- tibble::as_tibble(new_data)

  cols <- preprocessor$predictors
  validate_predictors(new_data, cols)

  if (outcome) {

    outcome_cols <- preprocessor$outcomes
    validate_outcomes(new_data, outcome_cols)

    cols <- c(outcome_cols, cols)

    # For the rare chance that an outcome
    # is also a predictor
    cols <- unique(cols)

  }

  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, cols, drop = FALSE]

  new_data
}
