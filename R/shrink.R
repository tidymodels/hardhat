#' Subset only required columns
#'
#' @description
#'
#' `shrink()` subsets `new_data` to only contain the required columns.
#' `shrink()` also performs the following validation checks on `new_data`:
#'
#' - [validate_new_data_column_names()] - Checks that all required columns
#' exist in `new_data`.
#'
#' @details
#'
#' If `outcomes = TRUE`, then the validation checks are performed on the known
#' outcome columns as well, and they are returned along with the predictors. If
#' [mold()] was called with the XY interface, then no preprocessing was done
#' to `y` and `outcomes` will have no effect (if a vector was passed as `y`
#' during the fit, `shrink()` has no way of knowing what column in `new_data`
#' corresponds to the outcome).
#'
#' `shrink()` is called by [forge()] before [scream()] and before the actual
#' processing is done.
#'
#' @inheritParams scream
#'
#' @param outcomes A logical. Should the outcomes be included in the shrunk
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
#' # and a formula preprocessing engine is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Pass that engine to shrink(), along with new_data
#' # to get a tibble of required predictors back
#' shrink(test, x$engine)
#'
#' # outcomes = TRUE also returns the outcomes,
#' # this can be useful if doing cross validation
#' shrink(test, x$engine, outcomes = TRUE)
#'
#' # forge() actually preprocesses the new_data, after
#' # a call to shrink(). Notice how now the Sepal.Width
#' # column is logged
#' forge(test, x$engine, outcomes = TRUE)$outcomes
#'
#' @export
shrink <- function(new_data, engine, outcomes = FALSE) {

  new_data <- check_is_data_like(new_data)

  cols <- colnames(engine$info$predictors)
  validate_new_data_column_names(new_data, cols)

  if (outcomes) {

    outcome_cols <- colnames(engine$info$outcomes)
    validate_new_data_column_names(new_data, outcome_cols)

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
