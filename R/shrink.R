#' Subset only required columns
#'
#' @description
#'
#' `shrink()` subsets `data` to only contain the required columns specified by
#' the prototype, `ptype`.
#'
#' @details
#'
#' `shrink()` is called by [forge()] before [scream()] and before the actual
#' processing is done.
#'
#' @param data A data frame containing the data to subset.
#'
#' @param ptype A data frame prototype containing the required columns.
#'
#' @return
#'
#' A tibble containing the required columns.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100, ]
#' test <- iris[101:150, ]
#'
#' # ---------------------------------------------------------------------------
#' # shrink()
#'
#' # mold() is run at model fit time
#' # and a formula preprocessing blueprint is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Inside the result of mold() are the prototype tibbles
#' # for the predictors and the outcomes
#' ptype_pred <- x$blueprint$ptypes$predictors
#' ptype_out <- x$blueprint$ptypes$outcomes
#'
#' # Pass the test data, along with a prototype, to
#' # shrink() to extract the prototype columns
#' shrink(test, ptype_pred)
#'
#' # To extract the outcomes, just use the
#' # outcome prototype
#' shrink(test, ptype_out)
#'
#' # shrink() makes sure that the columns
#' # required by `ptype` actually exist in the data
#' # and errors nicely when they don't
#' test2 <- subset(test, select = -Species)
#' try(shrink(test2, ptype_pred))
#' @export
shrink <- function(data, ptype) {
  if (is.null(data)) {
    return(NULL)
  }

  data <- check_is_data_like(data)

  cols <- colnames(ptype)
  validate_column_names(data, cols)

  out <- data[, cols, drop = FALSE]

  out
}
