#' Ensure that the outcome is univariate
#'
#' @description
#'
#' The following validation checks are performed:
#'
#' - `outcomes` must be a data frame
#'
#' - `outcomes` must have 1 column
#'
#' @param outcomes A data frame with 1 column.
#'
#' @details
#'
#' The easiest way to use this validation function is to supply it the
#' `$outcomes` element of the result of [mold()].
#'
#' @examples
#'
#' validate_outcome_is_univariate(data.frame(x = 1))
#'
#' \dontrun{
#' validate_outcome_is_univariate(mtcars)
#' }
#'
#' @family validation functions
#' @export
validate_outcome_is_univariate <- function(outcomes) {

  validate_outcome_is_data_frame(outcomes)

  n_cols <- ncol(outcomes)

  if (n_cols != 1L) {

    glubort(
      "There must only be 1 outcome, not {n_cols}."
    )

  }

  invisible(outcomes)
}

validate_outcome_is_data_frame <- function(outcomes) {

  if (!is.data.frame(outcomes)) {
    glubort(
      "The outcome must be a data.frame, not a {class1(outcomes)}."
    )
  }

  invisible(outcomes)
}
