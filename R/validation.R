#' Ensure that the outcome is univariate
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `outcomes` must have 1 column.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `n_cols` A single numeric. The number of columns.
#'
#' @param outcomes Ideally, a data frame with 1 column.
#'
#' @details
#'
#' The easiest way to use this validation function is to supply it the
#' `$outcomes` element of the result of [mold()].
#'
#' @examples
#'
#' validate_outcomes_is_univariate(data.frame(x = 1))
#'
#' \dontrun{
#' validate_outcomes_is_univariate(mtcars)
#' }
#'
#' @family validation functions
#' @export
validate_outcomes_is_univariate <- function(outcomes) {

  check <- check_outcomes_is_univariate(outcomes)

  if (!check$ok) {

    glubort(
      "There must only be 1 outcome, not {check$n_cols}."
    )

  }

  invisible(outcomes)
}

#' @rdname validate_outcomes_is_univariate
#' @export
check_outcomes_is_univariate <- function(outcomes) {

  if (!rlang::is_vector(outcomes)) {
    glubort("`outcomes` must be a vector, not a {class1(outcomes)}.")
  }

  n_cols <- NCOL(outcomes)

  ok <- (n_cols == 1L)

  check <- check_list(ok = ok, n_cols = n_cols)

  check
}

# ------------------------------------------------------------------------------

# ok = bool
# ... = extra info when not ok
check_list <- function(ok = TRUE, ...) {

  validate_is_bool(ok, "ok")
  elems <- rlang::list2(...)

  c(list(ok = ok), elems)
}

