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
#' @template section-validation
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

validate_new_data_column_names <- function(new_data, original_names) {

  check <- check_new_data_column_names(new_data, original_names)

  if (!check$ok) {

    validate_missing_name_isnt_.outcome(check$missing_names)

    missing_names <- glue_quote_collapse(check$missing_names)

    glubort(
      "`new_data` is missing the following required columns:
      {missing_names}."
    )

  }

  invisible(new_data)
}

check_new_data_column_names <- function(new_data, original_names) {

  new_data <- check_is_data_like(new_data)

  if (!is.character(original_names)) {
    glubort("`original_names` must be a character vector.")
  }

  new_names <- colnames(new_data)

  has_names <- original_names %in% new_names

  ok <- all(has_names)

  if (!ok) {
    missing_names <- original_names[!has_names]
  }
  else {
    missing_names <- character()
  }

  check_list(ok = ok, missing_names = missing_names)
}

validate_missing_name_isnt_.outcome <- function(missing_names) {

  not_ok <- ".outcome" %in% missing_names

  if (not_ok) {

    missing_names <- glue_quote_collapse(missing_names)

    glubort(
      "`new_data` is missing the following required columns:
      {missing_names}

      (This indicates that `mold()` was called with a vector for `y`. ",
      "When this is the case, and the outcome columns are requested ",
      "in `forge()`, `new_data` must include a column with the automatically ",
      "generated name '.outcome' containing the outcome.)"
    )

  }

  invisible(missing_names)
}

# ------------------------------------------------------------------------------

# ok = bool
# ... = extra info when not ok
check_list <- function(ok = TRUE, ...) {

  validate_is_bool(ok, "ok")
  elems <- rlang::list2(...)

  c(list(ok = ok), elems)
}

