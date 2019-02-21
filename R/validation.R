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

#' Ensure that `new_data` contains required column names
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - The column names of `new_data` must contain all `original_names`.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `missing_names` A character vector. The missing column names.
#'
#' @details
#'
#' A special error is thrown if the missing column is named `".outcome"`. This
#' only happens in the case where [mold()] is called using the xy-method, and
#' a _vector_ `y` value is supplied rather than a data frame or matrix. In that
#' case, `y` is coerced to a data frame, and the automatic name `".outcome"` is
#' added, and this is what is looked for in [forge()]. If this happens and the
#' user tries to request outcomes using `forge(..., outcomes = TRUE)` but
#' their `new_data` does not contain the required `".outcome"` column, a special
#' error is thrown telling them what to do. See the examples!
#'
#' @param new_data A data frame of new predictors and possibly outcomes.
#'
#' @param original_names A character vector. The original column names used
#' in training.
#'
#' @template section-validation
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#'
#' original_names <- colnames(mtcars)
#'
#' test <- mtcars
#' bad_test <- test[, -c(3, 4)]
#'
#' # All good
#' check_new_data_column_names(test, original_names)
#'
#' # Missing 2 columns
#' check_new_data_column_names(bad_test, original_names)
#'
#' \dontrun{
#' # Will error
#' validate_new_data_column_names(bad_test, original_names)
#' }
#'
#' # ---------------------------------------------------------------------------
#' # Special error when `.outcome` is missing
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' train_x <- subset(train, select = -Species)
#' train_y <- train$Species
#'
#' # Here, y is a vector
#' processed <- mold(train_x, train_y)
#'
#' # So the default column name is `".outcome"`
#' processed$outcomes
#'
#' # It doesn't affect forge() normally
#' forge(processed$preprocessor, test)
#'
#' # But if the outcome is requested, and `".outcome"`
#' # is not present in `new_data`, an error is thrown
#' # with very specific instructions
#' \dontrun{
#' forge(processed$preprocessor, test, outcomes = TRUE)
#' }
#'
#' # To get this to work, just create an .outcome column in new_data
#' test$.outcome <- test$Species
#'
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' @family validation functions
#' @export
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

#' @rdname validate_new_data_column_names
#' @export
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

