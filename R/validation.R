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
#' forge(test, processed$engine)
#'
#' # But if the outcome is requested, and `".outcome"`
#' # is not present in `new_data`, an error is thrown
#' # with very specific instructions
#' \dontrun{
#' forge(test, processed$engine, outcomes = TRUE)
#' }
#'
#' # To get this to work, just create an .outcome column in new_data
#' test$.outcome <- test$Species
#'
#' forge(test, processed$engine, outcomes = TRUE)
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

#' Ensure that `new_data` contains columns of the correct class
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - The classes of the columns of `new_data` must be exactly the same
#' as the classes defined by `original_classes`.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `incorrect_columns` A character vector. The columns that contain the
#' wrong class.
#'
#' @details
#'
#' This validation check is inspired by the base R functions
#' [stats::.MFclass()] and [stats::checkMFClasses()], but is more strict. See
#' [get_data_classes()] for more details.
#'
#' Ideally, this function is called after the following other validation
#' checks:
#'
#' - [validate_new_data_column_names()]
#'
#' If this is run first, then missing columns are taken care of.
#'
#' If this is not run first, then `validate_new_data_classes()` can still
#' be used, but any columns that exist in `original_classes` but not
#' in `new_data` will be silently skipped over.
#'
#' @param new_data A data frame of new predictors and possibly outcomes.
#'
#' @param original_classes A named list of the original classes of either the
#' outcomes or predictors. The names should match the column names in
#' `new_data`, and the values are character vectors of the original class
#' for that column.
#'
#' @template section-validation
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Example usage
#'
#' original_classes <- get_data_classes(train)
#'
#' # All good!
#' check_new_data_classes(test, original_classes)
#'
#' bad_test <- test
#' bad_test$Species <- as.character(bad_test$Species)
#'
#' # Species is incorrect!
#' check_new_data_classes(bad_test, original_classes)
#'
#' # The error also tells you what the class is, and what it should be
#' \dontrun{
#' validate_new_data_classes(bad_test, original_classes)
#' }
#'
#' @family validation functions
#' @export
validate_new_data_classes <- function(new_data, original_classes) {

  check <- check_new_data_classes(new_data, original_classes)

  if (!check$ok) {

    incorrect_columns <- check$incorrect_columns

    # Extract only the first class of everything for printing purposes
    new_data_classes <- vapply(new_data, class1, character(1))
    original_classes <- vapply(original_classes, function(x) x[1], character(1))

    wrong_class <- new_data_classes[incorrect_columns]
    right_class <- original_classes[incorrect_columns]

    errors <- glue::glue(
      "`{incorrect_columns}`: `{wrong_class}` should be `{right_class}`."
    )

    msg <- glue::glue_collapse(
      c("Some columns in `new_data` have an incorrect class:", errors),
      sep = "\n"
    )

    glubort(msg)

  }

  invisible(new_data)
}

#' @rdname validate_new_data_classes
#' @export
check_new_data_classes <- function(new_data, original_classes) {

  new_data <- check_is_data_like(new_data)
  validate_classes_list(original_classes, "original_classes")

  required_columns <- names(original_classes)

  check_class <- function(nm) {

    new_data_col <- new_data[[nm]]

    # Be robust against `original_classes` columns
    # that don't exist in `new_data`. It is not
    # check_new_data_classes() job to check for
    # that. It is the job of check_new_data_column_names()
    if (is.null(new_data_col)) {
      return(TRUE)
    }

    identical(
      get_data_class(new_data_col),
      original_classes[[nm]]
    )
  }

  ok_vec <- vapply(required_columns, check_class, logical(1))

  ok <- all(ok_vec)

  if (!ok) {
    incorrect_columns <- required_columns[!ok_vec]
  }
  else {
    incorrect_columns <- character()
  }

  check_list(ok = ok, incorrect_columns = incorrect_columns)
}

# ------------------------------------------------------------------------------

#' Ensure that predictions have the correct number of rows
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - The number of rows in `.pred` must be the same as the number
#' of rows in `new_data`.
#'
#' - `.pred` must be a data frame.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `n_new_data` A single numeric. The number of rows in `new_data`.
#'
#' - `n_pred` A single numeric. The number of rows in `.pred`.
#'
#' @param .pred A tibble. The predictions to return from any prediction
#' `type`. This is often created using one of the spruce functions, like
#' [spruce_response()].
#'
#' @param new_data A data frame of new predictors and possibly outcomes.
#'
#' @details
#'
#' This validation function is one that is more developer focused rather than
#' user focused. It is a final check to be used right before a value is
#' returned from your specific `predict()` method, and is mainly a "good
#' practice" sanity check to ensure that your prediction engine always returns
#' the same number of rows as `new_data`, which is one of the modeling
#' conventions this package tries to promote.
#'
#' @template section-validation
#'
#' @examples
#' # Say new_data has 5 rows
#' new_data <- mtcars[1:5,]
#'
#' # And somehow you generate predictions
#' # for those 5 rows
#' .pred_vec <- 1:5
#'
#' # Then you use `spruce_response()` to clean
#' # up these numeric predictions
#' .pred <- spruce_response(.pred_vec)
#'
#' .pred
#'
#' # Use this check to ensure that
#' # the number of rows or .pred match new_data
#' check_prediction_size(.pred, new_data)
#'
#' # An informative error message is thrown
#' # if the rows are different
#' \dontrun{
#' validate_prediction_size(spruce_response(1:4), new_data)
#' }
#'
#' @family validation functions
#' @export
validate_prediction_size <- function(.pred, new_data) {

  check <- check_prediction_size(.pred, new_data)

  if(!check$ok) {
    glubort(
      "The number of rows in `new_data` ({check$n_new_data}) must match the ",
      "number of rows in `.pred` ({check$n_pred})."
    )
  }

  invisible(.pred)
}

#' @rdname validate_prediction_size
#' @export
check_prediction_size <- function(.pred, new_data) {

  new_data <- check_is_data_like(new_data)

  if (!is.data.frame(.pred)) {
    glubort("`.pred` must be a data.frame.")
  }

  n_new_data <- nrow(new_data)
  n_pred <- nrow(.pred)

  ok <- n_pred == n_new_data

  check_list(ok = ok, n_new_data = n_new_data, n_pred = n_pred)

}

# ------------------------------------------------------------------------------

# ok = bool
# ... = extra info when not ok
check_list <- function(ok = TRUE, ...) {

  validate_is_bool(ok, "ok")
  elems <- rlang::list2(...)

  c(list(ok = ok), elems)
}

