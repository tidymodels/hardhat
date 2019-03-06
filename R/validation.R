#' Ensure that `data` is univariate
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `data` must have 1 column. Atomic vectors are treated as 1 column matrices.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `n_cols` A single numeric. The actual number of columns.
#'
#' @param data An object to check.
#'
#' @param name A name to be used in the default error message.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$outcomes` element of the result of a call to [mold()].
#'
#' @examples
#' validate_is_univariate(data.frame(x = 1))
#'
#' \dontrun{
#' validate_is_univariate(mtcars)
#' }
#'
#' @family validation functions
#' @export
validate_is_univariate <- function(data, name = "data") {

  validate_is_name(name, "name")
  check <- check_is_univariate(data)

  if (!check$ok) {
    glubort(
      "`{name}` must be univariate, but {check$n_cols} columns were found."
    )
  }

  invisible(data)
}

#' @rdname validate_is_univariate
#' @export
check_is_univariate <- function(data) {

  if (!rlang::is_vector(data)) {
    n_cols <- 0L
  }
  else {
    n_cols <- NCOL(data) %||% 0L
  }

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

# ------------------------------------------------------------------------------

validate_is_name <- function(.x, .x_nm) {
  validate_is_character(.x, .x_nm)

  if (length(.x) != 1L) {
    glubort(
      "`{.x_nm}` must be of size 1, not {length(.x)}."
    )
  }

  invisible(.x)
}
