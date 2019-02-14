#' Validation
#'
#' Various input validation helpers. These should not be exported.
#'
#' `@keywords internal` is used to generate documentation even though the
#' function is not exposed to the user. This is helpful for developers as it
#' allows you to document your internal functions.
#'
#' @keywords internal
#' @name validation
NULL

#' @rdname validation
validate_is <- function(.x, .f, .expected, .x_nm, .note = "") {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  ok <- .f(.x)

  if (!ok) {

    if (!identical(.note, "")) {
      .note <- glue::glue(" (", .note, ")")
    }

    .actual <- class1(.x)

    glubort(
      "{.x_nm} should be a {.expected}{.note}, ",
      "not a {.actual}."
    )
  }

  invisible(.x)
}

#' @rdname validation
validate_has_unique_names <- function(x, x_nm) {

  if (!has_unique_names(x)) {
    glubort(
      "All elements of `{x_nm}` must have unique names."
    )
  }

  invisible(x)
}

#' @rdname validation
validate_has_unique_column_names <- function(x, x_nm) {

  if (!has_unique_column_names(x)) {
    glubort(
      "All columns of `{x_nm}` must have unique names."
    )
  }

  invisible(x)
}

#' @rdname validation
validate_x_has_numeric_cols <- function(x) {

  all_cols_numeric <- all_numeric(x)

  if (!all_cols_numeric) {

    abort(
      "All columns of `x` must be numeric for this model.
      Please use the recipe or formula interface for
      automatic creation of dummy variables."
    )

  }

  invisible(x)
}

#' @rdname validation
validate_y_univariate <- function(y) {

  if (NCOL(y) > 1L) {
    abort("`y` must be univariate for this model.")
  }

  invisible(y)
}

#' @rdname validation
validate_predictors <- function(new_data, predictors) {

  new_data_cols <- colnames(new_data)

  has_predictors <- predictors %in% new_data_cols

  if (!all(has_predictors)) {

    missing_predictors <- glue::glue_collapse(
      x = predictors[!has_predictors],
      sep = ", ",
      last = ", and "
    )

    glubort(
      "`new_data` is missing the following required predictors:
      {missing_predictors}"
    )

  }

  invisible(new_data)
}

#' @rdname validation
validate_outcomes <- function(new_data, outcomes) {

  new_data_cols <- colnames(new_data)

  has_outcomes <- outcomes %in% new_data_cols

  if (!all(has_outcomes)) {

    missing_outcomes <- glue::glue_collapse(
      x = outcomes[!has_outcomes],
      sep = ", ",
      last = ", and "
    )

    glubort(
      "`new_data` is missing the following required outcomes:
      {missing_outcomes}"
    )

  }

  invisible(new_data)
}

#' @rdname validation
validate_recipes_available <- function() {

  if (!requireNamespace("recipes", quietly = TRUE)) {

    abort(
      "The `recipes` package must be available to use the recipe interface."
    )

  }

  invisible()
}

#' @rdname validation
validate_prediction_size <- function(.pred, new_data) {

  n_new <- nrow(new_data)
  n_pred <- nrow(.pred)

  if(n_pred != n_new) {
    glubort(
      "The number of rows in `new_data` ({n_new}) must match the ",
      "number of rows in `.pred` ({n_pred})."
    )
  }

  invisible(.pred)
}

#' @rdname validation
validate_new_data_classes <- function(new_data, original_classes) {

  # new_data is a tibble at this point

  required_columns <- names(original_classes)

  check_class <- function(nm) {
    identical(
      get_data_class(new_data[[nm]]),
      original_classes[[nm]]
    )
  }

  ok <- vapply(required_columns, check_class, logical(1))

  if (!all(ok)) {

    bad_columns <- required_columns[!ok]

    # Extract only the first class of everything for printing purposes
    new_data_classes <- vapply(new_data, class1, character(1))
    original_classes <- vapply(original_classes, function(x) x[1], character(1))

    wrong_class <- new_data_classes[bad_columns]
    right_class <- original_classes[bad_columns]

    errors <- glue::glue(
      "`{bad_columns}`: `{wrong_class}` should be `{right_class}`."
    )

    msg <- glue::glue_collapse(
      c("Some columns in `new_data` have an incorrect class:", errors),
      sep = "\n"
    )

    glubort(msg)

  }

  invisible(new_data)
}

#' @rdname validation
validate_intercept <- function(intercept) {

  if (!is.logical(intercept)) {
    cls <- class1(intercept)
    glubort("`intercept` must be a logical, not a {cls}.")
  }

  n <- length(intercept)
  if (!(n == 1L)) {
    glubort("`intercept` must have length 1, not {n}.")
  }

  invisible(intercept)
}


# the formula must have an implicit intercept to remove
# dont let the user do `0+` or `+0` or `-1`

#' @rdname validation
validate_formula_has_intercept <- function(formula) {

  formula <- rlang::f_rhs(formula)

  recurse_intercept_search(formula)
}

recurse_intercept_search <- function(x) {

  if (!rlang::is_call(x)) {
    return(invisible(x))
  }

  cll_fn <- rlang::call_fn(x)
  cll_args <- rlang::call_args(x)

  # Check for `+ 0` or `0 +`
  if (identical(cll_fn, `+`)) {
    for(arg in cll_args) {
      if (arg == 0L) {
        rlang::abort(
          "`formula` must not contain the intercept removal term: `+ 0` or `0 +`."
        )
      }
    }
  }

  # Check for `- 1`
  if (identical(cll_fn, `-`)) {

    if (length(cll_args) == 2L) {
      arg <- cll_args[[2]]
    }

    if (length(cll_args) == 1L) {
      arg <- cll_args[[1]]
    }

    if (arg == 1L) {
      rlang::abort("`formula` must not contain the intercept removal term: `- 1`.")
    }
  }

  # Recurse
  for(arg in cll_args) {
    recurse_intercept_search(arg)
  }

  invisible(x)
}
