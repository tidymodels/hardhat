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
validate_inherits <- function(x, x_nm, cls) {
  if (!inherits(x, cls)) {
    cls <- glue::glue_collapse(cls, ", ", last = ", or ")
    glubort("`{x_nm}` must inherit from {cls}, not {class(x)[1]}.")
  }
}

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
validate_length <- function(.x, .x_nm, .n = 1L) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  .n_x <- length(.x)
  ok <- .n_x == .n

  if (!ok) {

    glubort("{.x_nm} should be length {.n}, not length {.n_x}.")

  }

  invisible(.x)
}

#' @rdname validation
validate_mode <- function(mode) {
  ok <- mode %in% c("classification", "regression")
  if (!ok) {
    abort("`mode` must be one of: 'classification', 'regression'.")
  }
}

#' @rdname validation
validate_variateness <- function(variateness) {
  ok <- variateness %in% c("univariate", "multivariate")
  if (!ok) {
    abort("`variateness` must be one of: 'univariate', 'multivariate'.")
  }
}

#' @rdname validation
validate_has_named_columns <- function(x, x_nm) {
  x_nms <- colnames(x)

  if (is.null(x_nms)) {
    glubort("All columns of `{x_nm}` must be named.")
  }

  if (length(x_nms) != length(unique(x_nms))) {
    glubort("All columns of `{x_nm}` must have unique names.")
  }
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
validate_x_has_numeric_cols <- function(x) {
  all_cols_numeric <- all_numeric(x)
  if (!all_cols_numeric) {
    abort(
      "All columns of `x` must be numeric for this model.
      Please use the recipe or formula interface for
      automatic creation of dummy variables."
    )
  }
}

#' @rdname validation
validate_y_univariate <- function(y) {
  if (NCOL(y) > 1L) {
    abort("`y` must be univariate for this model.")
  }
}

#' @rdname validation
validate_predictors <- function(new_data, predictors) {
  new_predictors <- colnames(new_data)

  has_predictors <- predictors %in% new_predictors
  if (!all(has_predictors)) {
    missing_predictors <- glue::glue_collapse(predictors[!has_predictors], ", ", last = ", and ")
    glubort(
      "`new_data` is missing the following required predictors:
      {missing_predictors}."
    )
  }
}

#' @rdname validation
validate_recipes_available <- function() {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    abort("The `recipes` package must be available to use the recipe interface.")
  }
}

#' @rdname validation
validate_prediction_size <- function(.pred, new_data) {

  n_new <- nrow(new_data)
  n_pred <- nrow(.pred)

  if(n_pred != n_new) {
    glubort(
      "The number of rows in `new_data` ({n_new}) must match the number of rows in `.pred` ({n_pred})."
    )
  }
}

#' @rdname validation
validate_new_data_classes <- function(terms, new_model_frame) {

  # model.frame() was run directly before this, so we are ensured
  # that all of the columns required in terms exist in new_model_frame

  required_classes <- attr(terms, "dataClasses")
  required_names <- names(required_classes)

  # Extract from dataClasses by name (more robust than by position!)
  # Use of .MFclass() because that is what terms uses.
  # Required because integer columns are reported as numeric by .MFclass
  check_class <- function(nm) {
    identical(
      .MFclass(new_model_frame[[nm]]),
      required_classes[[nm]]
    )
  }

  ok <- vapply(required_names, check_class, logical(1))

  if (!all(ok)) {

    first_class <- function(x) class(x)[1]

    all_classes <- vapply(new_model_frame, first_class, character(1))

    wrong_class <- all_classes[!ok]
    right_class <- required_classes[!ok]
    right_name <- required_names[!ok]

    errors <- glue::glue("`{right_name}`: `{wrong_class}` should be `{right_class}`.")
    msg <- glue::glue_collapse(c("Some columns in `new_data` have an incorrect class:", errors), sep = "\n")
    glubort(msg)
  }

  invisible(NULL)
}

#' @rdname validation
check_new_data_factor_levels <- function(x_levels, new_data) {

  fct_cols <- names(x_levels)

  for(col in fct_cols) {

    lvls <- x_levels[[col]]
    new_col <- new_data[[col]]

    # If the new_data column is not a factor (or doesnt exist),
    # move on and let the next steps handle any appropriate
    # erroring
    if (!is.factor(new_col)) {
      next
    }

    new_lvls <- levels(new_col)
    unseen_lvls <- setdiff(new_lvls, lvls)

    if (length(unseen_lvls) > 0) {

      unseen_lvls <- glue::glue_collapse(dQuote(unseen_lvls), ", ")

      warning(glue(
        "The following factor levels are new for column, `{col}`, and have been coerced to `NA`: {unseen_lvls}."
      ), call. = FALSE)

      seen_lvls <- intersect(new_lvls, lvls)
      new_data[[col]] <- factor(new_col, levels = seen_lvls)
    }

  }

  new_data
}

#' @rdname validation
validate_predictor_arguments <- function(x, x_nm = "x", intercept = FALSE) {

  validate_has_named_columns(x, x_nm)
  validate_intercept(intercept)

}

#' @rdname validation
validate_intercept <- function(intercept) {
  if (!is.logical(intercept)) {
    cls <- first_class(intercept)
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

  if (identical(cll_fn, `+`)) {
    for(arg in cll_args) {
      if (arg == 0L) {
        rlang::abort("`formula` must not contain the intercept removal term: `+ 0` or `0 +`.")
      }
    }
  }

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

  for(arg in cll_args) {
    recurse_intercept_search(arg)
  }

  invisible(x)
}