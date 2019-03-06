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

validate_has_unique_names <- function(x, x_nm) {

  if (!has_unique_names(x)) {
    glubort(
      "All elements of `{x_nm}` must have unique names."
    )
  }

  invisible(x)
}

validate_has_unique_column_names <- function(x, x_nm) {

  if (!has_unique_column_names(x)) {
    glubort(
      "All columns of `{x_nm}` must have unique names."
    )
  }

  invisible(x)
}

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

validate_recipes_available <- function() {

  if (!requireNamespace("recipes", quietly = TRUE)) {

    abort(
      "The `recipes` package must be available to use the recipe interface."
    )

  }

  invisible()
}

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
