validate_is <- function(.x, .f, .expected, .x_nm, .note = "") {
  if (is_missing(.x_nm)) {
    .x_nm <- as_label(enexpr(.x))
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

validate_recipes_available <- function() {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    abort(
      "The `recipes` package must be available to use the recipe interface."
    )
  }

  invisible()
}

# the formula must have an implicit intercept to remove
# dont let the user do `0+` or `+0` or `-1`

validate_formula_has_intercept <- function(formula) {
  formula <- f_rhs(formula)

  validate_not_1_or_0(formula)

  recurse_intercept_search(formula)
}

validate_not_1_or_0 <- function(formula) {
  if (!is_scalar_integerish(formula)) {
    return(invisible(formula))
  }

  if (formula == 1) {
    glubort(
      "`formula` must not contain the intercept term, `1`."
    )
  }

  if (formula == 0) {
    glubort(
      "`formula` must not contain the intercept removal term, `0`."
    )
  }

  invisible(formula)
}

recurse_intercept_search <- function(x) {
  if (!is_call(x)) {
    return(invisible(x))
  }

  cll_fn <- call_fn(x)
  cll_args <- call_args(x)

  # Check for `+ 0` or `0 +`
  if (identical(cll_fn, `+`)) {
    for (arg in cll_args) {
      if (arg == 0L) {
        abort(
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
      abort("`formula` must not contain the intercept removal term: `- 1`.")
    }
  }

  # Recurse
  for (arg in cll_args) {
    recurse_intercept_search(arg)
  }

  invisible(x)
}
