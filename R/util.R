glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue(..., .sep = .sep, .envir = .envir))
}

glue_quote_collapse <- function(x) {
  glue::glue_collapse(glue::single_quote(x), sep = ", ")
}

# `NA` values are `FALSE`
strict_equal <- function(x, y) {
  vapply(x == y, isTRUE, logical(1))
}

validate_empty_dots <- function(...) {
  dots <- list(...)

  n_dots <- length(dots)

  if (n_dots != 0L) {
    dot_nms <- names(exprs_auto_name(dots))
    dot_nms <- glue_quote_collapse(dot_nms)

    glubort(
      "`...` must not contain any input. ",
      "{n_dots} elements were found in the dots with names: {dot_nms}."
    )
  }

  invisible()
}

simplify_terms <- function(x) {

  # This is like stats:::terms.default
  # but doesn't look at x$terms.

  is_terms <- inherits(x, "terms")

  if (!is_terms) {
    abort("`x` must be a terms object")
  }

  # It removes the environment
  # (which could be large)
  # - it is not needed for prediction
  # - it is used in model.matrix(data = environment(object))
  #   but you should never need that
  # - I guess it could be used to look up global variables in a formula,
  #   but don't we want to guard against that?
  # - It is used in model.frame() to evaluate the predvars, but that is also
  #   evaluated in the presence of the data so that should always suffice?
  attr(x, ".Environment") <- NULL

  x
}

# - RHS `.` should be expanded ahead of time by `expand_formula_dot_notation()`
# - Can't use `get_all_vars()` because it chokes on formulas with variables with
#   spaces like ~ `x y`
get_all_predictors <- function(formula, data) {
  predictor_formula <- new_formula(
    lhs = NULL,
    rhs = f_rhs(formula),
    env = f_env(formula)
  )

  predictors <- all.vars(predictor_formula)

  extra_predictors <- setdiff(predictors, names(data))
  if (length(extra_predictors) > 0) {
    extra_predictors <- glue_quote_collapse(extra_predictors)
    glubort("The following predictors were not found in `data`: {extra_predictors}.")
  }

  predictors
}

# LHS `.` are NOT expanded by `expand_formula_dot_notation()`, and should be
# considered errors
get_all_outcomes <- function(formula, data) {
  outcome_formula <- new_formula(
    lhs = f_lhs(formula),
    rhs = 1,
    env = f_env(formula)
  )

  outcomes <- all.vars(outcome_formula)

  if ("." %in% outcomes) {
    abort("The left hand side of the formula cannot contain `.`")
  }

  extra_outcomes <- setdiff(outcomes, names(data))
  if (length(extra_outcomes) > 0) {
    extra_outcomes <- glue_quote_collapse(extra_outcomes)
    glubort("The following outcomes were not found in `data`: {extra_outcomes}.")
  }

  outcomes
}

abort_unknown_mold_class <- function(x) {
  cls <- class(x)[1]
  glubort(
    "`x` is not a recognized type.
     Only data.frame, matrix, recipe, and formula objects are allowed.
     A {cls} was specified."
  )
}

remove_formula_intercept <- function(formula, intercept) {
  if (intercept) {
    return(formula)
  }

  rhs <- f_rhs(formula)
  lhs <- f_lhs(formula)
  env <- f_env(formula)

  rhs <- expr(!!rhs + 0)

  new_formula(
    lhs = lhs,
    rhs = rhs,
    env = env
  )
}

has_unique_names <- function(x) {
  nms <- names(x)

  if (length(nms) != length(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

has_unique_column_names <- function(x) {
  nms <- colnames(x)

  if (length(nms) != NCOL(x)) {
    return(FALSE)
  }

  if (any(is.na(nms) | nms == "")) {
    return(FALSE)
  }

  !anyDuplicated(nms)
}

class1 <- function(x) {
  class(x)[1]
}

is_bool <- function(x) {
  is_logical(x, n = 1) && !is.na(x)
}

validate_is_bool <- function(.x, .x_nm) {
  if (is_missing(.x_nm)) {
    .x_nm <- as_label(enexpr(.x))
  }

  validate_is(.x, is_bool, "bool", .x_nm, .note = "'TRUE' / 'FALSE'")
}

# ------------------------------------------------------------------------------

is_new_data_like <- function(x) {
  is.data.frame(x) || is.matrix(x)
}

validate_is_new_data_like <- function(new_data) {
  validate_is(
    new_data,
    is_new_data_like,
    "data.frame or matrix"
  )
}

check_is_data_like <- function(.x, .x_nm) {
  if (is_missing(.x_nm)) {
    .x_nm <- as_label(enexpr(.x))
  }

  if (!is_new_data_like(.x)) {
    glubort(
      "`{.x_nm}` must be a data.frame or a matrix, not a {class1(.x)}."
    )
  }

  tibble::as_tibble(.x)
}
