#' Utilites
#'
#' Helper functions that are used internally.
#'
#' @keywords internal
#' @name utilities
NULL

#' @rdname utilities
glubort <- function(..., .sep = "", .envir = parent.frame()) {
  abort(glue(..., .sep = .sep, .envir = .envir))
}

glue_quote_collapse <- function(x) {
  glue::glue_collapse(glue::single_quote(x), sep = ", ")
}

#' @rdname utilities
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

get_all_predictors <- function(formula, data) {
  predictor_formula <- rlang::new_formula(
    lhs = NULL,
    rhs = rlang::f_rhs(formula),
    env = rlang::f_env(formula)
  )

  unprocessed_predictor_df <- get_all_vars(predictor_formula, data)

  colnames(unprocessed_predictor_df)
}

get_all_outcomes <- function(formula, data) {
  outcome_formula <- rlang::new_formula(
    lhs = rlang::f_lhs(formula),
    rhs = 1,
    env = rlang::f_env(formula)
  )

  unprocessed_outcome_df <- get_all_vars(outcome_formula, data)

  colnames(unprocessed_outcome_df)
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

  rhs <- rlang::f_rhs(formula)
  lhs <- rlang::f_lhs(formula)
  env <- rlang::f_env(formula)

  rhs <- rlang::expr(!!rhs + 0)

  rlang::new_formula(
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

is_bool <- function (x) {
  rlang::is_logical(x, n = 1) && !is.na(x)
}

validate_is_bool <- function(.x, .x_nm) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  validate_is(.x, is_bool, "bool", .x_nm, .note = "'TRUE' / 'FALSE'")
}

all_numeric <- function(x) {
  all(vapply(x, is.numeric, logical(1)))
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

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  if (!is_new_data_like(.x)) {
    glubort(
      "`{.x_nm}` must be a data.frame or a matrix, not a {class1(.x)}."
    )
  }

  tibble::as_tibble(.x)
}

# ------------------------------------------------------------------------------
