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

#' @rdname utilities
extract_terms <- function(x) {

  # This is like stats:::terms.default
  # but doesn't look at x$terms.

  terms_value <- attr(x, "terms")

  if (is.null(terms_value)) {
    abort("No `terms` attribute found.")
  }

  # It also removes the environment
  # (which could be large)
  # - it is not needed for prediction
  # - it is used in model.matrix(data = environment(object))
  #   but you should never need that
  # - I guess it could be used to look up global variables in a formula,
  #   but don't we want to guard against that?
  # - It is used in model.frame() to evaluate the predvars, but that is also
  #   evaluated in the presence of the data so that should always suffice?
  attr(terms_value, ".Environment") <- NULL

  terms_value
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

# similar to delete.response()
# but also removes the dataClasses element
# corresponding to y if it exists
# http://r.789695.n4.nabble.com/delete-response-leaves-response-in-attribute-dataClasses-td4266902.html

#' @rdname utilities
delete_response <- function(x) {
  resp <- attr(x, "response")
  data_class <- attr(x, "dataClasses")

  # Remove dataClass corresponding to y
  # if it exists
  if (!is.null(resp)) {
    if (!is.null(data_class)) {
      attr(x, "dataClasses") <- data_class[-resp]
    }
  }

  delete.response(x)
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

add_intercept_column <- function(x, add) {
  if (add == FALSE) {
    return(x)
  }

  # mold() has already been run
  if ("(Intercept)" %in% colnames(x)) {
    return(x)
  }

  if (is.matrix(x)) {
    n <- nrow(x)
    dim_names <- list(NULL, "(Intercept)")
    new_col <- matrix(1L, nrow = n, dimnames = dim_names)
    x <- cbind(new_col, x)
  }

  if (is.data.frame(x)) {
    x <- tibble::add_column(x, `(Intercept)` = 1L, .before = 1L)
  }

  x
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

  validate_is(.x, is_bool, "bool", .x_nm, .note = "({ui_value('TRUE')} / {ui_value('FALSE')})")
}

all_numeric <- function(x) {
  all(vapply(x, is.numeric, logical(1)))
}

# - Preprocessing should _never_ removes rows
# with incomplete data. Setting the na.action
# to na.pass will retain the NA values through
# the preprocessing
model_frame <- function(formula, data, predictor_levels = NULL) {
  rlang::with_options(
    stats::model.frame(formula, data = data, xlev = predictor_levels),
    na.action = "na.pass"
  )
}
