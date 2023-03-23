glubort <- function(..., .sep = "", .envir = caller_env(), .call = .envir) {
  abort(glue(..., .sep = .sep, .envir = .envir), call = .call)
}

glue_quote_collapse <- function(x) {
  glue::glue_collapse(glue::single_quote(x), sep = ", ")
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

check_unique_names <- function(x,
                               ...,
                               arg = caller_arg(x),
                               call = caller_env()) {
  if (has_unique_names(x)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "All elements of {.arg {arg}} must have unique names.",
    call = call
  )
}

check_unique_column_names <- function(x,
                                      ...,
                                      arg = caller_arg(x),
                                      call = caller_env()) {
  if (has_unique_column_names(x)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "All columns of {.arg {arg}} must have unique names.",
    call = call
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

# ------------------------------------------------------------------------------

check_data_frame_or_matrix <- function(x,
                                       ...,
                                       arg = caller_arg(x),
                                       call = caller_env()) {
  if (!missing(x)) {
    if (is.data.frame(x) || is.matrix(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x = x,
    what = "a data frame or a matrix",
    arg = arg,
    call = call
  )
}

coerce_to_tibble <- function(x) {
  # Only to be used after calling `check_data_frame_or_matrix()`.
  # Coerces matrices and bare data frames to tibbles.
  # Avoids calling `as_tibble()` on tibbles, as that is more expensive than
  # you'd think.
  if (tibble::is_tibble(x)) {
    x
  } else if (is.data.frame(x)) {
    hardhat_new_tibble(x, size = vec_size(x))
  } else {
    tibble::as_tibble(x, .name_repair = "minimal")
  }
}

# ------------------------------------------------------------------------------

hardhat_new_tibble <- function (x, size) {
  # Faster than `tibble::new_tibble()`, and it drops extra attributes
  new_data_frame(x = x, n = size, class = c("tbl_df", "tbl"))
}

# ------------------------------------------------------------------------------

check_inherits <- function(x,
                           what,
                           ...,
                           allow_null = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (!missing(x)) {
    if (inherits(x, what)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x = x,
    what = cli::format_inline("a <{what}>"),
    arg = arg,
    call = call
  )
}

# ------------------------------------------------------------------------------

vec_cast_named <- function(x, to, ..., call = caller_env()) {
  # vec_cast() drops names currently
  # https://github.com/r-lib/vctrs/issues/623
  out <- vec_cast(x, to, ..., call = call)

  names <- vec_names(x)
  if (!is.null(names)) {
    out <- vec_set_names(out, names)
  }

  out
}
