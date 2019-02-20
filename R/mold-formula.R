#' Mold - Formula Method
#'
#' @description
#'
#' For a formula, `mold()` does the following:
#'
#' - Predictors
#'
#'    - The RHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ RHS`.
#'
#'    - Runs [stats::model.frame()] on the RHS formula and uses `data`.
#'
#'    - If `indicators = TRUE`, it then runs [stats::model.matrix()] on the
#'    result.
#'
#'    - If `indicators = FALSE`, factors are removed before `model.matrix()`
#'    is run, and then added back afterwards. No interactions or inline
#'    functions involving factors are allowed.
#'
#'    - If any offsets are present from using `offset()`, then they are
#'    extracted with [model_offset()].
#'
#'    - If `intercept = TRUE`, adds an intercept column.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' - Outcomes
#'
#'    - The LHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ LHS`.
#'
#'    - Runs [stats::model.frame()] on the LHS formula and uses `data`.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' @inheritParams mold.data.frame
#'
#' @param formula A formula specifying the terms of the model, with the outcomes
#' on the left-hand side of the formula, and the predictors on the right-hand
#' side.
#'
#' @param data A data frame containing the predictors and the outcomes.
#'
#' @param indicators Should factors and interactions be expanded
#' (In other words, should [stats::model.matrix()] be run)? If
#' `FALSE`, factor columns are returned without being expanded into dummy
#' variables and a warning is thrown if any interactions are detected.
#'
#' @section Differences From Base R:
#'
#' There are a number of differences from base R regarding how formulas are
#' processed by `mold()` that require some explanation.
#'
#' Multivariate outcomes can be specified on the LHS using syntax that is
#' similar to the RHS (i.e. `outcome_1 + outcome_2 ~ predictors`).
#' If any complex calculations are done on the LHS and they return matrices
#' (like [stats::poly()]), then those matrices are flattened into multiple
#' columns of the tibble after the call to `model.frame()`. While this is
#' possible, it is not recommended, and if a large amount of preprocessing is
#' required on the outcomes you are better off using a [recipes::recipe()].
#'
#' Global variables are _not_ allowed in the formula. An error will be thrown
#' if they are included. All terms in the formula should come from `data`.
#'
#' By default, intercepts are _not_ included in the predictor output from the
#' formula. To include an intercept, set `intercept = TRUE`. Having an intercept
#' argument is consistent with the other `mold()` methods. More importantly,
#' there are often modeling packages where an intercept is either always or
#' never allowed (for example, the `earth` package), and they do some fancy
#' footwork to keep the user from providing or removing an intercept.
#' This interface standardizes all of that flexibility in one place.
#'
#' @details
#'
#' While not different from base R, the behavior of expanding factors into
#' dummy variables when an intercept is _not_ present should be documented.
#'
#' - When an intercept is present, factors are expanded into `K-1` new columns,
#' where `K` is the number of levels in the factor.
#'
#' - When an intercept is _not_ present, factors are expanded into all `K`
#' columns (one-hot encoding).
#'
#' Offsets can be included in the formula method through the use of the inline
#' function [stats::offset()]. These are returned as a tibble with 1 column
#' named `".offset"` in the `$outcome` slot of the return value.
#'
#' @inherit mold return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Formula example
#'
#' processed <- mold(Sepal.Width ~ Species, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' processed$outcomes
#'
#' # ---------------------------------------------------------------------------
#' # Factors without an intercept
#'
#' # No intercept is added
#' processed <- mold(Sepal.Width ~ Species, iris)
#'
#' # So factor columns are completely expanded
#' # into all `K` columns (the number of levels)
#' processed$predictors
#'
#' # ---------------------------------------------------------------------------
#' # Global variables
#'
#' y <- rep(1, times = nrow(iris))
#'
#' # In base R, global variables are allowed in a model formula
#' frame <- model.frame(Species ~ y + Sepal.Length, iris)
#' head(frame)
#'
#' # mold() does not allow them, and throws an error
#' tryCatch(
#'   expr = mold(Species ~ y + Sepal.Length, iris),
#'   error = function(e) print(e$message)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables and interactions
#'
#' # By default, factor columns are expanded
#' # and interactions are created, both by
#' # calling model.matrix(). Some models (like
#' # tree based models) can take factors directly
#' # but still might want to use the formula method.
#' # In those cases, set `indicators = FALSE` to not
#' # run model.matrix() on factor columns. Interactions
#' # are still allowed and run on numeric columns.
#'
#' processed <- mold(
#'   ~ Species + Sepal.Width:Sepal.Length,
#'   iris,
#'   indicators = FALSE
#' )
#'
#' processed$predictors
#'
#' # An informative error is thrown when `indicators = FALSE` and
#' # factors are present in interaction terms or in inline functions
#' \dontrun{
#' mold(Sepal.Width ~ Sepal.Length:Species, iris, indicators = FALSE)
#' mold(Sepal.Width ~ paste0(Species), iris, indicators = FALSE)
#' }
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas can be specified easily
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, iris)
#' processed$outcomes
#'
#' # Inline functions on the LHS are run, but any matrix
#' # output is flattened (like what happens in `model.matrix()`)
#' # (essentially this means you don't wind up with columns
#' # in the tibble that are matrices)
#' processed <- mold(poly(Sepal.Length, degree = 2) ~ Species, iris)
#' processed$outcomes
#'
#' # TRUE
#' ncol(processed$outcomes) == 2
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets are handled specially in base R, so they deserve special
#' # treatment here as well. You can add offsets using the inline function
#' # offset()
#' processed <- mold(Sepal.Width ~ offset(Sepal.Length) + Species, iris)
#'
#' processed$offset
#'
#' # Multiple offsets can be included, and they get added together
#' processed <- mold(
#'   Sepal.Width ~ offset(Sepal.Length) + offset(Petal.Width),
#'   iris
#' )
#'
#' identical(
#'   processed$offset$.offset,
#'   iris$Sepal.Length + iris$Petal.Width
#' )
#'
#' @rdname mold-formula
#'
#' @export
mold.formula <- function(formula, data, intercept = FALSE,
                         indicators = TRUE, ...) {

  validate_formula_has_intercept(formula)

  data <- check_is_data_like(data)

  formula <- remove_formula_intercept(formula, intercept)
  formula <- alter_formula_environment(formula)

  predictors <- mold_formula_predictors(formula, data, indicators)
  outcomes <- mold_formula_outcomes(formula, data)

  preprocessor <- new_terms_preprocessor(
    engine = new_terms_preprocessor_engine(predictors$terms, outcomes$terms),
    intercept = intercept,
    info = info_lst(
      predictors = predictors$info,
      outcomes = outcomes$info
    ),
    indicators = indicators
  )

  mold_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    preprocessor = preprocessor,
    offset = predictors$offset
  )

}

# ------------------------------------------------------------------------------

mold_formula_predictors <- function(formula, data, indicators) {

  formula <- get_predictors_formula(formula)

  original_names <- get_all_predictors(formula, data)
  original_data <- data[, original_names, drop = FALSE]
  original_data_classes <- get_data_classes(original_data)
  original_levels <- get_levels(original_data)

  if (!indicators) {
    factor_names <- extract_original_factor_names(original_data_classes)
    validate_no_factors_in_functions(formula, factor_names)
    validate_no_factors_in_interactions(formula, factor_names)
    formula <- remove_factors_from_formula(formula, factor_names)
  }

  framed <- model_frame(formula, data)
  offset <- extract_offset(framed$data, framed$terms)

  predictors <- model_matrix(
    terms = framed$terms,
    data = framed$data
  )

  if (!indicators) {
    predictors <- reattach_factor_columns(predictors, data, factor_names)
  }

  terms <- simplify_terms(framed$terms)

  list(
    data = predictors,
    terms = terms,
    offset = offset,
    info = predictors_info(
      names = original_names,
      classes = original_data_classes,
      levels = original_levels
    )
  )

}

mold_formula_outcomes <- function(formula, data) {

  original_names <- get_all_outcomes(formula, data)
  original_data <- data[, original_names, drop = FALSE]
  original_data_classes <- get_data_classes(original_data)
  original_levels <- get_levels(original_data)

  formula <- get_outcomes_formula(formula)

  # used on the `~ LHS` formula
  validate_no_interactions(formula)

  framed <- model_frame(formula, data)

  outcomes <- flatten_embedded_columns(framed$data)

  terms <- simplify_terms(framed$terms)

  list(
    data = outcomes,
    terms = terms,
    info = outcomes_info(
      names = original_names,
      classes = original_data_classes,
      levels = original_levels
    )
  )

}

# ------------------------------------------------------------------------------

alter_formula_environment <- function(formula) {

  # formula environment is 1 step above global env to avoid
  # global variables but maintain ability to use pkg functions
  # (like stats::poly())
  env_above_global_env <- rlang::env_parent(rlang::global_env())

  rlang::new_formula(
    lhs = rlang::f_lhs(formula),
    rhs = rlang::f_rhs(formula),
    env = env_above_global_env
  )
}

# We do this extra flattening because it happens on the RHS
# automatically because of the model.matrix() call. So this
# makes the column types consistent when doing something
# complex on the LHS like poly(, degree = 2) that returns
# a matrix
flatten_embedded_columns <- function(data) {

  has_embedded_2D <- vapply(
    X = data,
    FUN = function(col) dims(col) > 1,
    FUN.VALUE = logical(1)
  )

  has_any_embedded_2D <- any(has_embedded_2D)

  if (has_any_embedded_2D) {

    # Inspired by
    # https://stackoverflow.com/questions/43281803/embedded-data-frame-in-r-what-is-it-what-is-it-called-why-does-it-behave-th
    # This could probably be better?
    # It doesn't work with tibble(!!!x)
    frame_flattener <- rlang::expr(
      data.frame(
        !!!data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )

    data <- rlang::eval_bare(frame_flattener)
  }

  tibble::as_tibble(data)
}

strip_model_matrix <- function(x) {
  attr(x, "assign") <- NULL
  attr(x, "dimnames") <- list(NULL, dimnames(x)[[2]])
  x
}

validate_no_factors_in_functions <- function(.formula, .factor_names) {

  .terms <- terms(.formula)

  bad_original_cols <- detect_factors_in_functions(.terms, .factor_names)

  ok <- length(bad_original_cols) == 0L

  if (!ok) {

    bad_original_cols <- glue::glue_collapse(
      glue::single_quote(bad_original_cols),
      ", "
    )

    glubort(
      "Functions involving factors have been detected on the ",
      "RHS of `formula`. These are not allowed when `indicators = FALSE`. ",
      "Functions involving factors were detected for the following columns: ",
      "{bad_original_cols}."
    )

  }

  invisible(.formula)
}

# Returns original column names of any factor columns that
# are present in an inline function
# The row.names() of the factors matrix contains all of the
# non-interaction expressions that are used in the formula
detect_factors_in_functions <- function(.terms, .factor_names) {

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  all_terms_exprs <- row.names(terms_matrix)

  # Remove bare factor names
  exprs_no_bare_factors <- all_terms_exprs[!(all_terms_exprs %in% .factor_names)]

  if (length(exprs_no_bare_factors) == 0L) {
    return(character(0))
  }

  .factor_name_is_in_a_fn <- vapply(
    .factor_names,
    function(nm) {
      any(grepl(nm, exprs_no_bare_factors))
    },
    logical(1)
  )

  bad_cols <- .factor_names[.factor_name_is_in_a_fn]

  bad_cols
}

validate_no_factors_in_interactions <- function(.formula, .factor_names) {

  # Call terms on a standard formula to generate the terms interaction matrix
  .terms <- terms(.formula)

  bad_original_cols <- detect_factors_in_interactions(.terms, .factor_names)

  ok <- length(bad_original_cols) == 0L

  if (!ok) {

    bad_original_cols <- glue::glue_collapse(
      glue::single_quote(bad_original_cols),
      ", "
    )

    glubort(
      "Interaction terms involving factors have been detected on the ",
      "RHS of `formula`. These are not allowed when `indicators = FALSE`. ",
      "Interactions involving factors were detected for the following columns: ",
      "{bad_original_cols}."
    )

  }

  invisible(.formula)
}

# Returns the _original_ column names
# of any factor columns that are present
# in any interaction terms (from : or * or %in% or ^)
detect_factors_in_interactions <- function(.terms, .factor_names) {

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  other_cols <- setdiff(colnames(terms_matrix), .factor_names)

  no_other_cols <- length(other_cols) == 0L
  if (no_other_cols) {
    return(character(0))
  }

  # Something like Species, rather than paste0(Species)
  bare_factor_names <- .factor_names[.factor_names %in% row.names(terms_matrix)]

  # Something like mold(~ paste0(Species), iris, indicators = FALSE)
  no_bare_factors_used <- length(bare_factor_names) == 0L
  if (no_bare_factors_used) {
    return(character(0))
  }

  factor_rows <- terms_matrix[bare_factor_names, , drop = FALSE]
  factor_rows <- factor_rows[, other_cols, drop = FALSE]

  # In the factor matrix, only `:` is present to represent interactions,
  # even if something like * or ^ or %in% was used to generate it
  where_interactions <- grepl(":", colnames(factor_rows))

  none_have_interactions <- !any(where_interactions)
  if (none_have_interactions) {
    return(character(0))
  }

  interaction_cols <- factor_rows[, where_interactions, drop = FALSE]

  factor_is_bad_if_gt_0 <- rowSums(interaction_cols)
  bad_factor_vals <- factor_is_bad_if_gt_0[factor_is_bad_if_gt_0 > 0]

  bad_cols <- names(bad_factor_vals)

  bad_cols
}

validate_no_interactions <- function(.formula) {

  bad_terms <- detect_interactions(.formula)

  no_interactions <- length(bad_terms) == 0L
  if (no_interactions) {
    return(invisible(.formula))
  }

  bad_terms <- glue::glue_collapse(glue::single_quote(bad_terms), ", ")

  glubort(
    "Interaction terms cannot be specified on the LHS of `formula`. ",
    "The following interaction terms were found: {bad_terms}."
  )
}

# Returns processed names of any interaction terms
# like 'Species:Sepal.Width', or character(0)
detect_interactions <- function(.formula) {

  .terms <- terms(.formula)

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  terms_nms <- colnames(terms_matrix)

  # All interactions (*, ^, %in%) will be expanded to `:`
  has_interactions <- grepl(":", terms_nms)

  has_any_interactions <- any(has_interactions)

  if (!has_any_interactions) {
    return(character(0))
  }

  bad_terms <- terms_nms[has_interactions]

  bad_terms
}

extract_original_factor_names <- function(.data_classes) {

  no_data_classes <- length(.data_classes) == 0L
  if (no_data_classes) {
    return(character(0))
  }

  where_factor <- vapply(.data_classes, function(cls) any(cls %in% c("factor", "ordered")), logical(1))
  factor_classes <- .data_classes[where_factor]

  no_factor_classes <- length(factor_classes) == 0L
  if (no_factor_classes) {
    return(character(0))
  }

  original_factor_columns <- names(factor_classes)

  original_factor_columns
}

remove_factors_from_formula <- function(.formula, .factor_names) {

  if (length(.factor_names) == 0L) {
    return(.formula)
  }

  .factor_syms <- rlang::syms(.factor_names)

  .f_rhs <- rlang::f_rhs(.formula)

  for (.factor_sym in .factor_syms) {
    .f_rhs <- rlang::expr(!! .f_rhs - !! .factor_sym)
  }

  rlang::new_formula(
    lhs = rlang::f_lhs(.formula),
    rhs = .f_rhs,
    env = rlang::f_env(.formula)
  )
}

reattach_factor_columns <- function(predictors, data, .factor_names) {
  data_factor_cols <- data[, .factor_names, drop = FALSE]
  tibble::add_column(predictors, !!! data_factor_cols)
}

get_predictors_formula <- function(formula) {
  rlang::new_formula(
    lhs = NULL,
    rhs = rlang::f_rhs(formula),
    env = rlang::f_env(formula)
  )
}

get_outcomes_formula <- function(formula) {

  new_formula <- rlang::new_formula(
    lhs = NULL,
    rhs = rlang::f_lhs(formula),
    env = rlang::f_env(formula)
  )

  remove_formula_intercept(new_formula, intercept = FALSE)
}
