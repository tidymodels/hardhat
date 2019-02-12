#' Prepare data for modeling
#'
#' @description
#'
#' `prepare()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' * For a formula method, this applies `model.frame()` and `model.matrix()`.
#'
#' * For a recipe, this performs a call to both [recipes::prep()]
#' and [recipes::juice()].
#'
#' * For a data frame or matrix, this uses the `new_default_preprocessor()`s
#' which converts the input to `type` and adds an intercept column if requested.
#'
#' @param x A data frame, matrix, or [recipes::recipe()]. If this is a
#' data.frame or matrix, it should contain the predictors.
#'
#' @param formula A formula specifying the terms in the format of
#' `outcome ~ predictors`.
#'
#' @param y A data frame, matrix, or vector containing the outcome(s).
#'
#' @param intercept A single logical specifying whether or not to
#' include an intercept in the prepared predictors.
#'
#' @param type A single character. One of `"tibble"`, `"data.frame"`, or
#' `"matrix"` specifying the result type of the predictors.
#'
#' @param data A data frame to prepare.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing:
#'
#'  - `predictors`: An object of class `type` containing the prepared predictors
#'  to be used in the model.
#'
#'  - `outcome`: If `y` was supplied, it is returned unmodified here. If a
#'  formula was used, this is the result of [model.response()]. If a recipe
#'  was used, this is a data.frame that is the result of calling
#'  [recipes::juice()] with [recipes::all_outcomes()] specified.
#'
#'  - `preprocessor`: A `"preprocessor"` object for use when making predictions.
#'
#'
#' @export
prepare <- function(x, ...) {
  UseMethod("prepare")
}

#' @rdname prepare
#' @export
prepare.data.frame <- function(x, y, intercept = FALSE,
                               type = "tibble", ...) {

  engine <- new_default_preprocessor_engine()

  predictor_nms <- colnames(x)

  preprocessor <- new_default_preprocessor(engine, intercept, type, predictor_nms)

  x <- engine$process(x, intercept, type)

  prepare_list(x, y, preprocessor)
}

#' @rdname prepare
#' @export
prepare.matrix <- function(x, y, intercept = FALSE,
                           type = "tibble", ...) {

  engine <- new_default_preprocessor_engine()

  predictor_nms <- colnames(x)

  preprocessor <- new_default_preprocessor(engine, intercept, type, predictor_nms)

  x <- engine$process(x, intercept, type)

  prepare_list(x, y, preprocessor)
}

#' @rdname prepare
#' @export
prepare.formula <- function(formula, data, intercept = FALSE,
                            type = "tibble", ...) {

  validate_formula_has_intercept(formula)

  formula <- remove_formula_intercept(formula, intercept)
  formula <- alter_formula_environment(formula)

  framed <- rlang::with_options(
    stats::model.frame(formula, data = data),
    na.action = "na.pass"
  )

  predictors <- rlang::with_options(
    model.matrix(formula, framed),
    na.action = "na.pass"
  )

  predictors <- retype(predictors, type)

  outcomes <- model.response(framed)

  terms <- extract_terms(framed)

  preprocessor <- new_terms_preprocessor(
    engine = terms,
    intercept = intercept,
    type = type,
    predictors = get_all_predictors(formula, data),
    outcomes = get_all_outcomes(formula, data),
    predictor_levels = get_predictor_levels(terms, framed)
  )

  prepare_list(predictors, outcomes, preprocessor)
}

#' @rdname prepare
#' @export
prepare.recipe <- function(x, data, intercept = FALSE,
                           type = "tibble", ...) {

  validate_recipes_available()

  prepped_recipe <- recipes::prep(x, training = data)

  # recipes bug?
  all_predictors <- recipes::all_predictors
  all_outcomes <- recipes::all_outcomes

  predictors <- recipes::juice(prepped_recipe, all_predictors())
  outcomes <- recipes::juice(prepped_recipe, all_outcomes())

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  predictor_nms <- colnames(predictors)
  outcome_nms <- colnames(outcomes)

  preprocessor <- new_recipes_preprocessor(
    engine = prepped_recipe,
    intercept = intercept,
    type = type,
    predictors = predictor_nms,
    outcomes = outcome_nms
  )

  predictors <- retype(predictors, type)
  predictors <- add_intercept_column(predictors, intercept)

  prepare_list(predictors, outcomes, preprocessor)
}

# ------------------------------------------------------------------------------
# Preparation helpers

prepare_list <- function(predictors, outcomes, preprocessor) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    preprocessor = preprocessor
  )
}

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
