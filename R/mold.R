#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' * For a formula, this applies [stats::model.frame()] and
#' possibly [stats::model.matrix()].
#'
#' * For a recipe, this performs a call to both [recipes::prep()]
#' and [recipes::juice()].
#'
#' * For a data frame or matrix, this simply adds an intercept column
#' if requested.
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
#' include an intercept in the molded predictors.
#'
#' @param data A data frame containing the predictors and the outcomes.
#'
#' @param indicators For use with the formula interface. Should factors and
#' interactions be expanded (In other words, should `model.matrix()` be run)? If
#' `FALSE`, factor columns are returned without being expanded into dummy
#' variables and a warning is thrown if any interactions are detected.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing:
#'
#'  - `predictors`: A tibble containing the molded predictors
#'  to be used in the model.
#'
#'  - `outcome`: A tibble.
#'
#'     - If `y` was supplied, it is returned after a call to
#'     `standardize()` is made.
#'
#'     - If a formula engine was used, this is a data frame
#'     that is the result of extracting the molded outcome columns from
#'     `model.frame()`.
#'
#'     - If a recipe was used, this is a data.frame that is the result of
#'     calling [recipes::juice()] with [recipes::all_outcomes()] specified.
#'
#'  - `preprocessor`: A `"preprocessor"` object for use when making predictions.
#'
#'
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  abort_unknown_mold_class(x)
}

#' @rdname mold
#' @export
mold.data.frame <- function(x, y, intercept = FALSE, ...) {

  engine <- new_default_preprocessor_engine()

  x <- engine$process(x, intercept)
  y <- standardize(y)

  preprocessor <- new_default_preprocessor(
    engine = engine,
    intercept = intercept,
    predictors = colnames(x),
    outcomes = colnames(y),
    predictor_levels = get_levels(x),
    outcome_levels = get_levels(y),
    predictor_classes = get_data_classes(x),
    outcome_classes = get_data_classes(y)
  )

  mold_list(x, y, preprocessor)
}

#' @rdname mold
#' @export
mold.matrix <- function(x, y, intercept = FALSE, ...) {

  engine <- new_default_preprocessor_engine()

  x <- engine$process(x, intercept)
  y <- standardize(y)

  preprocessor <- new_default_preprocessor(
    engine = engine,
    intercept = intercept,
    predictors = colnames(x),
    outcomes = colnames(y),
    predictor_levels = NULL,
    outcome_levels = get_levels(y),
    predictor_classes = get_data_classes(x),
    outcome_classes = get_data_classes(y)
  )

  mold_list(x, y, preprocessor)
}

#' @rdname mold
#' @export
mold.formula <- function(formula, data, intercept = FALSE,
                         indicators = TRUE, ...) {

  validate_formula_has_intercept(formula)

  formula <- remove_formula_intercept(formula, intercept)
  formula <- alter_formula_environment(formula)

  framed <- model_frame(formula, data)

  predictors <- extract_predictors(formula, framed, indicators)

  terms <- extract_terms(framed)

  outcomes <- extract_outcomes_from_frame(terms, framed)

  original_predictor_nms <- get_all_predictors(formula, data)
  original_outcome_nms <- get_all_outcomes(formula, data)

  original_predictors <- data[, original_predictor_nms, drop = FALSE]
  original_outcomes <- data[, original_outcome_nms, drop = FALSE]

  preprocessor <- new_terms_preprocessor(
    engine = terms,
    intercept = intercept,
    predictors = original_predictor_nms,
    outcomes = original_outcome_nms,
    predictor_levels = get_levels(original_predictors),
    outcome_levels = get_levels(original_outcomes),
    predictor_classes = get_data_classes(original_predictors),
    outcome_classes = get_data_classes(original_outcomes),
    indicators = indicators
  )

  mold_list(predictors, outcomes, preprocessor)
}

#' @rdname mold
#' @export
mold.recipe <- function(x, data, intercept = FALSE, ...) {

  validate_recipes_available()

  prepped_recipe <- recipes::prep(x, training = data)

  # recipes bug?
  all_predictors <- recipes::all_predictors
  all_outcomes <- recipes::all_outcomes

  # "composition" output is always tibble
  predictors <- recipes::juice(prepped_recipe, all_predictors())
  outcomes <- recipes::juice(prepped_recipe, all_outcomes())

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  all_levels <- get_original_recipe_levels(data, prepped_recipe)
  all_data_classes <- get_original_recipe_data_classes(data, prepped_recipe)

  preprocessor <- new_recipes_preprocessor(
    engine = prepped_recipe,
    intercept = intercept,
    predictors = colnames(predictors),
    outcomes = colnames(outcomes),
    predictor_levels = all_levels$predictor_levels,
    outcome_levels = all_levels$outcome_levels,
    predictor_classes = all_data_classes$predictor_classes,
    outcome_classes = all_data_classes$outcome_classes
  )

  predictors <- maybe_add_intercept_column(predictors, intercept)

  mold_list(predictors, outcomes, preprocessor)
}

# ------------------------------------------------------------------------------
# Preparation helpers

mold_list <- function(predictors, outcomes, preprocessor) {
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

get_original_recipe_levels <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictor_levels = get_levels(x[, original_predictors, drop = FALSE]),
    outcome_levels = get_levels(x[, original_outcomes, drop = FALSE])
  )

}

get_original_recipe_data_classes <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictor_classes = get_data_classes(x[, original_predictors, drop = FALSE]),
    outcome_classes = get_data_classes(x[, original_outcomes, drop = FALSE])
  )

}

extract_predictors <- function(formula, frame, indicators) {

  if (indicators) {
    predictors <- extract_predictors_with_model_matrix(formula, frame)
  }
  else {
    check_for_interactions(formula)
    predictors <- extract_predictors_from_frame(formula, frame)
  }

  tibble::as_tibble(predictors)
}

extract_predictors_with_model_matrix <- function(formula, frame) {

  predictors <- rlang::with_options(
    model.matrix(formula, frame),
    na.action = "na.pass"
  )

  predictors <- strip_model_matrix(predictors)

  predictors
}

strip_model_matrix <- function(x) {
  attr(x, "assign") <- NULL
  attr(x, "dimnames") <- list(NULL, dimnames(x)[[2]])
  x
}


check_for_interactions <- function(formula) {

  formula_chr <- rlang::as_label(formula)

  has_interactions <- grepl(":", formula_chr)

  if (has_interactions) {
    rlang::warn(glue::glue(
      "Interaction terms have been detected in `formula`. ",
      "These are not expanded when `indicators = FALSE`, but the individual ",
      "terms will be included in the output."
    ))
  }

  invisible(formula)
}
