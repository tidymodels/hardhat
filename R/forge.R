#' Forge prediction-ready data
#'
#' `forge()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' If the outcome is present in `new_data`, it can optionally be processed
#' and returned in the `outcome` slot of the returned list. This is only
#' applicable for the formula and recipes engines, but is very useful when
#' doing cross validation where you need to preprocess the outcome of a test
#' set before computing performance.
#'
#' @param preprocessor A valid `"preprocessor"`. The preprocessor that should
#' be used here is the one in the output from the corresponding call
#' to [mold()].
#'
#' @param new_data A data frame or matrix to preprocess.
#'
#' @param outcome A logical. Should the outcome be processed and returned
#' as well?
#'
#' @param ... Not currently used.
#'
#' @return
#'
#' A named list with elements:
#'
#'  - `predictors`: A tibble containing the preprocessed
#'  `new_data` predictors.
#'
#'  - `outcome`: If `outcome = TRUE`, the outcome is returned here, otherwise
#'  `NULL`. If a formula engine was used, this is a data frame that is the
#'  result of extracting the outcome columns from [stats::model.frame()].
#'  If a recipe was used, this is a data.frame that is the result of calling
#'  [recipes::bake()] with [recipes::all_outcomes()] specified.
#'
#' @export
forge <- function(preprocessor, new_data, ...) {
  UseMethod("forge")
}

#' @export
forge.default <- function(preprocessor, new_data, ...) {
  abort("Unknown preprocessor.")
}

#' @rdname forge
#' @export
forge.default_preprocessor <- function(preprocessor, new_data, ...) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_no_outcome_specified(list(...))

  new_data <- shrink(preprocessor, new_data)

  predictors <- preprocessor$engine$process(
    new_data,
    preprocessor$intercept
  )

  forge_list(predictors)
}

#' @rdname forge
#' @export
forge.recipes_preprocessor <- function(preprocessor, new_data,
                                       outcome = FALSE, ...) {

  validate_recipes_available()
  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcome)

  new_data <- shrink(preprocessor, new_data, outcome)

  baked_list <- bake_recipe_engine(
    preprocessor = preprocessor,
    new_data = new_data,
    outcome = outcome
  )

  baked_list$predictors <- maybe_add_intercept_column(
    x = baked_list$predictors,
    intercept = preprocessor$intercept
  )

  baked_list
}

#' @rdname forge
#' @export
forge.terms_preprocessor <- function(preprocessor, new_data,
                                          outcome = FALSE, ...) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcome)

  new_data <- shrink(preprocessor, new_data, outcome)

  baked_list <- bake_terms_engine(preprocessor, new_data, outcome)

  baked_list
}

# ------------------------------------------------------------------------------

forge_list <- function(predictors, outcomes = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes
  )
}

# ------------------------------------------------------------------------------

bake_recipe_engine <- function(preprocessor, new_data, outcome) {
  if (outcome) {
    bake_with_outcome(preprocessor, new_data)
  }
  else {
    bake_without_outcome(preprocessor, new_data)
  }
}

bake_with_outcome <- function(preprocessor, new_data) {

  engine <- preprocessor$engine

  roles <- engine$term_info$role
  processed_predictor_nms <- engine$term_info$variable[roles == "predictor"]
  processed_outcome_nms <- engine$term_info$variable[roles == "outcome"]

  # optimize and don't double bake
  preprocessed_new_data <- recipes::bake(
    object = engine,
    new_data = new_data
  )

  predictors <- preprocessed_new_data[, processed_predictor_nms, drop = FALSE]
  outcomes <- preprocessed_new_data[, processed_outcome_nms, drop = FALSE]

  forge_list(
    predictors = predictors,
    outcomes = outcomes
  )
}

bake_without_outcome <- function(preprocessor, new_data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::bake(
    object = preprocessor$engine,
    new_data = new_data,
    all_predictors()
  )

  forge_list(
    predictors = predictors
  )
}

# ------------------------------------------------------------------------------

bake_terms_engine <- function(preprocessor, new_data, outcome) {

  # From NULL to one env above global
  preprocessor$engine$predictors <- alter_terms_environment(preprocessor$engine$predictors)
  preprocessor$engine$outcomes <- alter_terms_environment(preprocessor$engine$outcomes)

  if (outcome) {
    bake_terms_with_outcome(preprocessor, new_data)
  }
  else {
    bake_terms_without_outcome(preprocessor, new_data)
  }

}

bake_terms_with_outcome <- function(preprocessor, new_data) {

  engine <- preprocessor$engine

  predictors_frame <- model_frame(engine$predictors, new_data, preprocessor$predictor_levels)
  outcomes_frame <- model_frame(engine$outcomes, new_data, preprocessor$outcome_levels)

  predictors <- extract_predictors(terms(predictors_frame), predictors_frame, preprocessor$indicators)
  outcomes <- extract_outcomes(outcomes_frame)

  forge_list(predictors, outcomes)
}

bake_terms_without_outcome <- function(preprocessor, new_data) {

  predictors_terms <- preprocessor$engine$predictors
  predictors_terms <- delete_response(predictors_terms)

  predictors_frame <- model_frame(predictors_terms, new_data, preprocessor$predictor_levels)

  predictors <- extract_predictors(terms(predictors_frame), predictors_frame, preprocessor$indicators)

  forge_list(predictors)
}

# To get the post processed name of the outcome column
response_name <- function(terms_engine) {
  rlang::as_label(rlang::f_lhs(terms_engine))
}

# Is this a bad idea? We need it to forge() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in mold()
alter_terms_environment <- function(terms_engine) {
  env_above_global_env <- rlang::env_parent(rlang::global_env())
  attr(terms_engine, ".Environment") <- env_above_global_env
  terms_engine
}

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

validate_no_outcome_specified <- function(dots) {

  if (length(dots) > 0) {
    if ("outcome" %in% names(dots)) {
      glubort(
        "`outcome` cannot be specified when a default preprocessor is used."
      )
    }
  }

  invisible(dots)
}

extract_outcomes_from_frame <- function(terms, frame) {

  processed_outcome_nms <- response_name(terms)
  outcomes <- frame[, processed_outcome_nms, drop = FALSE]

  # Simplify multivariate matrix columns
  if (is.matrix(outcomes[[1]])) {
    outcomes <- outcomes[[1]]
  }

  tibble::as_tibble(outcomes)
}

extract_predictors_from_frame <- function(terms, frame) {

  processed_outcome_nm <- response_name(terms)

  frame[[processed_outcome_nm]] <- NULL

  frame
}
