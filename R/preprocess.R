#' Internal preprocessing of new data
#'
#' `preprocess()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' The `preprocessor` can be a `recipe::recipes()` object, a `terms` object
#' resulting from the use of a formula method, or a
#' `new_default_preprocessor()` which converts `new_data` to a `type` class
#' and optionally adds an intercept.
#'
#' If the outcome is present in `new_data`, it can optionally be processed
#' and returned in the `outcome` slot of the returned list. This is only
#' applicable for the formula and recipes engines, but is very useful when
#' doing cross validation where you need to preprocess the outcome of a test
#' set before computing performance.
#'
#' @inheritParams retype
#'
#' @param preprocessor A valid preprocessor. Can be a [recipes::recipe()], a
#' terms object, or the result of `default_preprocessor()`.
#'
#' @param new_data A data frame to preprocess.
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
#'  - `predictors`: An object of class `type` containing the preprocessed
#'  `new_data` predictors.
#'
#'  - `outcome`: If `outcome = TRUE`, the outcome is returned here, otherwise
#'  `NULL`. If a formula engine was used, this is the result of
#'  [model.response()] (which could be a vector or a matrix). If a recipe was
#'  used, this is a data.frame that is the result of calling [recipes::bake()]
#'  with [recipes::all_outcomes()] specified.
#'
#' @export
preprocess <- function(preprocessor, new_data, ...) {
  UseMethod("preprocess")
}

#' @export
preprocess.default <- function(preprocessor, new_data, ...) {
  abort("Unknown preprocessor.")
}

#' @rdname preprocess
#' @export
preprocess.default_preprocessor <- function(preprocessor, new_data, ...) {

  validate_is_new_data_like(new_data)
  validate_has_named_columns(new_data, "new_data")

  predictors <- preprocessor$engine$process(
    new_data,
    preprocessor$intercept,
    preprocessor$type
  )

  preprocess_list(predictors)
}

#' @rdname preprocess
#' @export
preprocess.recipes_preprocessor <- function(preprocessor, new_data,
                                            outcome = FALSE, ...) {

  validate_recipes_available()
  validate_is_new_data_like(new_data)
  validate_has_named_columns(new_data, "new_data")
  validate_is_bool(outcome)

  baked_list <- bake_recipe_engine(
    engine = preprocessor$engine,
    new_data = new_data,
    outcome = outcome
  )

  baked_list$predictors <- retype(
    x = baked_list$predictors,
    type = preprocessor$type
  )

  baked_list$predictors <- add_intercept_column(
    x = baked_list$predictors,
    add = preprocessor$intercept
  )

  baked_list
}

#' @rdname preprocess
#' @export
preprocess.terms_preprocessor <- function(preprocessor, new_data,
                                          outcome = FALSE, ...) {

  validate_is_new_data_like(new_data)
  validate_has_named_columns(new_data, "new_data")
  validate_is_bool(outcome)

  baked_list <- bake_terms_engine(preprocessor$engine, new_data, outcome)

  baked_list$predictors <- retype(
    x = baked_list$predictors,
    type = preprocessor$type
  )

  baked_list
}

# ------------------------------------------------------------------------------

preprocess_list <- function(predictors, outcomes = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes
  )
}

# ------------------------------------------------------------------------------

bake_recipe_engine <- function(engine, new_data, outcome) {
  if (outcome) {
    bake_with_outcome(engine, new_data)
  }
  else {
    bake_without_outcome(engine, new_data)
  }
}

bake_with_outcome <- function(engine, new_data) {

  roles <- engine$term_info$role
  predictor_nms <- engine$term_info$variable[roles == "predictor"]
  outcome_nms <- engine$term_info$variable[roles == "outcome"]

  # optimize and don't double bake
  preprocessed_new_data <- recipes::bake(
    object = engine,
    new_data = new_data
  )

  predictors <- preprocessed_new_data[, predictor_nms, drop = FALSE]
  outcomes <- preprocessed_new_data[, outcome_nms, drop = FALSE]

  preprocess_list(
    predictors = predictors,
    outcomes = outcomes
  )
}

bake_without_outcome <- function(engine, new_data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::bake(
    object = engine,
    new_data = new_data,
    all_predictors()
  )

  preprocess_list(
    predictors = predictors
  )
}

# ------------------------------------------------------------------------------

bake_terms_engine <- function(engine, new_data, outcome) {

  engine <- alter_terms_environment(engine)

  # new_data could be a matrix, but model.frame() requires a data.frame
  new_data <- tibble::as_tibble(new_data)

  if (outcome) {
    bake_terms_with_outcome(engine, new_data)
  }
  else {
    bake_terms_without_outcome(engine, new_data)
  }

}

bake_terms_with_outcome <- function(engine, new_data) {

  frame <- preprocess_model_frame(engine, new_data)

  outcome_nm <- response_name(engine)
  outcome <- frame[, outcome_nm, drop = FALSE]

  predictors <- preprocess_model_matrix(engine, frame)

  preprocess_list(predictors, outcome)
}

bake_terms_without_outcome <- function(engine, new_data) {

  # Don't attempt to include Y in the model.frame()
  x_terms <- delete_response(engine)

  frame <- preprocess_model_frame(x_terms, new_data)
  predictors <- preprocess_model_matrix(x_terms, frame)

  preprocess_list(predictors)
}

preprocess_model_frame <- function(terms_engine, new_data) {

  # Ensure factors have no new levels
  # (we warn if they do and remove them)
  # (this is so model.frame(xlev) doesnt error out on new levels)
  new_data <- check_new_data_factor_levels(x_levels(terms_engine), new_data)

  # This will detect any missing columns in new_data
  # that should be there, but the error message isn't fantastic.
  # Preprocessing should _never_ removes rows
  # with incomplete data. Setting the na.action
  # to na.pass will retain the NA values through
  # the preprocessing
  new_data <- rlang::with_options(
    model.frame(terms_engine, data = new_data, xlev = x_levels(terms_engine)),
    na.action = "na.pass"
  )

  validate_new_data_classes(terms_engine, new_data)

  new_data
}

preprocess_model_matrix <- function(terms_engine, frame) {

  predictors <- rlang::with_options(
    model.matrix(terms_engine, data = frame),
    na.action = "na.pass"
  )

  predictors
}

response_name <- function(terms_engine) {
  rlang::as_label(rlang::f_lhs(terms_engine))
}

# Is this a bad idea? We need it to preprocess() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in prepare()
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

