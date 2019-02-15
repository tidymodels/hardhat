#' Forge prediction-ready data
#'
#' `forge()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' If the outcomes are present in `new_data`, it can optionally be processed
#' and returned in the `outcomes` slot of the returned list. This is only
#' applicable for the formula and recipes engines, but is very useful when
#' doing cross validation where you need to preprocess the outcomes of a test
#' set before computing performance.
#'
#' @param preprocessor A valid `"preprocessor"`. The preprocessor that should
#' be used here is the one in the output from the corresponding call
#' to [mold()].
#'
#' @param new_data A data frame or matrix to preprocess.
#'
#' @param outcomes A logical. Should the outcomes be processed and returned
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
#'  - `outcomes`: If `outcomes = TRUE`, the outcomes are returned here,
#'  otherwise `NULL`. If a formula engine was used, this is a data frame
#'  that is the result of extracting the outcome columns from
#'  [stats::model.frame()]. If a recipe was used, this is a data.frame that
#'  is the result of calling [recipes::bake()] with [recipes::all_outcomes()]
#'  specified.
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # XY Method and forge(outcomes = TRUE)
#'
#' # With the formula and recipes methods, you always know the name
#' # of the outcome columns. Because of this, you can ask for the
#' # outcome columns in `new_data` to be processed and returned.
#' # With the XY method if Y is a vector, a column name of `.outcome`
#' # is automatically generated. This name is what forge() looks for
#' # in `new_data` if `y` is a vector. If `y` is a data frame, it just
#' # looks for that original column name.
#'
#' # X and Y are data frames
#' x <- iris[, "Sepal.Width", drop = FALSE]
#' y <- iris[, "Species", drop = FALSE]
#'
#' processed <- mold(x, y)
#' forge(processed$preprocessor, iris, outcomes = TRUE)
#'
#' # Y is a vector
#' y_vec <- y$Species
#'
#' processed_vec <- mold(x, y_vec)
#'
#' # This throws an informative error that tell you
#' # to include a `".outcome"` column in `new_data`.
#' \dontrun{
#' forge(processed_vec$preprocessor, iris, outcomes = TRUE)
#' }
#'
#' iris2 <- iris
#' iris2$.outcome <- iris2$Species
#' iris2$Species <- NULL
#'
#' # This works, and returns a tibble in the $outcomes slot
#' forge(processed_vec$preprocessor, iris2, outcomes = TRUE)
#'
#' @export
forge <- function(preprocessor, new_data,
                  outcomes = FALSE, ...) {
  UseMethod("forge")
}

#' @export
forge.default <- function(preprocessor, new_data,
                          outcomes = FALSE, ...) {
  abort("Unknown preprocessor.")
}

#' @rdname forge
#' @export
forge.default_preprocessor <- function(preprocessor, new_data,
                                       outcomes = FALSE, ...) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")

  new_data <- shrink(preprocessor, new_data, outcomes)
  new_data <- scream(preprocessor, new_data, outcomes)

  baked_list <- bake_default_engine(preprocessor, new_data, outcomes)

  baked_list
}

#' @rdname forge
#' @export
forge.recipes_preprocessor <- function(preprocessor, new_data,
                                       outcomes = FALSE, ...) {

  validate_recipes_available()
  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink(preprocessor, new_data, outcomes)
  new_data <- scream(preprocessor, new_data, outcomes)

  baked_list <- bake_recipe_engine(
    preprocessor = preprocessor,
    new_data = new_data,
    outcomes = outcomes
  )

  baked_list
}

#' @rdname forge
#' @export
forge.terms_preprocessor <- function(preprocessor, new_data,
                                     outcomes = FALSE, ...) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink(preprocessor, new_data, outcomes)
  new_data <- scream(preprocessor, new_data, outcomes)

  baked_list <- bake_terms_engine(preprocessor, new_data, outcomes)

  baked_list
}

# ------------------------------------------------------------------------------

forge_list <- function(predictors, outcomes = NULL, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    offset = offset
  )
}

# ------------------------------------------------------------------------------

bake_default_engine <- function(preprocessor, new_data, outcomes) {

  if (outcomes) {
    baked_list <- bake_default_with_outcome(preprocessor, new_data)
  }
  else {
    baked_list <- bake_default_without_outcome(preprocessor, new_data)
  }

  baked_list$predictors <- preprocessor$engine$process(
    new_data = baked_list$predictors,
    intercept = preprocessor$intercept
  )

  baked_list
}

bake_default_with_outcome <- function(preprocessor, new_data) {

  original_predictor_columns <- preprocessor$predictors$names
  original_outcome_columns <- preprocessor$outcomes$names

  predictors <- new_data[, original_predictor_columns, drop = FALSE]
  outcomes <- new_data[, original_outcome_columns, drop = FALSE]

  forge_list(predictors, outcomes)
}

bake_default_without_outcome <- function(preprocessor, new_data) {

  original_predictor_columns <- preprocessor$predictors$names

  predictors <- new_data[, original_predictor_columns, drop = FALSE]

  forge_list(predictors)
}

# ------------------------------------------------------------------------------

bake_recipe_engine <- function(preprocessor, new_data, outcomes) {

  if (outcomes) {
    baked_list <- bake_with_outcome(preprocessor, new_data)
  }
  else {
    baked_list <- bake_without_outcome(preprocessor, new_data)
  }

  baked_list$predictors <- maybe_add_intercept_column(
    x = baked_list$predictors,
    intercept = preprocessor$intercept
  )

  baked_list
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

bake_terms_engine <- function(preprocessor, new_data, outcomes) {

  # From NULL to one env above global
  preprocessor$engine$predictors <- alter_terms_environment(preprocessor$engine$predictors)
  preprocessor$engine$outcomes <- alter_terms_environment(preprocessor$engine$outcomes)

  if (outcomes) {
    bake_terms_with_outcome(preprocessor, new_data)
  }
  else {
    bake_terms_without_outcome(preprocessor, new_data)
  }

}

bake_terms_with_outcome <- function(preprocessor, new_data) {

  engine <- preprocessor$engine

  predictors_frame <- model_frame(engine$predictors, new_data, preprocessor$predictors$levels)
  outcomes_frame <- model_frame(engine$outcomes, new_data, preprocessor$outcomes$levels)

  predictors <- extract_predictors(
    formula = terms(predictors_frame),
    frame = predictors_frame,
    indicators = preprocessor$indicators,
    intercept = preprocessor$intercept
  )

  outcomes <- extract_outcomes(outcomes_frame)

  offset <- extract_offset(predictors_frame)

  forge_list(predictors, outcomes, offset)
}

bake_terms_without_outcome <- function(preprocessor, new_data) {

  predictors_terms <- preprocessor$engine$predictors
  predictors_terms <- delete_response(predictors_terms)

  predictors_frame <- model_frame(predictors_terms, new_data, preprocessor$predictors$levels)

  predictors <- extract_predictors(
    formula = terms(predictors_frame),
    frame = predictors_frame,
    indicators = preprocessor$indicators,
    intercept = preprocessor$intercept
  )

  offset <- extract_offset(predictors_frame)

  forge_list(predictors, offset = offset)
}

# To get the post processed name of the outcome columns
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
