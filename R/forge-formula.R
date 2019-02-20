#' Forge - Formula Method
#'
#' @description
#'
#' For the terms preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Predictors
#'
#'    - Runs [stats::model.frame()] on `new_data` using the stored terms
#'    object corresponding to the _predictors_.
#'
#'    - If, in the original [mold()] call, `indicators = TRUE` was set, it
#'    then runs [stats::model.matrix()] on the result.
#'
#'    - If any offsets are present from using `offset()` in the original call
#'    to [mold()], then they are extracted with [model_offset()].
#'
#'    - If `intercept = TRUE` in the original call to [mold()], then an
#'    intecept column is added.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#'  - Outcomes
#'
#'    - Runs [stats::model.frame()] on `new_data` using the stored terms object
#'    corresponding to the _outcomes_.
#'
#'    - Coerces the result to a tibble.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"terms_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Formula Example
#'
#' # Call mold() with the training data
#' processed <- mold(
#'   log(Sepal.Length) ~ Sepal.Length + Species,
#'   train,
#'   intercept = TRUE
#' )
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables
#'
#' # If factors are not expanded in mold()...
#' processed <- mold(Sepal.Width ~ Species, train, indicators = FALSE)
#'
#' # ...then they aren't expanded in forge() either
#' forge(processed$preprocessor, test)
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas specified in mold()
#' # carry over into forge()
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, train)
#'
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets specified in mold() are computed in forge() as well,
#' # and are placed in the `$offset` slot of the result
#' processed <- mold(
#'   Sepal.Width ~ Species + offset(Sepal.Length) + offset(Petal.Width),
#'   train
#' )
#'
#' forge(processed$preprocessor, test)
#'
#' @rdname forge-formula
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

  predictors_framed <- model_frame(engine$predictors, new_data, preprocessor$info$predictors$levels)
  outcomes_framed <- model_frame(engine$outcomes, new_data, preprocessor$info$outcomes$levels)

  predictors <- model_matrix(
    terms = predictors_framed$terms,
    data = predictors_framed$data
  )

  if (!preprocessor$indicators) {
    factor_names <- extract_original_factor_names(preprocessor$info$predictors$classes)
    predictors <- reattach_factor_columns(predictors, new_data, factor_names)
  }

  outcomes <- flatten_embedded_columns(outcomes_framed$data)

  offset <- extract_offset(predictors_framed$data, predictors_framed$terms)

  forge_list(predictors, outcomes, offset)
}

bake_terms_without_outcome <- function(preprocessor, new_data) {

  predictors_terms <- preprocessor$engine$predictors
  predictors_terms <- delete_response(predictors_terms)

  predictors_framed <- model_frame(predictors_terms, new_data, preprocessor$info$predictors$levels)

  predictors <- model_matrix(
    terms = predictors_framed$terms,
    data = predictors_framed$data
  )

  if (!preprocessor$indicators) {
    factor_names <- extract_original_factor_names(preprocessor$info$predictors$classes)
    predictors <- reattach_factor_columns(predictors, new_data, factor_names)
  }

  offset <- extract_offset(predictors_framed$data, predictors_framed$terms)

  forge_list(predictors, offset = offset)
}

# ------------------------------------------------------------------------------

# Is this a bad idea? We need it to forge() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in mold()
alter_terms_environment <- function(terms_engine) {
  env_above_global_env <- rlang::env_parent(rlang::global_env())
  attr(terms_engine, ".Environment") <- env_above_global_env
  terms_engine
}
