#' Forge prediction-ready data
#'
#' @description
#'
#' `forge()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' The return values of each method are all consistent with one another, but the
#' nuances of exactly what is being done for each method vary enough to warrant
#' separate help files for each. Click through to each one below:
#'
#' * XY Method - [forge.default_preprocessor()]
#'
#' * Formula Method - [forge.terms_preprocessor()]
#'
#' * Recipes Method - [forge.recipes_preprocessor()]
#'
#' @details
#'
#' If the outcomes are present in `new_data`, they can optionally be processed
#' and returned in the `outcomes` slot of the returned list. This is very
#' useful when doing cross validation where you need to preprocess the
#' outcomes of a test set before computing performance.
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
#' A named list with 3 elements:
#'
#'  - `predictors`: A tibble containing the preprocessed
#'  `new_data` predictors.
#'
#'  - `outcomes`: If `outcomes = TRUE`, a tibble containing the preprocessed
#'  `new_data` outcomes. Otherwise, `NULL`.
#'
#'  - `offset`: If the `preprocessor` was a `"terms_preprocessor"`, and offsets
#'  were specified in the formula, this is a tibble containing the preprocessed
#'  offsets. Otherwise, `NULL`.
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

#' Forge - XY Method
#'
#' @description
#'
#' For the default preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Calls on the default preprocessor engine to potentially add an intercept
#' column onto `new_data`, if the corresponding call to [mold()] used one.
#'
#' @details
#'
#' The one special thing about the XY method of `forge()` is the behavior of
#' `outcomes = TRUE` when a _vector_ `y` value was provided to the original
#' call to [mold()] (which generated the preprocessor). In that case, `mold()`
#' converted `y` into a tibble, with a default name of `.outcome`. This is the
#' column that `forge()` will look for in `new_data` to preprocess. See the
#' examples section for a demonstration of this.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"default_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' train_x <- train[, "Sepal.Length", drop = FALSE]
#' train_y <- train[, "Species", drop = FALSE]
#'
#' test_x <- test[, "Sepal.Length", drop = FALSE]
#' test_y <- test[, "Species", drop = FALSE]
#'
#' # ---------------------------------------------------------------------------
#' # XY Example
#'
#' # First, call mold() with the training data
#' processed <- mold(train_x, train_y)
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test_x)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept
#'
#' processed <- mold(train_x, train_y, intercept = TRUE)
#'
#' forge(processed$preprocessor, test_x)
#'
#' # ---------------------------------------------------------------------------
#' # XY Method and forge(outcomes = TRUE)
#'
#' # You can request that the new outcome columns are preprocessed as well, but
#' # they have to be present in `new_data`!
#'
#' processed <- mold(train_x, train_y)
#'
#' # Can't do this!
#' # forge(processed$preprocessor, test_x, outcomes = TRUE)
#'
#' # Need to use the full test set, including `y`
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # With the XY method, if the Y value used in `mold()` is a vector,
#' # then a column name of `.outcome` is automatically generated.
#' # This name is what forge() looks for in `new_data`.
#'
#' # Y is a vector!
#' y_vec <- train_y$Species
#'
#' processed_vec <- mold(train_x, y_vec)
#'
#' # This throws an informative error that tell you
#' # to include an `".outcome"` column in `new_data`.
#' \dontrun{
#' forge(processed_vec$preprocessor, iris, outcomes = TRUE)
#' }
#'
#' test2 <- test
#' test2$.outcome <- test2$Species
#' test2$Species <- NULL
#'
#' # This works, and returns a tibble in the $outcomes slot
#' forge(processed_vec$preprocessor, test2, outcomes = TRUE)
#'
#' @rdname forge-xy
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

#' Forge - Recipes Method
#'
#' @description
#'
#' For the recipes preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Calls [recipes::bake()] on the `new_data` using the prepped recipe
#' used during training.
#'
#' - Potentially adds an intercept column onto `new_data`,
#' if the corresponding call to [mold()] used one.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"recipes_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#'
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Recipes Example
#'
#' # Create a recipe for the preprocessing
#' rec <- recipe(Sepal.Width ~ Sepal.Length + Species, iris) %>%
#'    step_log(Sepal.Length) %>%
#'    step_dummy(Species)
#'
#' # Call mold() with the training data
#' processed <- mold(rec, train)
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome!
#' # This logged the Sepal.Length column of `new_data`
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' @rdname forge-recipe
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
