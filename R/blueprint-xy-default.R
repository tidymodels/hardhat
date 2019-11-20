#' Default XY blueprint
#'
#' This pages holds the details for the XY preprocessing blueprint. This
#' is the blueprint used by default from `mold()` if `x` and `y` are provided
#' separately (i.e. the XY interface is used).
#'
#' @inheritParams new-blueprint
#'
#' @param x A data frame or matrix containing the predictors.
#'
#' @param y A data frame, matrix, or vector containing the outcomes.
#'
#' @param blueprint A preprocessing `blueprint`. If left as `NULL`, then a
#' [default_xy_blueprint()] is used.
#'
#' @param ... Not used.
#'
#' @details
#'
#' As documented in [standardize()], if `y` is a _vector_, then the returned
#' outcomes tibble has 1 column with a standardized name of `".outcome"`.
#'
#' The one special thing about the XY method's forge function is the behavior of
#' `outcomes = TRUE` when a _vector_ `y` value was provided to the original
#' call to [mold()]. In that case, `mold()` converts `y` into a tibble, with
#' a default name of `.outcome`. This is the column that `forge()` will look
#' for in `new_data` to preprocess. See the examples section for a
#' demonstration of this.
#'
#' @section Mold:
#'
#' When `mold()` is used with the default xy blueprint:
#'
#' - It converts `x` to a tibble.
#'
#' - It adds an intercept column to `x` if `intercept = TRUE`.
#'
#' - It runs [standardize()] on `y`.
#'
#' @section Forge:
#'
#' When `forge()` is used with the default xy blueprint:
#'
#' - It calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - It calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - It adds an intercept column onto `new_data` if `intercept = TRUE`.
#'
#' @examples
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
#' # Then, call forge() with the blueprint and the test data
#' # to have it preprocess the test data in the same way
#' forge(test_x, processed$blueprint)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept
#'
#' processed <- mold(train_x, train_y, blueprint = default_xy_blueprint(intercept = TRUE))
#'
#' forge(test_x, processed$blueprint)
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
#' \dontrun{
#' forge(test_x, processed$blueprint, outcomes = TRUE)
#' }
#'
#' # Need to use the full test set, including `y`
#' forge(test, processed$blueprint, outcomes = TRUE)
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
#' forge(iris, processed_vec$blueprint, outcomes = TRUE)
#' }
#'
#' test2 <- test
#' test2$.outcome <- test2$Species
#' test2$Species <- NULL
#'
#' # This works, and returns a tibble in the $outcomes slot
#' forge(test2, processed_vec$blueprint, outcomes = TRUE)
#'
#' @export
default_xy_blueprint <- function(intercept = FALSE, allow_novel_levels = FALSE) {

  mold <- get_mold_xy_default_function_set()
  forge <- get_forge_xy_default_function_set()

  new_default_xy_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels
  )

}

#' Create a new default blueprint
#'
#' This page contains the constructors for the default blueprints. They can be
#' extended if you want to add extra behavior on top of what the default
#' blueprints already do, but generally you will extend the non-default versions
#' of the constructors found in the documentation for [new_blueprint()].
#'
#' @inheritParams new_xy_blueprint
#' @inheritParams new_formula_blueprint
#' @inheritParams new_recipe_blueprint
#'
#' @name new-default-blueprint
#' @export
new_default_xy_blueprint <- function(mold,
                                     forge,
                                     intercept = FALSE,
                                     allow_novel_levels = FALSE,
                                     ptypes = NULL,
                                     ...,
                                     subclass = character()) {

  new_xy_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    ptypes = ptypes,
    ...,
    subclass = c(subclass, "default_xy_blueprint")
  )

}

#' @export
refresh_blueprint.default_xy_blueprint <- function(blueprint) {
  do.call(new_default_xy_blueprint, as.list(blueprint))
}

# ------------------------------------------------------------------------------

get_mold_xy_default_function_set <- function() {
  blueprint_function_set(mold_xy_default_clean, mold_xy_default_process)
}

# mold - xy - clean
mold_xy_default_clean <- function(blueprint, x, y) {
  cleaned <- mold_xy_default_clean_predictors(blueprint, x)

  blueprint <- cleaned$blueprint
  x <- cleaned$x

  # Special case `y = NULL` as a 0 column variation on `x`
  if (is.null(y)) {
    y <- x[, 0L, drop = FALSE]
  }

  cleaned <- mold_xy_default_clean_outcomes(blueprint, y)

  blueprint <- cleaned$blueprint
  y <- cleaned$y

  out$mold$clean_xy(blueprint, x, y)
}

mold_xy_default_clean_predictors <- function(blueprint, x) {
  x <- tibble::as_tibble(x)
  list(blueprint = blueprint, x = x)
}

mold_xy_default_clean_outcomes <- function(blueprint, y) {
  y <- standardize(y)
  list(blueprint = blueprint, y = y)
}

# mold - xy - process
mold_xy_default_process <- function(blueprint, x, y) {
  processed <- mold_xy_default_process_predictors(blueprint, x)

  blueprint <- processed$blueprint
  predictors_lst <- processed$terms_lst

  processed <- mold_xy_default_process_outcomes(blueprint, y)

  blueprint <- processed$blueprint
  outcomes_lst <- processed$terms_lst

  ptypes <- out$ptypes$final(predictors_lst$ptype, outcomes_lst$ptype)
  extras <- out$extras$final(predictors_lst$extras, outcomes_lst$extras)

  out$mold$process(blueprint, predictors_lst$data, outcomes_lst$data, ptypes, extras)
}

mold_xy_default_process_predictors <- function(blueprint, x) {

  # Important! Collect ptype before adding intercept!
  ptype <- extract_ptype(x)

  x <- maybe_add_intercept_column(x, blueprint$intercept)

  predictors_lst <- out$mold$process_terms_lst(data = x, ptype)

  out$mold$process_terms(blueprint, predictors_lst)
}

mold_xy_default_process_outcomes <- function(blueprint, y) {

  ptype <- extract_ptype(y)

  outcomes_lst <- out$mold$process_terms_lst(data = y, ptype)

  out$mold$process_terms(blueprint, outcomes_lst)
}

# ------------------------------------------------------------------------------

get_forge_xy_default_function_set <- function() {
  blueprint_function_set(forge_xy_default_clean, forge_xy_default_process)
}

forge_xy_default_clean <- function(blueprint, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  predictors <- shrink(new_data, blueprint$ptypes$predictors)

  predictors <- scream(
    predictors,
    blueprint$ptypes$predictors,
    allow_novel_levels = blueprint$allow_novel_levels
  )

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes)
    # Never allow novel levels for outcomes
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes)
  }
  else {
    outcomes <- NULL
  }

  out$forge$clean(blueprint, predictors, outcomes)
}

forge_xy_default_process <- function(blueprint, predictors, outcomes, extras) {
  processed <- forge_xy_default_process_predictors(blueprint, predictors)

  blueprint <- processed$blueprint
  predictors_lst <- processed$terms_lst

  processed <- forge_xy_default_process_outcomes(blueprint, outcomes)

  blueprint <- processed$blueprint
  outcomes_lst <- processed$terms_lst

  extras <- c(
    extras,
    out$extras$final(predictors_lst$extras, outcomes_lst$extras)
  )

  out$forge$process(predictors_lst$data, outcomes_lst$data, extras)
}

forge_xy_default_process_predictors <- function(blueprint, predictors) {

  predictors <- maybe_add_intercept_column(predictors, blueprint$intercept)

  predictors_lst <- out$forge$process_terms_lst(data = predictors)

  out$forge$process_terms(blueprint, predictors_lst)
}

forge_xy_default_process_outcomes <- function(blueprint, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    outcomes_lst <- out$forge$process_terms_lst()
    result <- out$forge$process_terms(blueprint, outcomes_lst)
    return(result)
  }

  outcomes_lst <- out$forge$process_terms_lst(data = outcomes)

  out$forge$process_terms(blueprint, outcomes_lst)
}
