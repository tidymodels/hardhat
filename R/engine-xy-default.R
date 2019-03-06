#' Default XY engine
#'
#' This pages holds the details for the XY preprocessing engine. This
#' is the engine used by default from `mold()` if `x` and `y` are provided
#' separately (i.e. the XY interface is used).
#'
#' @inheritParams new-engine
#'
#' @param x A data frame or matrix containing the predictors.
#'
#' @param y A data frame, matrix, or vector containing the outcomes.
#'
#' @param engine A preprocessing `engine`. If left as `NULL`, then a
#' [default_xy_engine()] is used.
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
#' When `mold()` is used with the default xy engine:
#'
#' - It converts `x` to a tibble.
#'
#' - It adds an intercept column to `x` if `intercept = TRUE`.
#'
#' - It runs [standardize()] on `y`.
#'
#' @section Forge:
#'
#' When `forge()` is used with the default xy engine:
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
#' # Then, call forge() with the engine and the test data
#' # to have it preprocess the test data in the same way
#' forge(test_x, processed$engine)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept
#'
#' processed <- mold(train_x, train_y, default_xy_engine(intercept = TRUE))
#'
#' forge(test_x, processed$engine)
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
#' forge(test_x, processed$engine, outcomes = TRUE)
#' }
#'
#' # Need to use the full test set, including `y`
#' forge(test, processed$engine, outcomes = TRUE)
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
#' forge(iris, processed_vec$engine, outcomes = TRUE)
#' }
#'
#' test2 <- test
#' test2$.outcome <- test2$Species
#' test2$Species <- NULL
#'
#' # This works, and returns a tibble in the $outcomes slot
#' forge(test2, processed_vec$engine, outcomes = TRUE)
#'
#' @export
default_xy_engine <- function(intercept = FALSE) {

  mold <- get_mold_xy_default_function_set()
  forge <- get_forge_xy_default_function_set()

  new_default_xy_engine(
    mold = mold,
    forge = forge,
    intercept = intercept
  )

}

#' Create a new default engine
#'
#' This page contains the constructors for the default engines. They can be
#' extended if you want to add extra behavior on top of what the default
#' engines already do, but generally you will extend the non-default versions
#' of the constructors found in the documentation for [new_engine()].
#'
#' @inheritParams new_xy_engine
#' @inheritParams new_formula_engine
#' @inheritParams new_recipe_engine
#'
#' @param terms A named list of two elements, `predictors` and `outcomes`. Both
#' elements are `terms` objects that describe the terms for the outcomes and
#' predictors separately. This argument is set automatically at [mold()] time.
#'
#' @name new-default-engine
#' @export
new_default_xy_engine <- function(mold,
                                  forge,
                                  intercept = FALSE,
                                  info = NULL,
                                  ...,
                                  subclass = character()) {

  new_xy_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    ...,
    subclass = c(subclass, "default_xy_engine")
  )

}

#' @export
refresh_engine.default_xy_engine <- function(engine) {
  do.call(new_default_xy_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

get_mold_xy_default_function_set <- function() {
  engine_function_set(mold_xy_default_clean, mold_xy_default_process)
}

# mold - xy - clean
mold_xy_default_clean <- function(engine, x, y) {

  c(engine, x) %<-% mold_xy_default_clean_predictors(engine, x)
  c(engine, y) %<-% mold_xy_default_clean_outcomes(engine, y)

  out$mold$clean_xy(engine, x, y)
}

mold_xy_default_clean_predictors <- function(engine, x) {
  x <- tibble::as_tibble(x)
  list(engine = engine, x = x)
}

mold_xy_default_clean_outcomes <- function(engine, y) {
  y <- standardize(y)
  list(engine = engine, y = y)
}

# mold - xy - process
mold_xy_default_process <- function(engine, x, y) {

  c(engine, predictors_lst) %<-% mold_xy_default_process_predictors(engine, x)
  c(engine, outcomes_lst) %<-% mold_xy_default_process_outcomes(engine, y)

  info <- out$info$final(predictors_lst$info, outcomes_lst$info)
  extras <- out$extras$final(predictors_lst$extras, outcomes_lst$extras)

  out$mold$process(engine, predictors_lst$data, outcomes_lst$data, info, extras)
}

mold_xy_default_process_predictors <- function(engine, x) {

  # Important! Collect info before adding intercept!
  info <- extract_info(x)

  x <- maybe_add_intercept_column(x, engine$intercept)

  predictors_lst <- out$mold$process_terms_lst(data = x, info)

  out$mold$process_terms(engine, predictors_lst)
}

mold_xy_default_process_outcomes <- function(engine, y) {

  info <- extract_info(y)

  outcomes_lst <- out$mold$process_terms_lst(data = y, info)

  out$mold$process_terms(engine, outcomes_lst)
}

# ------------------------------------------------------------------------------

get_forge_xy_default_function_set <- function() {
  engine_function_set(forge_xy_default_clean, forge_xy_default_process)
}

forge_xy_default_clean <- function(engine, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  c(predictors, outcomes) %<-% shrink2(new_data, engine, outcomes)

  predictors <- scream(predictors, engine$info$predictors)
  outcomes <- scream(outcomes, engine$info$outcomes)

  out$forge$clean(engine, predictors, outcomes)
}

forge_xy_default_process <- function(engine, predictors, outcomes) {

  c(engine, predictors_lst) %<-% forge_xy_default_process_predictors(engine, predictors)
  c(engine, outcomes_lst) %<-% forge_xy_default_process_outcomes(engine, outcomes)

  extras <- c(predictors_lst$extras, outcomes_lst$extras)

  out$forge$process(engine, predictors_lst$data, outcomes_lst$data, extras)
}

forge_xy_default_process_predictors <- function(engine, predictors) {

  predictors <- maybe_add_intercept_column(predictors, engine$intercept)

  predictors_lst <- out$forge$process_terms_lst(data = predictors)

  out$forge$process_terms(engine, predictors_lst)
}

forge_xy_default_process_outcomes <- function(engine, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    outcomes_lst <- out$forge$process_terms_lst()
    result <- out$forge$process_terms(engine, outcomes_lst)
    return(result)
  }

  outcomes_lst <- out$forge$process_terms_lst(data = outcomes)

  out$forge$process_terms(engine, outcomes_lst)
}
