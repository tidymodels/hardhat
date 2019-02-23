#' Create a new default XY engine
#'
#' This is the constructor for a default XY preprocessing engine. This
#' is the engine used by default from `mold()` if `x` and `y` are provided
#' separately (i.e. the XY interface is used). To learn about what the
#' default mold and forge functionality are, see the Mold and Forge
#' sections below.
#'
#' @details
#'
#' As documented in [standardize()], if `y` is a _vector_, then the returned
#' outcomes tibble has 1 column with a standardized name of `".outcome"`.
#'
#' The one special thing about the XY method's forge function is the behavior of
#' `outcomes = TRUE` when a _vector_ `y` value was provided to the original
#' call to [mold()]. In that case, `mold()` converted `y` into a tibble, with
#' a default name of `.outcome`. This is the column that `forge()` will look
#' for in `new_data` to preprocess. See the examples section for a
#' demonstration of this.
#'
#' @section Mold:
#'
#' The XY engine's mold function does the following:
#'
#' - Converts `x` to a tibble.
#'
#' - Adds an intercept column to `x` if `intercept = TRUE`.
#'
#' - Runs [standardize()] on `y`.
#'
#' @section Forge:
#'
#' The XY engine's forge function does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Potentially adds an intercept column onto `new_data`, if
#' `intercept = TRUE`.
#'
#' @return
#'
#' A preprocessing engine with the class, `"default_xy_engine"` that can
#' be used with [mold()] and [forge()].
#'
#' @inheritParams new_xy_engine
#'
#' @examples
#'
#' # In all of the examples below, X and Y are supplied to mold(),
#' # which means that the default_xy_engine() is being used.
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
#' # Then, call forge() with the engine and the test data
#' # to have it preprocess the test data in the same way
#' forge(test_x, processed$engine)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept
#'
#' processed <- mold(train_x, train_y, intercept = TRUE)
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

#' @rdname default_xy_engine
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

  list(
    engine = engine,
    x = x,
    y = y
  )
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

  c(engine, predictors) %<-% mold_xy_default_process_predictors(engine, x)
  c(engine, outcomes) %<-% mold_xy_default_process_outcomes(engine, y)

  list(
    engine = engine,
    predictors = predictors,
    outcomes = outcomes
  )
}

mold_xy_default_process_predictors <- function(engine, x) {

  # Important! Collect info before adding intercept!
  info <- predictors_info(
    names = colnames(x),
    classes = get_data_classes(x),
    levels = get_levels(x)
  )

  x <- maybe_add_intercept_column(x, engine$intercept)

  list(
    engine = engine,
    predictors = list(
      data = x,
      info = info,
      offset = NULL
    )
  )

}

mold_xy_default_process_outcomes <- function(engine, y) {

  info <- outcomes_info(
    names = colnames(y),
    classes = get_data_classes(y),
    levels = get_levels(y)
  )

  list(
    engine = engine,
    outcomes = list(
      data = y,
      info = info
    )
  )

}

# ------------------------------------------------------------------------------

get_forge_xy_default_function_set <- function() {
  engine_function_set(forge_xy_default_clean, forge_xy_default_process)
}

forge_xy_default_clean <- function(engine, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink(new_data, engine, outcomes)
  new_data <- scream(new_data, engine, outcomes)

  list(
    engine = engine,
    new_data = new_data
  )

}

forge_xy_default_process <- function(engine, new_data, outcomes) {

  c(engine, .predictors) %<-% forge_xy_default_process_predictors(engine, new_data)
  c(engine, .outcomes) %<-% forge_xy_default_process_outcomes(engine, new_data, outcomes)

  list(
    engine = engine,
    predictors = .predictors,
    outcomes = .outcomes
  )
}

forge_xy_default_process_predictors <- function(engine, new_data) {

  original_names <- engine$info$predictors$names

  .predictors <- new_data[, original_names, drop = FALSE]

  .predictors <- maybe_add_intercept_column(.predictors, engine$intercept)

  list(
    engine = engine,
    predictors = list(
      data = .predictors,
      offset = NULL
    )
  )

}

forge_xy_default_process_outcomes <- function(engine, new_data, outcomes) {

  if (!outcomes) {

    out <- list(
      engine = engine,
      outcomes = list(
        data = NULL
      )
    )

    return(out)
  }

  original_names <- engine$info$outcomes$names

  .outcomes <- new_data[, original_names, drop = FALSE]

  list(
    engine = engine,
    outcomes = list(
      data = .outcomes
    )
  )

}
