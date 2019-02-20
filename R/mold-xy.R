#' Mold - XY Method
#'
#' @description
#'
#' For a data frame / matrix, `mold()` does the following:
#'
#' - Converts `x` to a tibble.
#'
#' - Adds an intercept column to `x` if `intercept = TRUE`.
#'
#' - Runs [standardize()] on `y`.
#'
#' @details
#'
#' As documented in [standardize()], if `y` is a _vector_, then the returned
#' outcomes tibble has 1 column with a standardized name of `".outcome"`.
#'
#' @inheritParams mold
#'
#' @param x A data frame or matrix containing the predictors.
#'
#' @param y A data frame, matrix, or vector containing the outcome(s).
#'
#' @param intercept A single logical specifying whether or not to
#' include an intercept in the molded predictors.
#'
#' @inherit mold return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # XY Example
#'
#' x <- iris[, c("Sepal.Width", "Species"), drop = FALSE]
#' y <- iris[, "Sepal.Length", drop = FALSE]
#'
#' processed <- mold(x, y)
#'
#' # The predictors are returned as a tibble
#' processed$predictors
#'
#' # So are the outcomes
#' processed$outcomes
#'
#' # A default preprocessor is also returned
#' # This contains all of the information required
#' # to preprocess new data at prediction time.
#' processed$preprocessor
#'
#' # The preprocessor should be stored in the model object
#' # and is later used by forge()
#' forge(processed$preprocessor, iris)
#'
#' # ---------------------------------------------------------------------------
#' # Y as a vector
#'
#' # Often, `y` will be supplied as a vector to a model. `mold()` handles
#' # this by letting `standardize()` convert `y` to a tibble, adding the
#' # default column name, `".outcome"`.
#' y_vec <- y$Sepal.Length
#'
#' mold(x, y_vec)$outcomes
#'
#' @rdname mold-xy
#'
#' @export
mold.data.frame <- function(x, y, intercept = FALSE, ...) {

  engine <- new_default_preprocessor_engine()

  predictors <- mold_xy_predictors(engine, x, intercept)
  outcomes <- mold_xy_outcomes(y)

  preprocessor <- new_default_preprocessor(
    engine = engine,
    intercept = intercept,
    info = info_lst(
      predictors = predictors$info,
      outcomes = outcomes$info
    )
  )

  mold_list(
    predictors = predictors$data,
    outcomes$data,
    preprocessor
  )

}

#' @rdname mold-xy
#' @export
mold.matrix <- mold.data.frame

# ------------------------------------------------------------------------------

mold_xy_predictors <- function(engine, x, intercept) {

  info <- predictors_info(
    names = colnames(x),
    classes = get_data_classes(x),
    levels = get_levels(x)
  )

  # Process x _after_ extracting the "original"
  # predictor column names and classes
  x <- engine$process(x, intercept)

  list(
    data = x,
    info = info
  )

}

mold_xy_outcomes <- function(y) {

  y <- standardize(y)

  info = outcomes_info(
    names = colnames(y),
    classes = get_data_classes(y),
    levels = get_levels(y)
  )

  list(
    data = y,
    info = info
  )

}
