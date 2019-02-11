#' Internal preprocessing of new data
#'
#' `preprocess()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' The `preprocessor` can be a `recipe::recipes()` object, a `terms` object
#' resulting from the use of a formula method, or a `default_preprocessor()`
#' which converts `new_data` to a matrix and optionally adds an intercept.
#'
#' @inheritParams retype
#'
#' @param preprocessor A valid preprocessor. Can be a [recipes::recipe()], a
#' terms object, or the result of `default_preprocessor()`.
#'
#' @param new_data A data frame to preprocess.
#'
#' @param intercept A logical. Should an intercept column
#' be added to `new_data`?
#'
#' @return
#'
#' The preprocessed data, returned as a `type`.
#'
#' @export
preprocess <- function(preprocessor, new_data) {
  UseMethod("preprocess")
}

#' @export
preprocess.default <- function(preprocessor, new_data) {
  abort("Unknown preprocessor.")
}

#' @export
preprocess.default_preprocessor <- function(preprocessor, new_data) {
  preprocessor$engine$process(new_data, preprocessor$intercept, preprocessor$type)
}

#' @export
preprocess.recipes_preprocessor <- function(preprocessor, new_data) {

  validate_recipes_available()

  all_predictors <- recipes::all_predictors

  # bake() the recipe on the new_data
  # Remove the response and anything besides predictors to
  # ensure all columns are numeric
  new_data <- recipes::bake(preprocessor$engine, new_data = new_data, all_predictors())

  new_data <- retype(new_data, preprocessor$type)

  new_data <- add_intercept_column(new_data, preprocessor$intercept)

  new_data
}

#' @export
preprocess.terms_preprocessor <- function(preprocessor, new_data) {

  # `intercept` can be ignored here as it is added by the preprocessor

  # Don't attempt to include Y in the model.frame()
  x_terms <- delete_response(preprocessor$engine)

  # Ensure factors have no new levels
  # (we warn if they do and remove them)
  # (this is so model.frame(xlev) doesnt error out on new levels)
  new_data <- check_new_data_factor_levels(x_levels(x_terms), new_data)

  # This will detect any missing columns in new_data
  # that should be there, but the error message isn't fantastic.
  # Preprocessing should _never_ removes rows
  # with incomplete data. Setting the na.action
  # to na.pass will retain the NA values through
  # the preprocessing
  new_data <- rlang::with_options(
    model.frame(x_terms, data = new_data, xlev = x_levels(x_terms)),
    na.action = "na.pass"
  )

  validate_new_data_classes(x_terms, new_data)

  new_data <- rlang::with_options(
    model.matrix(x_terms, data = new_data),
    na.action = "na.pass"
  )

  new_data <- retype(new_data, preprocessor$type)

  new_data
}
