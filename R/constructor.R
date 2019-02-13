#' Constructor for a base model
#'
#' A __model__ is a _scalar object_, as classified in
#' [Advanced R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it
#' takes uniquely named fields in `...` and combines them into a list with
#' a class of `class`. This entire object represent a single model.
#'
#' Because every model should have multiple interfaces, including formula
#' and `recipes` interfaces, all models should have a `preprocessor` that
#' can process new data when `predict()` is called. The default `preprocessor`
#' used in the data frame and matrix methods is `default_preprocessor()`, which
#' converts an object to a data frame or matrix, and adds an intercept column
#' if required.
#'
#' @param preprocessor A preprocessor for new data. This can be
#' a `terms` object from the formula interface, a `recipe` created with
#' [recipes::recipe()], or the default which converts to either a matrix or a
#' data frame, and adds an intercept column if requested.
#'
#' @param ... Name-value pairs for elements specific to the model defined by
#' `class`.
#'
#' @param class A character vector representing the class of the model.
#'
#' @examples
#'
#' new_base_model(
#'   custom_field = "my-field",
#'   class = "custom_model"
#' )
#'
#' @export
new_base_model <- function(preprocessor = NULL, ..., class) {

  if (is.null(preprocessor)) {
    preprocessor <- new_default_preprocessor()
  }

  validate_is_preprocessor(preprocessor)

  new_fields <- rlang::list2(...)
  validate_has_unique_names(new_fields, "...")

  fields <- list(preprocessor = preprocessor)

  fields <- c(fields, new_fields)

  if (is_missing(class)) {
    abort("A model `class` must be provided.")
  }

  structure(fields, class = class)
}
