#' Constructor for a base model
#'
#' A __model__ is a _scalar object_, as classified in
#' [Advanced R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it
#' takes uniquely named fields in `...` and combines them into a list with
#' a class of `class`. This entire object represent a single model.
#'
#' Because every model should have multiple interfaces, including formula
#' and `recipes` interfaces, all models should have an `engine` that
#' can process new data when `predict()` is called. The easiest way to generate
#' an engine with all of the information required at prediction time is to
#' use the one that is returned from a call to [mold()].
#'
#' @param engine A preprocessing `engine` returned from a call to [mold()].
#'
#' @param ... Name-value pairs for elements specific to the model defined by
#' `class`.
#'
#' @param class A character vector representing the class of the model.
#'
#' @examples
#'
#' new_base_model(
#'   engine = new_default_xy_engine(),
#'   custom_field = "my-field",
#'   class = "custom_model"
#' )
#'
#' @export
new_base_model <- function(engine, ..., class) {

  if (rlang::is_missing(engine)) {
    abort("`engine` is missing, but a preprocessing engine is required.")
  }

  if (is_missing(class)) {
    abort("A model `class` must be provided.")
  }

  validate_is_engine(engine)

  new_fields <- rlang::list2(...)
  validate_has_unique_names(new_fields, "...")

  fields <- list(engine = engine)

  fields <- c(fields, new_fields)

  structure(fields, class = class)
}

validate_is_engine <- function(engine) {
  validate_is(engine, is_engine, "engine")
}
