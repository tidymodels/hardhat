#' Constructor for a base model
#'
#' A __model__ is a _scalar object_, as classified in
#' [Advanced R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it
#' takes uniquely named elements in `...` and combines them into a list with
#' a class of `class`. This entire object represent a single model.
#'
#' Because every model should have multiple interfaces, including formula
#' and `recipes` interfaces, all models should have an `engine` that
#' can process new data when `predict()` is called. The easiest way to generate
#' an engine with all of the information required at prediction time is to
#' use the one that is returned from a call to [mold()].
#'
#' @param ... Name-value pairs for elements specific to the model defined by
#' `class`.
#'
#' @param engine A preprocessing `engine` returned from a call to [mold()].
#'
#' @param class A character vector representing the class of the model.
#'
#' @examples
#' new_model(
#'   custom_element= "my-elem",
#'   engine = default_xy_engine(),
#'   class = "custom_model"
#' )
#'
#' @export
new_model <- function(..., engine = default_xy_engine(), class = character()) {

  validate_is_engine(engine)

  new_abstract_model(..., engine = engine, class = class)
}

new_abstract_model <- function(..., class = character()) {

  if (is_missing(class)) {
    abort("A model `class` must be provided.")
  }

  elems <- rlang::list2(...)
  validate_has_unique_names(elems, "...")

  new_scalar(elems, class = c(class, "hardhat_model"))
}

new_scalar <- function(elems, ..., class = character()) {
  check_elems(elems)
  structure(elems, ..., class = c(class, "hardhat_scalar"))
}

check_elems <- function(elems) {

  if (!is.list(elems) || length(elems) == 0) {
    abort("`elems` must be a list of length 1 or greater.")
  }

  if (!has_unique_names(elems)) {
    abort("`elems` must have unique names.")
  }

  if (!identical(names(attributes(elems)), "names")) {
    abort("`elems` must have no attributes (apart from names).")
  }

  invisible(elems)
}

validate_is_engine <- function(engine) {
  validate_is(engine, is_engine, "engine")
}
