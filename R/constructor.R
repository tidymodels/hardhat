#' Constructor for a base model
#'
#' A __model__ is a _scalar object_, as classified in
#' [Advanced R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it
#' takes uniquely named elements in `...` and combines them into a list with
#' a class of `class`. This entire object represent a single model.
#'
#' Because every model should have multiple interfaces, including formula
#' and `recipes` interfaces, all models should have a `blueprint` that
#' can process new data when `predict()` is called. The easiest way to generate
#' an blueprint with all of the information required at prediction time is to
#' use the one that is returned from a call to [mold()].
#'
#' @param ... Name-value pairs for elements specific to the model defined by
#' `class`.
#'
#' @param blueprint A preprocessing `blueprint` returned from a call to [mold()].
#'
#' @param class A character vector representing the class of the model.
#'
#' @return
#'
#' A new scalar model object, represented as a classed list with named elements
#' specified in `...`.
#'
#' @examples
#' new_model(
#'   custom_element = "my-elem",
#'   blueprint = default_xy_blueprint(),
#'   class = "custom_model"
#' )
#' @export
new_model <- function(..., blueprint = default_xy_blueprint(), class = character()) {
  check_blueprint(blueprint)

  new_abstract_model(..., blueprint = blueprint, class = c(class, "hardhat_model"))
}

# ------------------------------------------------------------------------------

#' @export
print.hardhat_model <- function(x, ...) {
  cat_line("<", class(x)[1], ">")
  x$blueprint <- NULL
  print(unclass(x))
}

cat_line <- function(...) {
  cat(paste0(..., "\n", collapse = ""))
}

# ------------------------------------------------------------------------------

new_abstract_model <- function(..., class) {
  elems <- list2(...)
  check_unique_names(elems, arg = "...")

  new_scalar(elems, class = class)
}

new_scalar <- function(elems, ..., class = character()) {
  check_elems(elems)
  structure(elems, ..., class = c(class, "hardhat_scalar"))
}

# ------------------------------------------------------------------------------

check_elems <- function(elems) {
  if (!is.list(elems) || length(elems) == 0) {
    cli::cli_abort("{.arg elems} must be a list of length 1 or greater.")
  }

  if (!has_unique_names(elems)) {
    cli::cli_abort("{.arg elems} must have unique names.")
  }

  if (!identical(names(attributes(elems)), "names")) {
    cli::cli_abort("{.arg elems} must have no attributes (apart from names).")
  }

  invisible(elems)
}
