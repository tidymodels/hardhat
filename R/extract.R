#' Generics for object extraction
#'
#' @description
#' These generics are used to extract elements from various model
#' objects. Methods are defined in other packages, such as tune,
#' workflows, and workflowsets, but the returned object is always the same.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using `parsnip::linear_reg()`
#'   with the `"lm"` engine, this returns the underlying `lm` object.
#'
#' - `extract_fit_parsnip()` returns a parsnip model fit.
#'
#' - `extract_mold()` returns the preprocessed "mold" object returned
#'   from [mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe, the formula terms object, or
#'   variable selectors.
#'
#' - `extract_spec_parsnip()` returns a parsnip model specification.
#'
#' - `extract_preprocessor()` returns the formula, recipe, or variable
#'   expressions used for preprocessing.
#'
#' - `extract_recipe()` returns a recipe, possibly estimated.
#'
#' - `extract_tailor()` returns a tailor, possibly fit.
#'
#' - `extract_workflow()` returns a workflow, possibly fit.
#'
#' - `extract_parameter_dials()` returns a single dials parameter object.
#'
#' - `extract_parameter_set_dials()` returns a set of dials parameter objects.
#'
#' - `extract_fit_time()` returns a tibble with fit times.
#'
#' @param x An object.
#'
#' @param ... Extra arguments passed on to methods.
#'
#' @name hardhat-extract
#'
#' @examples
#' # See packages where methods are defined for examples, such as `parsnip` or
#' # `workflows`.
NULL

# nocov start

#' @rdname hardhat-extract
#' @export
extract_workflow <- function(x, ...) {
  UseMethod("extract_workflow")
}

#' @rdname hardhat-extract
#' @export
extract_recipe <- function(x, ...) {
  UseMethod("extract_recipe")
}

#' @rdname hardhat-extract
#' @export
extract_spec_parsnip <- function(x, ...) {
  UseMethod("extract_spec_parsnip")
}

#' @rdname hardhat-extract
#' @export
extract_fit_parsnip <- function(x, ...) {
  UseMethod("extract_fit_parsnip")
}

#' @rdname hardhat-extract
#' @export
extract_fit_engine <- function(x, ...) {
  UseMethod("extract_fit_engine")
}

#' @rdname hardhat-extract
#' @export
extract_mold <- function(x, ...) {
  UseMethod("extract_mold")
}

#' @rdname hardhat-extract
#' @export
extract_preprocessor <- function(x, ...) {
  UseMethod("extract_preprocessor")
}

#' @rdname hardhat-extract
#' @export
extract_postprocessor <- function(x, ...) {
  UseMethod("extract_postprocessor")
}

#' @rdname hardhat-extract
#' @export
extract_tailor <- function(x, ...) {
  UseMethod("extract_tailor")
}

#' @rdname hardhat-extract
#' @export
extract_parameter_dials <- function(x, ...) {
  UseMethod("extract_parameter_dials")
}

#' @rdname hardhat-extract
#' @export
extract_parameter_set_dials <- function(x, ...) {
  UseMethod("extract_parameter_set_dials")
}

#' @rdname hardhat-extract
#' @export
extract_fit_time <- function(x, ...) {
  UseMethod("extract_fit_time")
}

# nocov end
