#' Generics for object extraction
#'
#' @description
#' These generics are used to extract elements from various model
#' objects. Methods are defined in other packages, such as tune,
#' workflows, and workflowsets, but the returned object is always the same.
#'
#' - `extract_workflow()` returns a workflow, possibly fit.
#'
#' - `extract_recipe()` returns a recipe, possibly prepped.
#'
#' - `extract_parsnip_spec()` returns a parsnip model specification.
#'
#' - `extract_parsnip_fit()` returns a parsnip model fit.
#'
#' - `extract_engine_fit()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using `parsnip::linear_reg()`
#'   with the `"lm"` engine, this would return the underlying lm object.
#'
#'
#' @param x An object.
#'
#' @param ... Extra arguments passed on to methods.
#'
#' @name hardhat-extract
#'
#' @examples
#' # See packages where methods are defined for examples.
NULL

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
extract_parsnip_spec <- function(x, ...) {
  UseMethod("extract_parsnip_spec")
}

#' @rdname hardhat-extract
#' @export
extract_parsnip_fit <- function(x, ...) {
  UseMethod("extract_parsnip_fit")
}

#' @rdname hardhat-extract
#' @export
extract_engine_fit <- function(x, ...) {
  UseMethod("extract_engine_fit")
}
