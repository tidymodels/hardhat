#' Create a new preprocessor
#'
#' A preprocessor holds a preprocessing engine, and various elements that are
#' useful for performing structural validation of `new_data` in `forge()` when
#' it is time to make predictions using a model.
#'
#' @param engine A preprocessing engine. This can be a
#' `"default_preprocessor_engine"`, a `"recipe"`, or a `"terms"` object.
#'
#' @param intercept A logical. Should an intercept be included?
#'
#' @param info A named list with 2 elements:
#'
#' - `"predictors"`: A named list with the following 3 elements:
#'
#'    - `"names"`: A character vector of the original predictor column names.
#'
#'    - `"classes"`: A named list of the original predictor classes, or `NULL`.
#'
#'    - `"levels"`: A named list of the original predictor levels for any factor
#'    columns, or `NULL`.
#'
#' - `"outcomes"`: A named list with the following 3 elements:
#'
#'    - `"names"`: A character vector of the original outcome column names.
#'
#'    - `"classes"`: A named list of the original outcome classes, or `NULL`.
#'
#'    - `"levels"`: A named list of the original outcome levels for any factor
#'    columns, or `NULL`.
#'
#' @param ... Name-value pairs of extra elements that should be added to
#' subclassed preprocessors.
#'
#' @param subclass An optional character. The subclass of the `"preprocessor"`.
#'
#' @keywords internal
NULL

# TODO can we adapt this documentation for the engines?
