#' Coerce predictors to a new type
#'
#' `retype()` is a simple function that coerces the predictors to a tibble,
#' data.frame or a matrix.
#'
#' `retype()` is used from [mold()] and [forge()].
#'
#' @param x Predictors. These are a data frame or a matrix.
#' @param type A single character. One of `"tibble"`, `"data.frame"`, or
#' `"matrix"`.
#'
#' @keywords internal
retype <- function(x, type = "tibble") {

  validate_type(type)

  if (type == "data.frame") {
    x <- as.data.frame(x)
  }

  if (type == "matrix") {
    x <- as.matrix(x)
  }

  if (type == "tibble") {
    x <- tibble::as_tibble(x)
  }

  x
}

validate_type <- function(type) {

  if (!rlang::is_scalar_character(type)) {
    abort_valid_type()
  }

  if (!is_valid_type(type)) {
    abort_valid_type()
  }

  invisible(type)
}

is_valid_type <- function(x) {
  x %in% valid_types
}

abort_valid_type <- function() {
  types <- glue::glue_collapse(glue::single_quote(valid_types), sep = ", ")
  glubort("`type` must be a single character, and one of: {types}.")
}

valid_types <- c("tibble", "data.frame", "matrix")
