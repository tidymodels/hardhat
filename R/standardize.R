#' Standardize the outcome
#'
#' Most of the time, the input to a model should be flexible enough to capture
#' a number of different input types from the user. `standardize()` focuses
#' on capturing the flexibility in the _outcome_.
#'
#' `standardize()` is called from [mold()] when using an XY interface (i.e.
#' a `y` argument was supplied).
#'
#' @param y The outcome. This can be:
#' - A factor vector
#' - A numeric vector
#' - A 1D numeric array
#' - A numeric matrix with column names
#' - A 2D numeric array with column names
#' - A data frame with numeric or factor columns
#'
#' @return
#'
#' All possible values of `y` are transformed into a `tibble` for
#' standardization. Vectors are transformed into a `tibble` with
#' a single column named `".outcome"`.
#'
#' @examples
#' standardize(1:5)
#'
#' standardize(factor(letters[1:5]))
#'
#' mat <- matrix(1:10, ncol = 2)
#' colnames(mat) <- c("a", "b")
#' standardize(mat)
#'
#' df <- data.frame(x = 1:5, y = 6:10)
#' standardize(df)
#' @export
standardize <- function(y) {
  UseMethod("standardize")
}

#' @export
standardize.default <- function(y) {
  glubort("`y` is of unknown type '{class1(y)}'.")
}

#' @export
standardize.factor <- function(y) {
  tibble::tibble(.outcome = y)
}

#' @export
standardize.integer <- function(y) {
  tibble::tibble(.outcome = y)
}

#' @export
standardize.double <- function(y) {
  tibble::tibble(.outcome = y)
}

#' @export
standardize.matrix <- function(y) {
  validate_has_unique_column_names(y, "y")
  validate_numeric_elements(y, "y")
  tibble::as_tibble(y)
}

#' @export
standardize.array <- function(y) {
  validate_numeric_elements(y, "y")

  # tibble() will strip the array class for us
  if (dims(y) == 1) {
    tibble::tibble(.outcome = y)
  } else if (dims(y) == 2) {
    standardize.matrix(y)
  } else {
    glubort("3D+ arrays are not supported outcome types.")
  }
}

#' @export
standardize.data.frame <- function(y) {
  validate_has_unique_column_names(y, "y")
  validate_has_known_outcome_types(y)
  tibble::as_tibble(y)
}

is_known_output_type <- function(x) {
  is.numeric(x) || is.factor(x)
}

validate_has_known_outcome_types <- function(y) {
  known <- vapply(y, is_known_output_type, logical(1))

  if (!all(known)) {
    not_known <- which(!known)
    not_known <- colnames(y)[not_known]
    not_known <- glue_quote_collapse(not_known)

    glubort(
      "Not all columns of `y` are known outcome types. ",
      "These columns have unknown types: {not_known}."
    )
  }

  invisible(y)
}
