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
  cli::cli_abort("No {.fn standardize} method provided for {.obj_type_friendly {y}}.")
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
  check_unique_column_names(y)

  if (!is.numeric(y)) {
    stop_input_type(y, "a numeric matrix")
  }

  tibble::as_tibble(y, .name_repair = "minimal")
}

#' @export
standardize.array <- function(y) {
  if (!is.numeric(y)) {
    stop_input_type(y, "a numeric array")
  }

  # tibble() will strip the array class for us
  if (dims(y) == 1) {
    tibble::tibble(.outcome = y)
  } else if (dims(y) == 2) {
    standardize.matrix(y)
  } else {
    cli::cli_abort("3D+ arrays are not supported outcome types.")
  }
}

#' @export
standardize.data.frame <- function(y) {
  check_unique_column_names(y)
  validate_has_known_outcome_types(y)
  coerce_to_tibble(y)
}

is_known_output_type <- function(x) {
  is.numeric(x) || is.factor(x)
}

validate_has_known_outcome_types <- function(y) {
  known <- vapply(y, is_known_output_type, logical(1))

  if (!all(known)) {
    not_known <- which(!known)
    not_known <- colnames(y)[not_known]
    cli::cli_abort(
      c(
        "Not all columns of {.arg y} are known outcome types.",
        "i" = "{?This/These} column{?s} {?has/have} {?an/} unknown type{?s}:
              {.val {not_known}}."
      )
    )
  }

  invisible(y)
}
