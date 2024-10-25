#' Encode a factor as a one-hot indicator matrix
#'
#' @description
#' `fct_encode_one_hot()` encodes a factor as a one-hot indicator matrix.
#'
#' This matrix consists of `length(x)` rows and `length(levels(x))` columns.
#' Every value in row `i` of the matrix is filled with `0L` except for the
#' column that has the same name as `x[[i]]`, which is instead filled with `1L`.
#'
#' @details
#' The columns are returned in the same order as `levels(x)`.
#'
#' If `x` has names, the names are propagated onto the result as the row names.
#'
#' @param x A factor.
#'
#'   `x` can't contain missing values.
#'
#'   `x` is allowed to be an ordered factor.
#'
#' @return An integer matrix with `length(x)` rows and `length(levels(x))`
#'   columns.
#'
#' @export
#' @examples
#' fct_encode_one_hot(factor(letters))
#'
#' fct_encode_one_hot(factor(letters[1:2], levels = letters))
#'
#' set.seed(1234)
#' fct_encode_one_hot(factor(sample(letters[1:4], 10, TRUE)))
fct_encode_one_hot <- function(x) {
  if (!is.factor(x)) {
    cli::cli_abort("{.arg x} must be a factor, not {.obj_type_friendly {x}}.")
  }

  row_names <- names(x)
  col_names <- levels(x)
  dim_names <- list(row_names, col_names)

  n_cols <- length(col_names)
  n_rows <- length(x)

  x <- unclass(x)

  if (vec_any_missing(x)) {
    cli::cli_abort("{.arg x} can't contain missing values.")
  }

  out <- matrix(0L, nrow = n_rows, ncol = n_cols, dimnames = dim_names)

  # Use integer matrix indexing to assign the `1`s
  loc <- cbind(row = seq_len(n_rows), col = x)
  out[loc] <- 1L

  out
}
