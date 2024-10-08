#' Recompose a data frame into another form
#'
#' @description
#' `recompose()` takes a data frame and converts it into one of:
#' - A tibble
#' - A data frame
#' - A matrix
#' - A sparse matrix (using the Matrix package)
#'
#' This is an internal function used only by hardhat and recipes.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param data A data frame.
#'
#' @param composition One of:
#'   - `"tibble"` to convert to a tibble.
#'   - `"data.frame"` to convert to a base data frame.
#'   - `"matrix"` to convert to a matrix. All columns must be numeric.
#'   - `"dgCMatrix"` to convert to a sparse matrix. All columns must be numeric,
#'     and the Matrix package must be installed.
#'
#' @returns
#' The output type is determined from the `composition`.
#'
#' @export
#' @keywords internal
#'
#' @examples
#' df <- vctrs::data_frame(x = 1)
#'
#' recompose(df)
#' recompose(df, composition = "matrix")
#'
#' # All columns must be numeric to convert to a matrix
#' df <- vctrs::data_frame(x = 1, y = "a")
#' try(recompose(df, composition = "matrix"))
recompose <- function(data,
                      ...,
                      composition = "tibble") {
  check_dots_empty0(...)
  check_data_frame(data)

  composition <- arg_match0(
    arg = composition,
    values = c("tibble", "data.frame", "matrix", "dgCMatrix")
  )

  switch(
    composition,
    tibble = {
      coerce_to_tibble(data)
    },
    data.frame = {
      new_data_frame(data, n = vec_size(data))
    },
    matrix = {
      coerce_to_matrix(data)
    },
    dgCMatrix = {
      if (is_sparse_tibble(data)) {
        sparsevctrs::coerce_to_sparse_matrix(data)
      } else {
        data <- coerce_to_matrix(data)
        coerce_to_sparse(data)
      }
    }
  )
}

coerce_to_matrix <- function(data, error_call = caller_env()) {
  numeric <- map_lgl(data, is.numeric)

  if (!all(numeric)) {
    loc <- which(!numeric)
    loc <- names(data)[loc]

    message <- c(
      "{.arg data} must only contain numeric columns.",
      i = "These columns aren't numeric: {.str {loc}}."
    )

    cli::cli_abort(message, call = error_call)
  }

  as.matrix(data)
}

coerce_to_sparse <- function(data, error_call = caller_env()) {
  check_installed("Matrix", call = error_call)
  Matrix::Matrix(data, sparse = TRUE)
}
