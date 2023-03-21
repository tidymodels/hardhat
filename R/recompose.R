# This is the same as the "recomposition" at the end of recipes::bake()

recompose <- function(data, composition) {
  if (identical(composition, "tibble")) {
    data
  } else if (identical(composition, "dgCMatrix")) {
    convert_matrix(data, sparse = TRUE)
  } else if (identical(composition, "matrix")) {
    convert_matrix(data, sparse = FALSE)
  } else {
    abort("Internal error: Unknown `composition` type.")
  }
}

convert_matrix <- function(x, sparse = TRUE) {
  is_num <- vapply(x, is.numeric, logical(1))

  if (!all(is_num)) {
    num_viol <- sum(!is_num)
    if (num_viol < 5) {
      abort(
        paste0(
          "Columns (",
          paste0("`", names(is_num)[!is_num], "`", collapse = ", "),
          ") are not numeric; cannot convert to matrix."
        )
      )
    } else {
      abort(
        paste0(
          num_viol,
          " columns are not numeric; cannot ",
          "convert to matrix."
        )
      )
    }
  }

  # At this point, all cols are numeric so we can just use as.matrix()
  res <- as.matrix(x)

  if (sparse) {
    if (!is_installed("Matrix")) {
      abort("The Matrix package must be installed to use a 'dgCMatrix' `composition`")
    }
    res <- Matrix::Matrix(res, sparse = TRUE)
  }

  res
}
