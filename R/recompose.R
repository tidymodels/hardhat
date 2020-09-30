# This is the same as the "recomposition" at the end of recipes::bake()

recompose <- function(data, composition) {
  if (composition == "tibble") {
    return(data)
  } else if (composition == "dgCMatrix") {
    data <- convert_matrix(data, sparse = TRUE)
  } else if (composition == "matrix") {
    data <- convert_matrix(data, sparse = FALSE)
  }
  data
}

convert_matrix <- function(x, sparse = TRUE) {
  is_num <- vapply(x, is.numeric, logical(1))

  if (!all(is_num)) {
    num_viol <- sum(!is_num)
    if (num_viol < 5)
      rlang::abort(
        paste0(
          "Columns (",
          paste0("`", names(is_num)[!is_num], "`", collapse = ", "),
          ") are not numeric; cannot convert to matrix."
        )
      )
    else
      rlang::abort(
        paste0(
          num_viol,
          " columns are not numeric; cannot ",
          "convert to matrix."
        )
      )
  }

  # At this point, all cols are numeric so we can just use as.matrix()
  res <- as.matrix(x)

  if (sparse) {
    res <- Matrix::Matrix(res, sparse = TRUE)
  }

  res
}

validate_composition <- function(composition) {
  rlang::arg_match0(
    arg = composition,
    values = c("tibble", "matrix", "dgCMatrix"),
    arg_nm = "composition"
  )
}
