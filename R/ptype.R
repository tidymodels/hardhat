#' Extract a prototype
#'
#' @description
#'
#' `extract_ptype()` extracts a tibble with 0 rows from `data`. This contains
#' all of the required information about column names, classes, and factor
#' levels that are required to check the structure of new data at prediction
#' time.
#'
#' @param data A data frame or matrix.
#'
#' @return
#'
#' A 0 row slice of `data` after converting it to a tibble.
#'
#' @details
#'
#' `extract_ptype()` is useful when creating a new preprocessing `blueprint`. It
#' extracts the required information that will be used by the validation checks
#' at prediction time.
#'
#' @examples
#'
#' hardhat:::extract_ptype(iris)
#' @keywords internal
#'
extract_ptype <- function(data) {
  if (is.null(data)) {
    return(NULL)
  }

  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  vec_slice(data, 0L)
}
