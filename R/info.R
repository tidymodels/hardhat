#' Extract a prototype
#'
#' @description
#'
#' `extract_ptype()` extracts a tibble with 0 rows from `x`. This contains
#' all of the required information about column names, classes, and factor
#' levels that are required to check the structure of new data at prediction
#' time.
#'
#' @return
#'
#' A 0 row slice of `x` after converting it to a tibble.
#'
#' @param x A data frame or matrix.
#'
#' @details
#'
#' `extract_ptype()` is useful when creating a new preprocessing `engine`. It
#' extracts the required information that will be used by the validation checks
#' at prediction time.
#'
#' @examples
#'
#' hardhat:::extract_ptype(iris)
#'
#' @keywords internal
#'
extract_ptype <- function(x) {

  x <- check_is_data_like(x)

  vctrs::vec_slice(x, 0L)
}
