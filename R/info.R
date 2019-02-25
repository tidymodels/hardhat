#' Extract class and type information
#'
#' @description
#'
#' `extract_info()` extracts the following pieces of information from `x`:
#'
#' - `names`: Using `colnames()`.
#'
#' - `classes`: Use [get_data_classes()].
#'
#' - `levels`: Using [get_levels()].
#'
#' @return
#'
#' A named list with 3 elements: `names`, `classes`, and `levels`.
#'
#' @param x A data frame or matrix.
#'
#' @details
#'
#' `extract_info()` is useful when creating new preprocessing `engine`. It
#' extracts the required information that will be used by the validation checks
#' at prediction time.
#'
#' @examples
#'
#' str(hardhat:::extract_info(iris))
#'
#' @keywords internal
#'
extract_info <- function(x) {

  x <- check_is_data_like(x)

  list(
    names = colnames(x),
    classes = get_data_classes(x),
    levels = get_levels(x)
  )

}
