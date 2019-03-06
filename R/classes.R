#' Extract data classes from a data frame or matrix
#'
#' When predicting from a model, it is often important for the `new_data` to
#' have the same classes as the original data used to fit the model.
#' `get_data_classes()` extracts the classes from the original training data.
#'
#' @param x A data frame or matrix.
#'
#' @return
#'
#' A named list. The names are the column names of `x` and the values are
#' character vectors containing the class of that column.
#'
#' @examples
#' get_data_classes(iris)
#'
#' get_data_classes(as.matrix(mtcars))
#'
#' # Unlike .MFclass(), the full class
#' # vector is returned
#' x <- data.frame(col = ordered(c("a", "b")))
#'
#' .MFclass(x$col)
#'
#' get_data_classes(x)
#'
#' @export
get_data_classes <- function(x) {

  ptype <- extract_ptype(x)

  validate_has_unique_column_names(ptype, "x")

  lapply(ptype, class)
}
