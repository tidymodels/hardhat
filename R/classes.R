#' Extract data classes from a data frame or matrix
#'
#' When predicting from a model, it is often important for the `new_data` to
#' have the same classes as the original data used to fit the model.
#' `get_data_classes()` extracts the classes from the original training data.
#'
#' @param data A data frame or matrix.
#' 
#' @inheritParams validate_column_names
#'
#' @return
#'
#' A named list. The names are the column names of `data` and the values are
#' character vectors containing the class of that column.
#'
#' @examples
#' get_data_classes(iris)
#'
#' get_data_classes(as.matrix(mtcars))
#'
#' # Unlike .MFclass(), the full class
#' # vector is returned
#' data <- data.frame(col = ordered(c("a", "b")))
#'
#' .MFclass(data$col)
#'
#' get_data_classes(data)
#' @export
get_data_classes <- function(data, ..., call = current_env()) {
  check_dots_empty0(...)
  data <- extract_ptype(data, call = call)
  check_unique_column_names(data, call = call)
  lapply(data, class)
}
