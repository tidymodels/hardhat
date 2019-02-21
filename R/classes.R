#' Extract data classes from a data frame or matrix
#'
#' When predicting from a model, it is often important for the `new_data` to
#' have the same classes as the original data used to fit the model.
#' `get_data_classes()` extracts the classes from the original training data.
#'
#' `get_data_classes()` is inspired by [stats::.MFclass()]. It returns the
#' exact classes of the data frame columns with the one exception that integer
#' and double columns are both returned as `"numeric"`. This is to ensure
#' that if an integer column is used when fitting, an error is not thrown if
#' a double column is used at prediction time. It also does not allow for
#' any columns of `x` to be multivariate themselves (no data frame columns,
#' or matrix columns).
#'
#' The classes returned by `get_data_classes()` are stored in the preprocessor
#' objects. This function is called by [mold()].
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
#' @export
get_data_classes <- function(x) {

  validate_has_unique_column_names(x, "x")

  if (is.matrix(x)) {
    nms <- colnames(x)
    cls <- get_data_class(x[,1])
    out <- rlang::rep_named(nms, list(cls))
    return(out)
  }

  if (is.data.frame(x)) {
    out <- lapply(x, get_data_class)
    return(out)
  }

  glubort("`x` must be a data frame or matrix to extract classes for.")
}

# Like .MFclass(), but better
get_data_class <- function(x) {

  if (dims(x) > 1) {
    glubort("Each element of `x` can only be a 1D vector.")
  }

  # doubles and integers
  if (rlang::is_bare_numeric(x)) {
    return("numeric")
  }

  class(x)
}
