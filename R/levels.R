#' Extract factor levels from a data frame
#'
#' `get_levels()` extracts the levels from any factor columns in `data`. It is
#' mainly useful for extracting the original factor levels from the predictors
#' in the training set. `get_outcome_levels()` is a small wrapper around
#' `get_levels()` for extracting levels from a factor outcome
#' that first calls [standardize()] on `y`.
#'
#' @inheritParams standardize
#'
#' @param data A data.frame to extract levels from.
#'
#' @return
#'
#' A named list with as many elements as there are factor columns in `data`
#' or `y`. The names are the names of the factor columns, and the values
#' are character vectors of the levels.
#'
#' If there are no factor columns, `NULL` is returned.
#'
#' @seealso [stats::.getXlevels()]
#'
#' @examples
#'
#' # Factor columns are returned with their levels
#' get_levels(iris)
#'
#' # No factor columns
#' get_levels(mtcars)
#'
#' # standardize() is first run on `y`
#' # which converts the input to a data frame
#' # with an automatically named column, `".outcome"`
#' get_outcome_levels(y = factor(letters[1:5]))
#'
#' @export
get_levels <- function(data) {

  if (!is.data.frame(data)) {
    return(NULL)
  }

  lvl_lst <- lapply(data, levels)

  null_elems <- vapply(lvl_lst, is.null, logical(1))

  if (all(null_elems)) {
    return(NULL)
  }

  lvl_lst[!null_elems]
}

#' @rdname get_levels
#' @export
get_outcome_levels <- function(y) {
  y <- standardize(y)
  get_levels(y)
}
