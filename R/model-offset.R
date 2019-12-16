#' Extract a model offset
#'
#' `model_offset()` extracts a numeric offset from a model frame. It is
#' inspired by [stats::model.offset()], but has nicer error messages and
#' is slightly stricter.
#'
#' @param terms A `"terms"` object corresponding to `data`, returned from a
#' call to `model_frame()`.
#'
#' @param data A data frame returned from a call to `model_frame()`.
#'
#' @return
#'
#' A numeric vector representing the offset.
#'
#' @details
#'
#' If a column that has been tagged as an offset is not numeric, a nice error
#' message is thrown telling you exactly which column was problematic.
#'
#' [stats::model.offset()] also allows for a column named `"(offset)"` to be
#' considered an offset along with any others that have been tagged by
#' [stats::offset()]. However, [stats::model.matrix()] does not recognize
#' these columns as offsets (so it doesn't remove them as it should). Because
#' of this inconsistency, columns named `"(offset)"` are _not_ treated specially
#' by `model_offset()`.
#'
#' @examples
#'
#' x <- model.frame(Species ~ offset(Sepal.Width), iris)
#'
#' model_offset(terms(x), x)
#'
#' xx <- model.frame(Species ~ offset(Sepal.Width) + offset(Sepal.Length), iris)
#'
#' model_offset(terms(xx), xx)
#'
#' # Problematic columns are caught with intuitive errors
#' tryCatch(
#'   expr = {
#'     x <- model.frame(~ offset(Species), iris)
#'     model_offset(terms(x), x)
#'   },
#'   error = function(e) {
#'     print(e$message)
#'   }
#' )
#'
#' @export
model_offset <- function (terms, data) {

  .offset_pos <- attr(terms, "offset")

  has_offset <- !is.null(.offset_pos)

  if (!has_offset) {
    return(NULL)
  }

  ans <- rep(0, times = nrow(data))

  for (.pos in .offset_pos) {

    .offset_val <- data[[.pos]]

    if (!is.numeric(.offset_val)) {

      bad_col <- colnames(data)[.pos]

      glubort(
        "Column, '{bad_col}', is tagged as an offset, but is not numeric. ",
        "All offsets must be numeric."
      )

    }

    ans <- ans + .offset_val
  }

  ans
}

extract_offset <- function(terms, data) {

  .offset <- model_offset(terms, data)

  if (is.null(.offset)) {
    NULL
  }
  else {
    tibble::tibble(.offset = .offset)
  }

}
