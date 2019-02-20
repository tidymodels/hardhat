#' Extract a model offset
#'
#' `model_offset()` extracts a numeric offset from a model frame. It is
#' inspired by [stats::model.offset()], but has nicer error messages and
#' is slightly stricter.
#'
#' @param data A data frame that was the result of a call to `model_frame()`.
#' @param terms A `"terms"` object corresponding to `data`. This is also
#' returned by `model_frame()`.
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
#' model_offset(x, terms(x))
#'
#' xx <- model.frame(Species ~ offset(Sepal.Width) + offset(Sepal.Length), iris)
#'
#' model_offset(xx, terms(xx))
#'
#' # Problematic columns are caught with intuitive errors
#' tryCatch(
#'   expr = {
#'     x <- model.frame(~ offset(Species), iris)
#'     model_offset(x, terms(x))
#'   },
#'   error = function(e) {
#'     print(e$message)
#'   }
#' )
#'
#' @export
model_offset <- function (data, terms) {

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

# model.matrix() does this automatically, but
# we have to do it separately here
# TODO - remove this since we always run model.matrix()
# on the RHS now?
remove_offsets <- function(frame) {

  .offset_pos <- attr(attr(frame, "terms"), "offset")

  has_offset <- !is.null(.offset_pos)

  if (has_offset) {
    frame <- frame[, -.offset_pos, drop = FALSE]
  }

  frame

}

extract_offset <- function(data, terms) {

  .offset <- model_offset(data, terms)

  if (is.null(.offset)) {
    NULL
  }
  else {
    tibble::tibble(.offset = .offset)
  }

}
