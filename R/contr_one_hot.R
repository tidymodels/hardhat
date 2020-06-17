#' Contrast function for one-hot encodings
#'
#' This contrast function produces a model matrix that has indicator columns for
#' each level of each factor.
#'
#' @param n A vector of factor levels or the number of unique levels.
#' @param contrasts Should contrasts be computed. Only a value of `TRUE` is used
#'  (this argument is included for S comparability).
#' @param sparse A logical for a sparse matrices. Only non-sparse formats are
#'  currently implemented.
#' @return A diagonal matrix that is `n`-by-`n`.
#' @examples
#' dat <-
#'   data.frame(
#'     x = 1:12,
#'     y = factor(rep(letters[1:3], each = 4)),
#'     z = factor(rep(LETTERS[1:2], 6))
#'   )
#'
#' ## -----------------------------------------------------------------------------
#'
#' # No intercept does not produce all columns for all factors:
#' model.matrix(x ~ y + z + 0, data = dat)
#'
#' ## -----------------------------------------------------------------------------
#'
#' # Save current contrast to reset at bottom
#' old_contr <- options("contrasts")$contrasts
#' new_contr <- old_contr
#' new_contr["unordered"] <- "contr_one_hot"
#' options(contrasts = new_contr)
#'
#' ## -----------------------------------------------------------------------------
#'
#' model.matrix(x ~ y + z, data = dat)
#'
#' ## -----------------------------------------------------------------------------
#'
#' options(contrasts = old_contr)
#'
#' @export
contr_one_hot <- function(n, contrasts = TRUE, sparse = FALSE) {
  if (sparse) {
    rlang::warn("'sparse' not implmented for `one_hot_contrast()`")
  }
  if (!contrasts) {
    rlang::warn("'contrasts = FALSE' not implmented for `one_hot_contrast()`")
  }
  vals <- n
  n <- length(vals)
  if (!is.character(vals)) {
    vals <- format(seq_along(vals))
  }
  res <- diag(n)
  rownames(res) <- vals
  colnames(res) <- vals
  res
}
