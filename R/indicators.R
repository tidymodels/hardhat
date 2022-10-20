#' Get indicator matrix from factor
#'
#' Takes a factor vector and construct the corresponding indicator matrix, also
#' known as a one-hot encoding matrix.
#'
#' @param x A factor without missing values.
#'
#' @return A integer matrix with the same number of columns as levels of `x`,
#' and the same number of rows as the number of elements in `x`.
#'
#' @examples
#' factor_to_indicators(factor(letters))
#'
#' factor_to_indicators(factor(letters[1:2], levels = letters))
#'
#' set.seed(1234)
#' factor_to_indicators(factor(sample(letters[1:4], 10, TRUE)))
#' @export
factor_to_indicators <- function(x) {
  if (!is.factor(x)) {
    rlang::abort("`x` must be a factor.")
  }
  pos <- as.integer(x)
  if (anyNA(pos)) {
    rlang::abort("`x` must not have any missing values.")
  }

  levels <- levels(x)
  res <- matrix(0L, nrow = length(x), ncol = length(levels))
  colnames(res) <- levels

  for (i in seq_along(x)) {
    res[i, pos[i]] <- 1L
  }
  res
}
