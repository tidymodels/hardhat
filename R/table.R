#' Weighted table
#'
#' @description
#' `weighted_table()` computes a weighted contingency table based on factors
#' provided in `...` and a double vector of weights provided in `weights`. It
#' can be seen as a weighted extension to [base::table()] and an alternative
#' to [stats::xtabs()].
#'
#' `weighted_table()` always uses the _exact_ set of levels returned by
#' `levels()` when constructing the table. This results in the following
#' properties:
#'
#' - Missing values found in the factors are never included in the table unless
#' there is an explicit `NA` factor level. If needed, this can be added to a
#' factor with [base::addNA()] or `forcats::fct_expand(x, NA)`.
#'
#' - Levels found in the factors that aren't actually used in the underlying
#' data are included in the table with a value of `0`. If needed, you can
#' drop unused factor levels by re-running your factor through [factor()],
#' or by calling `forcats::fct_drop()`.
#'
#' See the examples section for more information about these properties.
#'
#' @details
#' The result of `weighted_table()` does not have a `"table"` class attached
#' to it. It is only a double array. This is because "table" objects are
#' defined as containing integer counts, but weighted tables can utilize
#' fractional weights.
#'
#' @param ... Factors of equal length to use in the weighted table. If the
#'   `...` are named, those names will propagate onto the "dimnames names" of
#'   the resulting table. At least one factor must be provided.
#'
#' @param weights A double vector of weights used to fill the cells of the
#'   weighted table. This must be the same length as the factors provided in
#'   `...`.
#'
#' @param na_remove A single `TRUE` or `FALSE` for handling whether or not
#'   missing values in `weights` should be removed when summing up the weights.
#'
#' @return
#' The weighted table as an array of double values.
#'
#' @export
#' @examples
#' x <- factor(c("x", "y", "z", "x", "x", "y"))
#' y <- factor(c("a", "b", "a", "a", "b", "b"))
#' w <- c(1.5, 2, 1.1, .5, 3, 2)
#'
#' weighted_table(x = x, y = y, weights = w)
#'
#' # ---------------------------------------------------------------------------
#' # If `weights` contains missing values, then missing values will be
#' # propagated into the weighted table
#' x <- factor(c("x", "y", "y"))
#' y <- factor(c("a", "b", "b"))
#' w <- c(1, NA, 3)
#'
#' weighted_table(x = x, y = y, weights = w)
#'
#' # You can remove the missing values while summing up the weights with
#' # `na_remove = TRUE`
#' weighted_table(x = x, y = y, weights = w, na_remove = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # If there are missing values in the factors, those typically don't show
#' # up in the weighted table
#' x <- factor(c("x", NA, "y", "x"))
#' y <- factor(c("a", "b", "a", NA))
#' w <- 1:4
#'
#' weighted_table(x = x, y = y, weights = w)
#'
#' # This is because the missing values aren't considered explicit levels
#' levels(x)
#'
#' # You can force them to show up in the table by using `addNA()` ahead of time
#' # (or `forcats::fct_expand(x, NA)`)
#' x <- addNA(x, ifany = TRUE)
#' y <- addNA(y, ifany = TRUE)
#' levels(x)
#'
#' weighted_table(x = x, y = y, weights = w)
#'
#' # ---------------------------------------------------------------------------
#' # If there are levels in your factors that aren't actually used in the
#' # underlying data, then they will still show up in the table with a `0` value
#' x <- factor(c("x", "y", "x"), levels = c("x", "y", "z"))
#' y <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
#' w <- 1:3
#'
#' weighted_table(x = x, y = y, weights = w)
#'
#' # If you want to drop these empty factor levels from the result, you can
#' # rerun `factor()` ahead of time to drop them (or `forcats::fct_drop()`)
#' x <- factor(x)
#' y <- factor(y)
#' levels(x)
#'
#' weighted_table(x = x, y = y, weights = w)
weighted_table <- function(..., weights, na_remove = FALSE) {
  args <- list2(...)
  n_args <- length(args)

  if (n_args == 0L) {
    abort("At least one vector must be supplied to `...`.")
  }
  if (!all(map_lgl(args, is.factor))) {
    abort("All elements of `...` must be factors.")
  }

  sizes <- list_sizes(args)
  size <- sizes[[1L]]

  if (!all(sizes == size)) {
    abort("All elements of `...` must be the same size.")
  }

  weights <- vec_cast(weights, to = double())
  vec_assert(weights, size = size)

  if (!is_bool(na_remove)) {
    abort("`na_remove` must be a single `TRUE` or `FALSE`.")
  }

  tapply(
    X = weights,
    INDEX = args,
    FUN = sum,
    na.rm = na_remove,
    default = 0,
    simplify = TRUE
  )
}
