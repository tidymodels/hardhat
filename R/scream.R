#' \if{html}{\Sexpr[stage=render,results=rd]{"\U0001f631"}} Scream.
#'
#' @description
#'
#' `scream()` ensures that the structure of `data` is the same as `reference`.
#' Under the hood, [vctrs::vec_cast()] is used, which casts each column of
#' `data` to the same type as the corresponding column in `reference`.
#'
#' This casting enforces a number of important structural checks,
#' including but not limited to:
#'
#' - _Data Classes_ - Checks that the class of each column in `data` is the
#' same as the corresponding column in `reference`.
#'
#' - _Novel Levels_ - Checks that the factor columns in `data` don't have any
#' _new_ levels when compared with the `reference` columns. If there are new
#' levels, a warning is issued and they are coerced to `NA`.
#'
#' - _Level Recovery_ - Checks that the factor columns in `data` aren't
#' missing any factor levels when compared with the `reference` columns. If
#' there are missing levels, then they are restored.
#'
#' @details
#'
#' `scream()` is called by [forge()] after [shrink()] but before the
#' actual processing is done. Generally, you don't need to call `scream()`
#' directly, as `forge()` will do it for you.
#'
#' If `scream()` is used as a standalone function, it is good practice to call
#' [shrink()] right before it as there are no checks in `scream()` that ensure
#' that all of the required column names actually exist in `data`. Those
#' checks exist in `shrink()`.
#'
#' @param data A data frame containing the new data to check the structure
#' of.
#'
#' @param reference A data frame to cast `data` to.
#'
#' @return
#'
#' A tibble containing the required columns after any required structural
#' modifications have been made.
#'
#' @examples
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # mold() is run at model fit time
#' # and a formula preprocessing engine is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Inside the result of mold() are the reference tibbles
#' # for the predictors and the outcomes
#' ref_pred <- x$engine$info$predictors
#' ref_out <- x$engine$info$outcomes
#'
#' # Pass that engine to shrink(), along with new_data
#' # to get a tibble of required predictors back
#' test_shrunk <- shrink(test, x$engine)
#'
#' # Now pass that to scream() to perform validation checks
#' # Silence is key!
#' scream(test_shrunk, ref_pred)
#'
#' # If `outcomes = TRUE` is used with shrink(),
#' # it should also be used with scream()
#' test_outcome <- shrink(test, x$engine, outcomes = TRUE)
#' scream(test_outcome, ref_out)
#'
#' # scream() validates that the classes of `new_data`
#' # are the same as the ones used in mold(). The below call
#' # to scream() will fail with an informative error.
#' test2 <- test
#' test2$Species <- as.character(test2$Species)
#'
#' \dontrun{
#' scream(test2, ref_pred)
#' }
#'
#' @export
scream <- function(data, reference) {

  if (is.null(data)) {
    return(NULL)
  }

  data <- check_is_data_like(data, "data")

  # can imagine this catching and rethrowing errors / warnings related to:
  # - factor recovery
  #   - silently recover missing levels
  #   - novel levels dropped. warning only if its actually used in the data and the data becomes NA
  # - ordered factor recovery
  #   - order is always recovered
  #   - same as factor otherwise
  # - errors in casting individual columns to the reference type
  #   - will silently do it if possible

  # NOT caring about too many columns / missing columns, as that was taken care of by shrink
  # and the vctrs behavior (adding NA columns) isn't great here

  # TODO waiting on https://github.com/r-lib/vctrs/issues/225 thoughts

  vctrs::vec_cast(data, reference)
}
