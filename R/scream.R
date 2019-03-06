#' \if{html}{\Sexpr[stage=render,results=rd]{"\U0001f631"}} Scream.
#'
#' @description
#'
#' `scream()` ensures that the structure of `data` is the same as
#' prototype, `ptype`. Under the hood, [vctrs::vec_cast()] is used, which
#' casts each column of `data` to the same type as the corresponding
#' column in `ptype`.
#'
#' This casting enforces a number of important structural checks,
#' including but not limited to:
#'
#' - _Data Classes_ - Checks that the class of each column in `data` is the
#' same as the corresponding column in `ptype`.
#'
#' - _Novel Levels_ - Checks that the factor columns in `data` don't have any
#' _new_ levels when compared with the `ptype` columns. If there are new
#' levels, a warning is issued and they are coerced to `NA`.
#'
#' - _Level Recovery_ - Checks that the factor columns in `data` aren't
#' missing any factor levels when compared with the `ptype` columns. If
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
#' @param ptype A data frame prototype to cast `data` to. This is commonly
#' a 0-row slice of the training set.
#'
#' @return
#'
#' A tibble containing the required columns after any required structural
#' modifications have been made.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # shrink() / scream()
#'
#' # mold() is run at model fit time
#' # and a formula preprocessing engine is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Inside the result of mold() are the prototype tibbles
#' # for the predictors and the outcomes
#' ptype_pred <- x$engine$info$predictors
#' ptype_out <- x$engine$info$outcomes
#'
#' # Pass the test data, along with a prototype, to
#' # shrink() to extract the prototype columns
#' test_shrunk <- shrink(test, ptype_pred)
#'
#' # Now pass that to scream() to perform validation checks
#' # If no warnings / errors are thrown, the checks were
#' # successful!
#' scream(test_shrunk, ptype_pred)
#'
#' # ---------------------------------------------------------------------------
#' # Outcomes
#'
#' # To also extract the outcomes, use the outcome prototype
#' test_outcome <- shrink(test, ptype_out)
#' scream(test_outcome, ptype_out)
#'
#' # ---------------------------------------------------------------------------
#' # Casting
#'
#' # scream() uses vctrs::vec_cast() to intelligently convert
#' # new data to the prototype automatically. This means
#' # it can automatically perform certain conversions, like
#' # coercing character columns to factors.
#' test2 <- test
#' test2$Species <- as.character(test2$Species)
#'
#' test2_shrunk <- shrink(test2, ptype_pred)
#' scream(test2_shrunk, ptype_pred)
#'
#' # It can also recover missing factor levels.
#' # For example, it is plausible that the test data only had the
#' # "virginica" level
#' test3 <- test
#' test3$Species <- factor(test3$Species, levels = "virginica")
#'
#' test3_shrunk <- shrink(test3, ptype_pred)
#' test3_fixed <- scream(test3_shrunk, ptype_pred)
#'
#' # scream() recovered the missing levels
#' levels(test3_fixed$Species)
#'
#' @export
scream <- function(data, ptype) {

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
  # - errors in casting individual columns to the prototype
  #   - will silently do it if possible

  # NOT caring about too many columns / missing columns, as that was taken care of by shrink
  # and the vctrs behavior (adding NA columns) isn't great here

  # TODO waiting on https://github.com/r-lib/vctrs/issues/225 thoughts

  vctrs::vec_cast(data, ptype)
}
