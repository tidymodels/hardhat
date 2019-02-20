#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' The return values of each method are all consistent with one another, but the
#' nuances of exactly what is being done for each method vary enough to warrant
#' separate help files for each. Click through to each one below:
#'
#' * XY Method - [mold.data.frame()] / [mold.matrix()]
#'
#' * Formula Method - [mold.formula()]
#'
#' * Recipes Method - [mold.recipe()]
#'
#' @param x A data frame, matrix, formula, or [recipes::recipe()]. If this is a
#' data.frame or matrix, it should contain the predictors.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing 4 elements:
#'
#'  - `predictors`: A tibble containing the molded predictors to be used in the
#'  model.
#'
#'  - `outcome`: A tibble containing the molded outcomes to be used in the
#'  model.
#'
#'  - `preprocessor`: A `"preprocessor"` object for use when making predictions.
#'
#'  - `offset`: A tibble with a single column named `".offset"` if an offset
#'  was specified in the formula method. Otherwise, `NULL`.
#'
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  abort_unknown_mold_class(x)
}

# ------------------------------------------------------------------------------

mold_list <- function(predictors, outcomes, preprocessor, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    preprocessor = preprocessor,
    offset = offset
  )
}

check_is_data_like <- function(data) {

  if (!is_new_data_like(data)) {
    glubort(
      "`data` must be a data.frame or a matrix, not a {class1(data)}."
    )
  }

  tibble::as_tibble(data)
}
