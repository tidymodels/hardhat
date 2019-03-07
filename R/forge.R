#' Forge prediction-ready data
#'
#' @description
#'
#' `forge()` applies the transformations requested by the specific `engine`
#' on a set of `new_data`. This `new_data` contains new predictors
#' (and potentially outcomes) that will be used to generate predictions.
#'
#' All engines have consistent return values with the others, but each is
#' unique enough to have its own help page. Click through below to learn
#' how to use each one in conjunction with `forge()`.
#'
#' * XY Method - [default_xy_engine()]
#'
#' * Formula Method - [default_formula_engine()]
#'
#' * Recipes Method - [default_recipe_engine()]
#'
#' @details
#'
#' If the outcomes are present in `new_data`, they can optionally be processed
#' and returned in the `outcomes` slot of the returned list by setting
#' `outcomes = TRUE`. This is very useful when doing cross validation where
#' you need to preprocess the outcomes of a test set before computing
#' performance.
#'
#' @param new_data A data frame or matrix of predictors to process. If
#' `outcomes = TRUE`, this should also contain the outcomes to process.
#'
#' @param engine A preprocessing `engine`.
#'
#' @param outcomes A logical. Should the outcomes be processed and returned
#' as well?
#'
#' @param ... Not used.
#'
#' @return
#'
#' A named list with 3 elements:
#'
#'  - `predictors`: A tibble containing the preprocessed
#'  `new_data` predictors.
#'
#'  - `outcomes`: If `outcomes = TRUE`, a tibble containing the preprocessed
#'  outcomes found in `new_data`. Otherwise, `NULL`.
#'
#'  - `extras`: Either `NULL` if the engine returns no extra information,
#'  or a named list containing the extra information.
#'
#' @examples
#' # See the engine specific documentation linked above
#' # for various ways to call forge with different
#' # engines.
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # Formula
#' processed <- mold(
#'   log(Sepal.Width) ~ Species,
#'   train,
#'   engine = default_formula_engine(indicators = FALSE)
#' )
#'
#' forge(test, processed$engine, outcomes = TRUE)
#'
#'
#' @export
forge <- function(new_data, engine, ..., outcomes = FALSE) {
  UseMethod("forge")
}

#' @export
forge.default <- function(new_data, engine, ..., outcomes = FALSE) {
  glubort("The class of `new_data`, '{class1(new_data)}', is not recognized.")
}

#' @export
forge.data.frame <- function(new_data, engine, ..., outcomes = FALSE) {

  validate_empty_dots(...)
  validate_is_engine(engine)

  c(engine, predictors, outcomes) %<-% engine$forge$clean(
    engine = engine,
    new_data = new_data,
    outcomes = outcomes
  )

  c(predictors, outcomes, extras) %<-% engine$forge$process(
    engine = engine,
    predictors = predictors,
    outcomes = outcomes
  )

  out$forge$final(predictors, outcomes, extras)
}

#' @export
forge.matrix <- forge.data.frame
