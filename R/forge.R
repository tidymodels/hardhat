#' Forge prediction-ready data
#'
#' @description
#'
#' `forge()` applies the transformations requested by the specific `blueprint`
#' on a set of `new_data`. This `new_data` contains new predictors
#' (and potentially outcomes) that will be used to generate predictions.
#'
#' All blueprints have consistent return values with the others, but each is
#' unique enough to have its own help page. Click through below to learn
#' how to use each one in conjunction with `forge()`.
#'
#' * XY Method - [default_xy_blueprint()]
#'
#' * Formula Method - [default_formula_blueprint()]
#'
#' * Recipes Method - [default_recipe_blueprint()]
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
#' @param blueprint A preprocessing `blueprint`.
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
#'  - `extras`: Either `NULL` if the blueprint returns no extra information,
#'  or a named list containing the extra information.
#'
#' @examples
#' # See the blueprint specific documentation linked above
#' # for various ways to call forge with different
#' # blueprints.
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # Formula
#' processed <- mold(
#'   log(Sepal.Width) ~ Species,
#'   train,
#'   blueprint = default_formula_blueprint(indicators = FALSE)
#' )
#'
#' forge(test, processed$blueprint, outcomes = TRUE)
#'
#'
#' @export
forge <- function(new_data, blueprint, ..., outcomes = FALSE) {
  UseMethod("forge")
}

#' @export
forge.default <- function(new_data, blueprint, ..., outcomes = FALSE) {
  glubort("The class of `new_data`, '{class1(new_data)}', is not recognized.")
}

#' @export
forge.data.frame <- function(new_data, blueprint, ..., outcomes = FALSE) {

  validate_empty_dots(...)
  validate_is_blueprint(blueprint)

  c(blueprint, predictors, outcomes, extras) %<-% blueprint$forge$clean(
    blueprint = blueprint,
    new_data = new_data,
    outcomes = outcomes
  )

  c(predictors, outcomes, extras) %<-% blueprint$forge$process(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )

  out$forge$final(predictors, outcomes, extras)
}

#' @export
forge.matrix <- forge.data.frame
