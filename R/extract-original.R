#' Extract the original predictors from a model object
#'
#' Often, when `predict()`-ing on `new_data`, it is a requirement that the
#' columns in `new_data` match the columns in the original data used to fit
#' the model. `extract_original_predictors()` provides a way to extract the
#' names of those required predictors.
#'
#' @param preprocessor A `"preprocessor"` object.
#'
#' @details
#'
#' This function knows how to extract 3 different types of predictors, depending
#' on the type of preprocessing:
#'
#' 1) Default preprocessing - The original predictors are the same as the
#' column names stored in `preprocessor$predictors`.
#'
#' 2) Formula preprocessing - The original predictors are stored in the
#' modified `terms` attribute `"predictors"`. If `prepare()` was used on
#' the input data, then this attribute is constructed for you.
#'
#' 3) Recipe preprocessing - The original predictors are stored in the
#' `variable` column of `recipe$var_info`, after filtering down to
#' `type == "predictor"`.
#'
#' This function is called from [shrink()].
#'
#' @export
extract_original_predictors <- function(preprocessor) {
  UseMethod("extract_original_predictors")
}

#' @export
extract_original_predictors.default_preprocessor <- function(preprocessor) {
  preprocessor$predictors
}

#' @export
extract_original_predictors.terms_preprocessor <- function(preprocessor) {
  attr(preprocessor$engine, "predictors")
}

#' @export
extract_original_predictors.recipe_preprocessor <- function(preprocessor) {

  engine <- preprocessor$engine

  has_predictor_role <- engine$var_info$role == "predictor"
  predictors <- engine$var_info$variable[has_predictor_role]

  # in case a predictor has multiple types
  predictors <- unique(predictors)

  predictors
}

# ------------------------------------------------------------------------------

#' Extract the original outcomes from a model object
#'
#' Often, when `predict()`-ing on `new_data`, it is a requirement that the
#' columns in `new_data` match the columns in the original data used to fit
#' the model. `extract_original_outcomes()` provides a way to extract the
#' names of those required outcomes.
#'
#' @param preprocessor A `"preprocessor"` object.
#'
#' @details
#'
#' This function knows how to extract 3 different types of outcomes, depending
#' on the type of preprocessing:
#'
#' 1) Default preprocessing - The original outcomes are the same as the
#' column names stored in `preprocessor$outcomes`.
#'
#' 2) Formula preprocessing - The original outcomes are stored in the
#' modified `terms` attribute `"outcomes"`. If `prepare()` was used on
#' the input data, then this attribute is constructed for you.
#'
#' 3) Recipe preprocessing - The original outcomes are stored in the
#' `variable` column of `recipe$var_info`, after filtering down to
#' `type == "outcome"`.
#'
#' This function is called from [preprocess()] when `outcome = TRUE`.
#'
#' @export
extract_original_outcomes <- function(preprocessor) {
  UseMethod("extract_original_outcomes")
}

#' @export
extract_original_outcomes.default_preprocessor <- function(preprocessor) {
  preprocessor$outcomes
}

#' @export
extract_original_outcomes.terms_preprocessor <- function(preprocessor) {
  attr(preprocessor$engine, "outcomes")
}

#' @export
extract_original_outcomes.recipe_preprocessor <- function(preprocessor) {

  engine <- preprocessor$engine

  has_predictor_role <- engine$var_info$role == "outcome"
  outcomes <- engine$var_info$variable[has_predictor_role]

  # in case a outcome has multiple types
  outcomes <- unique(outcomes)

  outcomes
}
