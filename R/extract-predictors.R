#' Extract the original predictors from a model object
#' 
#' Often, when `predict()`-ing on `new_data`, it is a requirement that the 
#' columns in `new_data` match the columns in the original data used to fit
#' the model. `extract_original_predictors()` provides a way to extract the
#' names of those required predictors.
#' 
#' @param object A model object.
#' 
#' @details 
#' 
#' This function knows how to extract 3 different types of predictors, depending
#' on the type of preprocessing:
#' 
#' 1) Default preprocessing - The original predictors are the same as the 
#' column names stored in `object$predictors` (which generally holds post
#' processed column names) minus the `"(Intercept)"` column if it exists. 
#' 
#' 2) Formula preprocessing - The original predictors are stored in the
#' modified `terms` attribute `"x_predictors"`. If `prepare()` was used on
#' the input data, then this attribute is constructed for you.
#' 
#' 3) Recipe preprocessing - The original predictors are stored in the
#' `variable` column of `recipe$var_info`, after filtering down to 
#' `type == "predictor"`.
#' 
#' This function is called from [shrink()].
#' 
#' @keywords internal
extract_original_predictors <- function(object) {
  extract_original_predictors_impl(object$preprocessor, object)
}

extract_original_predictors_impl <- function(preprocessor, object) {
  UseMethod("extract_original_predictors_impl")
}

extract_original_predictors_impl.default_preprocessor <- function(preprocessor, object) {
  # Never specify the intercept as an "original predictor"
  # TODO: This assumes that a $predictors element exists.
  predictors <- setdiff(object$predictors, "(Intercept)")
  predictors
}

extract_original_predictors_impl.terms <- function(preprocessor, object) {
  attr(preprocessor, "x_predictors")
}

extract_original_predictors_impl.recipe <- function(preprocessor, object) {
  has_predictor_role <- preprocessor$var_info$role == "predictor"
  predictors <- preprocessor$var_info$variable[has_predictor_role]
  
  # in case a predictor has multiple types
  predictors <- unique(predictors)
  
  predictors
}