#' Predict from a `{{model}}`
#'
#' @param object A `{{model}}` object.
#'
#' @param new_data A data frame of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"response"` for numeric predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- {{model}}(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.{{model}} <- function(object, new_data, type = "response", ...) {
  processed <- hardhat::forge(new_data, object$blueprint)
  type <- rlang::arg_match(type, valid_predict_types())
  predict_{{model}}_bridge(type, object, processed$predictors)
}

valid_predict_types <- function() {
  c("response")
}
