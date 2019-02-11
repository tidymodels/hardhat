#' Constructor for a base model
#' 
#' A __model__ is a _scalar object_, as classified in 
#' [Advanced R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it
#' takes uniquely named fields in `...` and combines them into a list with
#' a class of `class`. This entire object represent a single model.
#' 
#' Because every model should have multiple interfaces, including formula
#' and `recipes` interfaces, all models should have a `preprocessor` that 
#' can process new data when `predict()` is called. The default `preprocessor`
#' used in the data frame and matrix methods is `default_preprocessor()`, which 
#' converts an object to a matrix and adds an intercept column if required.
#' 
#' Additionally, all models have a `mode`, generally: `"classification"` or
#' `"regression"`, and a `variateness`: `"univariate"` or `"multivariate"`.
#' A single model can be both a classification and regression model (for 
#' example, random forest), but at _fit time_ the `mode` of the model is set
#' for the lifetime of that model object. 
#' 
#' @param mode A character. Generally either `"classification"` 
#' or `"regression"`.
#' @param variateness A character. One of `"univariate"` or `"multivariate"`.
#' @param preprocessor A preprocessor for new data. This can be 
#' a `terms` object from the formula interface, a `recipe` created with
#' [recipes::recipe()], or the default which converts to matrix and adds
#' an intercept column if requested.
#' @param ... Named fields of subclasses of the model.
#' @param class A character vector representing the class of the model.
#' 
#' @export
new_base_model <- function(mode, variateness, preprocessor = NULL, ..., class) {
  
  # Check that the provided preprocessor is valid
  stopifnot(is_preprocessor(preprocessor))
  
  # Check types and lengths
  stopifnot(is.character(mode) && length(mode) == 1)
  stopifnot(is.character(variateness) && length(variateness) == 1)
  
  fields <- list(
    ..., 
    mode = mode, 
    variateness = variateness, 
    preprocessor = preprocessor
  )
  
  # All fields must be named
  validate_named(fields, "...")
  
  if (is_missing(class)) {
    abort("A model `class` must be provided.")
  }
  
  structure(fields, class = class)
  
}

is_preprocessor <- function(x) {
  inherits(x, c("NULL", "recipe", "terms", "default_preprocessor"))
}
