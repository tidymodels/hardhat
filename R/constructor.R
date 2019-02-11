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
#' converts an object to a data frame or matrix, and adds an intercept column
#' if required.
#'
#' Additionally, all models have a `mode`, generally: `"classification"` or
#' `"regression"`, and a `variateness`: `"univariate"` or `"multivariate"`.
#' A single model can be both a classification and regression model (for
#' example, random forest), but at _fit time_ the `mode` of the model is set
#' for the lifetime of that model object.
#'
#' @param mode A character. Generally either `"classification"`
#' or `"regression"`.
#'
#' @param variateness A character. One of `"univariate"` or `"multivariate"`.
#'
#' @param preprocessor A preprocessor for new data. This can be
#' a `terms` object from the formula interface, a `recipe` created with
#' [recipes::recipe()], or the default which converts to either a matrix or a
#' data frame, and adds an intercept column if requested.
#'
#' @param ... Name-value pairs for elements specific to the model defined by
#' `class`.
#'
#' @param class A character vector representing the class of the model.
#'
#' @examples
#'
#' new_base_model(
#'   "regression",
#'   "univariate",
#'   default_preprocessor(),
#'   custom_field = "my-field",
#'   class = "custom_model"
#' )
#'
#' @export
new_base_model <- function(mode, variateness, preprocessor = NULL, ..., class) {

  if (is.null(preprocessor)) {
    preprocessor <- default_preprocessor()
  }

  validate_is_preprocessor(preprocessor)
  validate_is_variateness_like(variateness)
  validate_is_mode_like(mode)

  new_fields <- rlang::list2(...)
  validate_has_unique_names(new_fields, "...")

  fields <- list(
    mode = mode,
    variateness = variateness,
    preprocessor = preprocessor
  )

  fields <- c(fields, new_fields)

  if (is_missing(class)) {
    abort("A model `class` must be provided.")
  }

  structure(fields, class = class)

}

# ------------------------------------------------------------------------------

#' Is `x` a valid preprocessor?
#'
#' This function checks to see if `x` is a valid preprocessor.
#'
#' @param x A `"recipe"`, `"terms"` or `"default_preprocessor"` object.
#'
#' @examples
#'
#' is_preprocessor(default_preprocessor())
#'
#' @export
is_preprocessor <- function(x) {
  inherits(x, c("recipe", "terms", "default_preprocessor"))
}

# ------------------------------------------------------------------------------

validate_is_preprocessor <- function(preprocessor) {
  validate_is(
    preprocessor,
    is_preprocessor,
    "preprocessor",
    .note = "recipe, terms, or default_preprocessor"
  )
}

validate_is_mode_like <- function(mode) {
  validate_is(mode, rlang::is_character, "character")
  validate_length(mode, .n = 1L)
}

validate_is_variateness_like <- function(variateness) {
  validate_is(variateness, rlang::is_character, "character")
  validate_length(variateness, .n = 1L)
}
