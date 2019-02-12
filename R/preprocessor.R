#' Create a new preprocessor
#'
#' A preprocessor holds a preprocessing engine, an indicator for whether or
#' not to include the intercept, and a character string representing the
#' output type of the predictors.
#'
#' @param engine A preprocessing engine. This can be a
#' `"default_preprocessor_engine"`, a `"recipe"`, or a `"terms"` object.
#'
#' @param intercept A logical. Should an intercept be included?
#'
#' @param type A character. One of `"tibble"`, `"data.frame"` or `"matrix"`.
#' The output type for the predictors.
#'
#' @param subclass An optional character. The subclass of the `"preprocessor"`.
#'
#' @export
new_preprocessor <- function(engine = new_default_preprocessor_engine(),
                             intercept = FALSE,
                             type = "tibble",
                             predictors = character(),
                             outcomes = character(),
                             ...,
                             subclass = character()) {

  validate_is_preprocessor_engine(engine)
  validate_intercept(intercept)
  validate_type(type)
  validate_is_character(predictors, "predictors")
  validate_is_character(outcomes, "outcomes")

  elems <- list(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes
  )

  new_elems <- rlang::list2(...)
  validate_has_unique_names(new_elems, "...")

  elems <- c(elems, new_elems)

  structure(elems, class = c(subclass, "preprocessor"))

}

new_default_preprocessor <- function(engine = new_default_preprocessor_engine(),
                                     intercept = FALSE,
                                     type = "tibble",
                                     predictors = character(),
                                     outcomes = character()) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    subclass = "default_preprocessor"
  )

}

new_terms_preprocessor <- function(engine,
                                   intercept = FALSE,
                                   type = "tibble",
                                   predictors = character(),
                                   outcomes = character(),
                                   predictor_levels = list()) {

  validate_predictor_levels(predictor_levels)

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    predictor_levels = predictor_levels,
    subclass = "terms_preprocessor"
  )

}

new_recipes_preprocessor <- function(engine,
                                     intercept = FALSE,
                                     type = "tibble",
                                     predictors = character(),
                                     outcomes = character()) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    subclass = "recipes_preprocessor"
  )

}

# ------------------------------------------------------------------------------

#' Is `x` a valid preprocessor?
#'
#' This function checks to see if `x` is a valid `"preprocessor"`.
#'
#' @param x An object.
#'
#' @examples
#'
#' is_preprocessor(new_preprocessor())
#'
#' @export
is_preprocessor <- function(x) {
  inherits(x, "preprocessor")
}

# ------------------------------------------------------------------------------

#' Create a default preprocessor engine
#'
#' A default preprocessor engine is a function that performs some basic
#' preprocessing on `new_data` to prepare it for ingestion into a model. To
#' control how the engine preprocesses `new_data` the `intercept` and `type`
#' arguments can be set, which are passed down to the created engine function.
#' A default preprocessor engine is used in matrix and data frame methods of a
#' model (as opposed to a recipe or formula method which performs
#' preprocessing for you). This preprocessing is performed on both the training
#' and testing data sets.
#'
#' The preprocessor function returned from `new_default_preprocessor_engine()`
#' will do two things:
#'
#' - Call [retype()] with `type` to coerce `new_data` to a specific type.
#'
#' - Call `add_intercept_column()` with `intercept` to add an intercept to
#' `new_data` if required.
#'
#' The returned function that `new_default_preprocessor_engine()` creates
#' has 3 arguments:
#'
#' - `new_data`: The data to preprocess.
#'
#' - `intercept`: A logical. Passed on to `add_intercept_column()`.
#'
#' - `type`: A single character. Passed on to `retype()`.
#'
#'
#' @export
new_default_preprocessor_engine <- function() {

  process <- function(new_data, intercept, type) {
    new_data <- retype(new_data, type)
    new_data <- add_intercept_column(new_data, intercept)
    new_data
  }

  structure(list(process = process), class = "default_preprocessor_engine")
}

# ------------------------------------------------------------------------------

#' Is `x` a valid preprocessor engine?
#'
#' This function checks to see if `x` is a valid preprocessing engine. This
#' can be a `recipe`, a `terms` object, or a `default_preprocessor_engine`.
#'
#' @param x An object.
#'
#' @examples
#'
#' is_preprocessor_engine(new_default_preprocessor_engine())
#'
#' @export
is_preprocessor_engine <- function(x) {
  inherits(x, c("recipe", "terms", "default_preprocessor_engine"))
}

# ------------------------------------------------------------------------------

validate_is_preprocessor_engine <- function(engine) {
  validate_is(
    engine,
    is_preprocessor_engine,
    "preprocessor engine",
    .note = "recipe, terms, or default_preprocessor_engine"
  )
  invisible(engine)
}

validate_is_preprocessor <- function(preprocessor) {
  validate_is(
    preprocessor,
    is_preprocessor,
    "preprocessor"
  )
  invisible(preprocessor)
}

validate_is_character <- function(.x, .x_nm) {
  validate_is(
    .x,
    rlang::is_character,
    "character",
    .x_nm
  )
}

validate_predictor_levels <- function(predictor_levels) {

  valid_pred_levels_obj <- function(predictor_levels) {

    if (!is.list(predictor_levels)) {
      return(FALSE)
    }

    is_character_or_null <- function(x) {
      rlang::is_character(x) || is.null(x)
    }

    ok <- vapply(predictor_levels, is_character_or_null, logical(1))

    all(ok)
  }

  validate_has_unique_names(predictor_levels, "predictor_levels")

  if (!valid_pred_levels_obj(predictor_levels)) {
    glubort("`predictor_level` must be a list of character vectors, or `NULL`.")
  }

  invisible(predictor_levels)
}
