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
#' @param predictors A character vector. The original predictors used when
#' fitting the model.
#'
#' @param outcomes A character vector. The original outcome columns used
#' when fitting the model.
#'
#' @param predictor_levels A named list. Each element of the list should be a
#' character vector of levels. The names of the list should be the _predictor_
#' factor columns in the training data. If there are no factor columns, this is
#' `NULL`.
#'
#' @param outcome_levels A named list. Each element of the list should be a
#' character vector of levels. The names of the list should be the _outcome_
#' factor columns in the training data. If there are no factor columns, this is
#' `NULL`.
#'
#' @param predictor_classes A named list. Each element of the list should
#' be a character vector of classes. The names of the list are the _predictor_
#' names in the training data.
#'
#' @param outcome_classes A named list. Each element of the list should
#' be a character vector of classes. The names of the list are the _outcome_
#' names in the training data.
#'
#' @param ... Name-value pairs of extra elements that should be added to
#' subclassed preprocessors.
#'
#' @param subclass An optional character. The subclass of the `"preprocessor"`.
#'
#' @keywords internal
new_preprocessor <- function(engine = new_default_preprocessor_engine(),
                             intercept = FALSE,
                             type = "tibble",
                             predictors = character(),
                             outcomes = character(),
                             predictor_levels = NULL,
                             outcome_levels = NULL,
                             predictor_classes = NULL,
                             outcome_classes = NULL,
                             ...,
                             subclass = character()) {

  validate_is_preprocessor_engine(engine)
  validate_intercept(intercept)
  validate_type(type)
  validate_is_character(predictors, "predictors")
  validate_is_character(outcomes, "outcomes")
  validate_levels_list(predictor_levels, "predictor_levels")
  validate_levels_list(outcome_levels, "outcome_levels")

  elems <- list(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    predictor_levels = predictor_levels,
    outcome_levels = outcome_levels,
    predictor_classes = predictor_classes,
    outcome_classes = outcome_classes
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
                                     outcomes = character(),
                                     predictor_levels = NULL,
                                     outcome_levels = NULL,
                                     predictor_classes = NULL,
                                     outcome_classes = NULL) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    predictor_levels = predictor_levels,
    outcome_levels = outcome_levels,
    predictor_classes = predictor_classes,
    outcome_classes = outcome_classes,
    subclass = "default_preprocessor"
  )

}

new_terms_preprocessor <- function(engine,
                                   intercept = FALSE,
                                   type = "tibble",
                                   predictors = character(),
                                   outcomes = character(),
                                   predictor_levels = NULL,
                                   outcome_levels = NULL,
                                   predictor_classes = NULL,
                                   outcome_classes = NULL) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    predictor_levels = predictor_levels,
    outcome_levels = outcome_levels,
    predictor_classes = predictor_classes,
    outcome_classes = outcome_classes,
    subclass = "terms_preprocessor"
  )

}

new_recipes_preprocessor <- function(engine,
                                     intercept = FALSE,
                                     type = "tibble",
                                     predictors = character(),
                                     outcomes = character(),
                                     predictor_levels = NULL,
                                     outcome_levels = NULL,
                                     predictor_classes = NULL,
                                     outcome_classes = NULL) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    type = type,
    predictors = predictors,
    outcomes = outcomes,
    predictor_levels = predictor_levels,
    outcome_levels = outcome_levels,
    predictor_classes = predictor_classes,
    outcome_classes = outcome_classes,
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
#' x <- prepare(Species ~ Sepal.Width, iris)
#' is_preprocessor(x$preprocessor)
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
#' @keywords internal
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
#' @keywords internal
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

validate_levels_list <- function(lst, lst_nm) {

  valid_levels_obj <- function(x) {

    if (is.null(x)) {
      return(TRUE)
    }

    if (!is.list(x)) {
      return(FALSE)
    }

    ok <- vapply(x, rlang::is_character, logical(1))

    all(ok)
  }

  validate_has_unique_names(lst, lst_nm)

  if (!valid_levels_obj(lst)) {
    glubort("`{lst_nm}` must be a list of character vectors, or `NULL`.")
  }

  invisible(lst)
}
