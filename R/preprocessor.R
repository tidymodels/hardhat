#' Create a new preprocessor
#'
#' A preprocessor holds a preprocessing engine, and various elements that are
#' useful for performing structural validation of `new_data` in `forge()` when
#' it is time to make predictions using a model.
#'
#' @param engine A preprocessing engine. This can be a
#' `"default_preprocessor_engine"`, a `"recipe"`, or a `"terms"` object.
#'
#' @param intercept A logical. Should an intercept be included?
#'
#' @param predictors A named list with the following 3 elements:
#'
#' - `"names"`: A character vector of the original predictor column names.
#'
#' - `"classes"`: A named list of the original predictor classes, or `NULL`.
#'
#' - `"levels"`: A named list of the original predictor levels for any factor
#' columns, or `NULL`.
#'
#' @param outcomes A named list with the following 3 elements:
#'
#' - `"names"`: A character vector of the original outcome column names.
#'
#' - `"classes"`: A named list of the original outcome classes, or `NULL`.
#'
#' - `"levels"`: A named list of the original outcome levels for any factor
#' columns, or `NULL`.
#'
#' @param ... Name-value pairs of extra elements that should be added to
#' subclassed preprocessors.
#'
#' @param subclass An optional character. The subclass of the `"preprocessor"`.
#'
#' @keywords internal
new_preprocessor <- function(engine = new_default_preprocessor_engine(),
                             intercept = FALSE,
                             info = info_lst(),
                             ...,
                             subclass = character()) {

  validate_is_preprocessor_engine(engine)
  validate_intercept(intercept)
  validate_is_info_list(info, "info")

  elems <- list(
    engine = engine,
    intercept = intercept,
    info = info
  )

  new_elems <- rlang::list2(...)
  validate_has_unique_names(new_elems, "...")

  elems <- c(elems, new_elems)

  structure(elems, class = c(subclass, "preprocessor"))

}

new_default_preprocessor <- function(engine = new_default_preprocessor_engine(),
                                     intercept = FALSE,
                                     info = info_lst()) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    info = info,
    subclass = "default_preprocessor"
  )

}

new_terms_preprocessor <- function(engine,
                                   intercept = FALSE,
                                   info = info_lst(),
                                   indicators = TRUE) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    info = info,
    indicators = indicators,
    subclass = "terms_preprocessor"
  )

}

new_recipes_preprocessor <- function(engine,
                                     intercept = FALSE,
                                     info = info_lst()) {

  new_preprocessor(
    engine = engine,
    intercept = intercept,
    info = info,
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
#' x <- mold(Species ~ Sepal.Width, iris)
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
#' preprocessing on `new_data` to mold it for ingestion into a model. To
#' control how the engine preprocesses `new_data` the `intercept` argument can
#' be set, which is passed down to the created engine function.
#' A default preprocessor engine is used in matrix and data frame methods of a
#' model (as opposed to a recipe or formula method which performs
#' preprocessing for you). This preprocessing is performed on both the training
#' and testing data sets.
#'
#' The preprocessor function returned from `new_default_preprocessor_engine()`
#' will only do one thing:
#'
#' - Call `add_intercept_column()` if required to add an intercept to
#' `new_data`.
#'
#' The returned function that `new_default_preprocessor_engine()` creates
#' has 2 arguments:
#'
#' - `new_data`: The data to preprocess.
#'
#' - `intercept`: A logical. Determines whether or not an
#' intercept will be added.
#'
#' @keywords internal
new_default_preprocessor_engine <- function() {

  process <- function(new_data, intercept) {
    new_data <- tibble::as_tibble(new_data)
    new_data <- maybe_add_intercept_column(new_data, intercept)
    new_data
  }

  structure(list(process = process), class = "default_preprocessor_engine")
}

#' Create a terms preprocessor engine
#'
#' A terms preprocessor engine is composed of two terms objects. One for the
#' RHS of the original formula, which is used to preprocess the predictors.
#' And one for the LHS of the original formula, reformulated as `~ LHS`.
#'
#' This is done so that multivariate outcomes can be specified as
#' `Sepal.Width + Sepal.Length ~ Species` and can be processed by
#' `model.frame()` in a meaningful way.
#'
#' @param predictors A terms object for the predictors. The formula should be
#' for `~ RHS`.
#' @param outcomes A terms object for the outcomes. The formula should be for
#' `~ LHS`.
#'
#' @keywords internal
new_terms_preprocessor_engine <- function(predictors, outcomes) {

  if (!inherits(predictors, "terms")) {
    abort("`predictors` must be a 'terms' object for the RHS of the formula.")
  }

  if (!inherits(outcomes, "terms")) {
    abort("`outcomes` must be a 'terms' object for the LHS of the formula.")
  }

  elems <- list(
    predictors = predictors,
    outcomes = outcomes
  )

  structure(elems, class = "terms_preprocessor_engine")
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
  inherits(
    x,
    c("recipe", "terms_preprocessor_engine", "default_preprocessor_engine")
  )
}

# ------------------------------------------------------------------------------

info_lst <- function(predictors = predictors_info(),
                     outcomes = outcomes_info()) {

  list(
    predictors = predictors,
    outcomes = outcomes
  )

}

predictors_info <- function(names = character(),
                            classes = NULL,
                            levels = NULL) {
  list(
    names = names,
    classes = classes,
    levels = levels
  )
}

outcomes_info <- function(names = character(),
                          classes = NULL,
                          levels = NULL) {
  list(
    names = names,
    classes = classes,
    levels = levels
  )
}

# ------------------------------------------------------------------------------

validate_is_preprocessor_engine <- function(engine) {
  validate_is(
    engine,
    is_preprocessor_engine,
    "preprocessor engine",
    .note = "recipe, terms_preprocessor_engine, or default_preprocessor_engine"
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

validate_is_info_list <- function(.x, .x_nm) {

  validate_has_name(.x, .x_nm, "predictors")
  validate_has_name(.x, .x_nm, "outcomes")

  validate_is_terms_info_list(.x$predictors, glue("{.x_nm}$predictors"))
  validate_is_terms_info_list(.x$outcomes, glue("{.x_nm}$outcomes"))

  invisible(.x)
}

validate_is_terms_info_list <- function(.x, .x_nm) {

  validate_has_name(.x, .x_nm, "names")
  validate_has_name(.x, .x_nm, "classes")
  validate_has_name(.x, .x_nm, "levels")

  validate_is_character(.x$names, glue("{.x_nm}$names"))
  validate_classes_list(.x$classes, glue("{.x_nm}$classes"))
  validate_levels_list(.x$levels, glue("{.x_nm}$levels"))

  invisible(.x)
}

validate_has_name <- function(.x, .x_nm, .nm) {
  if (!tibble::has_name(.x, .nm)) {
    glubort("`{.x_nm}` must have an element named '{.nm}'.")
  }
  invisible(.x)
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

# Just happen to be the same structure
validate_classes_list <- validate_levels_list
