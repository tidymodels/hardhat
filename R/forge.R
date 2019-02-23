#' Forge prediction-ready data
#'
#' @description
#'
#' `forge()` applies the transformations requested by the `preprocessor`
#' on a set of `new_data` to be used in predictions.
#'
#' The return values of each method are all consistent with one another, but the
#' nuances of exactly what is being done for each method vary enough to warrant
#' separate help files for each. Click through to each one below:
#'
#' * XY Method - [forge.default_preprocessor()]
#'
#' * Formula Method - [forge.terms_preprocessor()]
#'
#' * Recipes Method - [forge.recipes_preprocessor()]
#'
#' @details
#'
#' If the outcomes are present in `new_data`, they can optionally be processed
#' and returned in the `outcomes` slot of the returned list. This is very
#' useful when doing cross validation where you need to preprocess the
#' outcomes of a test set before computing performance.
#'
#' @param preprocessor A valid `"preprocessor"`. The preprocessor that should
#' be used here is the one in the output from the corresponding call
#' to [mold()].
#'
#' @param new_data A data frame or matrix to preprocess.
#'
#' @param outcomes A logical. Should the outcomes be processed and returned
#' as well?
#'
#' @param ... Not currently used.
#'
#' @return
#'
#' A named list with 3 elements:
#'
#'  - `predictors`: A tibble containing the preprocessed
#'  `new_data` predictors.
#'
#'  - `outcomes`: If `outcomes = TRUE`, a tibble containing the preprocessed
#'  `new_data` outcomes. Otherwise, `NULL`.
#'
#'  - `offset`: If the `preprocessor` was a `"terms_preprocessor"`, and offsets
#'  were specified in the formula, this is a tibble containing the preprocessed
#'  offsets. Otherwise, `NULL`.
#'
#' @export
forge <- function(new_data, engine, outcomes = FALSE, ...) {
  UseMethod("forge")
}

#' @export
forge.default <- function(new_data, engine, outcomes = FALSE, ...) {
  glubort("The class of `new_data`, '{class1(new_data)}', is not recognized.")
}

#' @export
forge.data.frame <- function(new_data, engine, outcomes = FALSE, ...) {

  validate_is_engine(engine)

  engine <- update_engine(engine, ...)

  forge_impl(engine, new_data, outcomes)

}

#' @export
forge.matrix <- forge.data.frame

# ------------------------------------------------------------------------------

# We make this generic because mold_impl() has to be generic to support the
# different ways data can come in (`x` and `y` or `data`) and we want to be
# consistent with that.

# But, for now, they all do the same thing (as they should!),
# and we don't expose this to the user/developer.

forge_impl <- function(engine, ...) {
  UseMethod("forge_impl")
}

forge_impl.xy_engine <- function(engine, new_data, outcomes) {

  c(engine, new_data) %<-% engine$forge$clean(
    engine = engine,
    new_data = new_data,
    outcomes = outcomes
  )

  c(engine, predictors, outcomes) %<-% engine$forge$process(
    engine = engine,
    new_data = new_data,
    outcomes = outcomes
  )

  forge_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    offset = predictors$offset
  )

}

forge_impl.formula_engine <- forge_impl.xy_engine

forge_impl.recipe_engine <- forge_impl.xy_engine

# ------------------------------------------------------------------------------

# Would it also be useful to add an `extras` element here?
# So specific implementations can return other processed
# information as needed?

forge_list <- function(predictors, outcomes = NULL, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    offset = offset
  )
}
