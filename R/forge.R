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
#' train <- iris[1:100, ]
#' test <- iris[101:150, ]
#'
#' # Formula
#' processed <- mold(
#'   log(Sepal.Width) ~ Species,
#'   train,
#'   blueprint = default_formula_blueprint(indicators = "none")
#' )
#'
#' forge(test, processed$blueprint, outcomes = TRUE)
#' @export
forge <- function(new_data, blueprint, ..., outcomes = FALSE) {
  UseMethod("forge")
}

#' @export
forge.default <- function(new_data, blueprint, ..., outcomes = FALSE) {
  cli::cli_abort("No {.fn forge} method provided for {.obj_type_friendly {new_data}} object.")
}

#' @export
forge.data.frame <- function(new_data, blueprint, ..., outcomes = FALSE) {
  check_dots_empty0(...)
  check_blueprint(blueprint)

  run_forge(
    blueprint,
    new_data = new_data,
    outcomes = outcomes
  )
}

#' @export
forge.matrix <- forge.data.frame

# ------------------------------------------------------------------------------

#' `forge()` according to a blueprint
#'
#' @description
#' This is a developer facing function that is _only_ used if you are creating
#' your own blueprint subclass. It is called from [forge()] and dispatches off
#' the S3 class of the `blueprint`. This gives you an opportunity to forge the
#' new data in a way that is specific to your blueprint.
#'
#' `run_forge()` is always called from `forge()` with the same arguments, unlike
#' [run_mold()], because there aren't different interfaces for calling
#' `forge()`. `run_forge()` is always called as:
#'
#' `run_forge(blueprint, new_data = new_data, outcomes = outcomes)`
#'
#' If you write a blueprint subclass for [new_xy_blueprint()],
#' [new_recipe_blueprint()], [new_formula_blueprint()], or [new_blueprint()],
#' then your `run_forge()` method signature must match this.
#'
#' @inheritParams forge
#'
#' @return
#' `run_forge()` methods return the object that is then immediately returned
#' from `forge()`. See the return value section of [forge()] to understand what
#' the structure of the return value should look like.
#'
#' @name run-forge
#' @order 1
#' @export
#' @examples
#' bp <- default_xy_blueprint()
#'
#' outcomes <- mtcars["mpg"]
#' predictors <- mtcars
#' predictors$mpg <- NULL
#'
#' mold <- run_mold(bp, x = predictors, y = outcomes)
#'
#' run_forge(mold$blueprint, new_data = predictors)
run_forge <- function(blueprint,
                      new_data,
                      ...,
                      outcomes = FALSE) {
  UseMethod("run_forge")
}

#' @export
run_forge.default <- function(blueprint,
                              new_data,
                              ...,
                              outcomes = FALSE) {
  cli::cli_abort("No {.fn run_forge} method provided for {.obj_type_friendly {blueprint}} object.")
}
