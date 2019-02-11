#' Prepare data for modeling
#'
#' @description
#'
#' `prepare()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' * For a formula method, this applies `model.frame()` and `model.matrix()`.
#'
#' * For a recipe, this performs a call to both [recipes::prep()]
#' and [recipes::juice()].
#'
#' * For a data frame or matrix, this uses the [default_preprocessor()] which
#' converts the input to `type` and adds an intercept column if requested.
#'
#' @param x A data frame, matrix, or [recipes::recipe()]. If this is a
#' data.frame or matrix, it should contain the predictors.
#'
#' @param formula A formula specifying the terms in the format of
#' `outcome ~ predictors`.
#'
#' @param y A data frame, matrix, or vector containing the outcome(s).
#'
#' @param intercept A single logical specifying whether or not to
#' include an intercept in the prepared predictors.
#'
#' @param type A single character. One of `"tibble"`, `"data.frame"`, or
#' `"matrix"` specifying the result type of the predictors.
#'
#' @param data A data frame to prepare.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing:
#'
#'  - `predictors`: An object of class `type` containing the prepared predictors
#'  to be used in the model.
#'
#'  - `outcome`: If `y` was supplied, it is returned unmodified here. If a
#'  formula was used, this is the result of [model.response()]. If a recipe
#'  was used, this is a data.frame that is the result of calling
#'  [recipes::juice()] with [recipes::all_outcomes()] specified.
#'
#'  - `preprocessor`: One of: a `default_preprocessor`, a prepped `recipe`,
#'  or a modified `terms` object.
#'
#' @examples
#'
#'
#'
#' @export
prepare <- function(x, ...) {
  UseMethod("prepare")
}

#' @rdname prepare
#' @export
prepare.data.frame <- function(x, y, intercept = FALSE,
                               type = "tibble", ...) {

  preprocessor <- default_preprocessor()
  x <- preprocessor$process(x, intercept, type)

  prepare_list(x, y, preprocessor)
}

#' @rdname prepare
#' @export
prepare.matrix <- function(x, y, intercept = FALSE,
                           type = "tibble", ...) {

  preprocessor <- default_preprocessor()
  x <- preprocessor$process(x, intercept, type)

  prepare_list(x, y, preprocessor)
}

#' @rdname prepare
#' @export
prepare.formula <- function(formula, data, intercept = FALSE,
                            type = "tibble", ...) {

  formula <- remove_formula_intercept(formula, intercept)

  framed <- rlang::with_options(
    model.frame(formula, data = data),
    na.action = "na.pass"
  )

  predictors <- rlang::with_options(
    model.matrix(formula, framed),
    na.action = "na.pass"
  )

  predictors <- retype(predictors, type)

  outcome <- model.response(framed)

  terms <- extract_terms(framed, data)

  prepare_list(predictors, outcome, terms)
}

#' @rdname prepare
#' @export
prepare.recipe <- function(x, data, intercept = FALSE,
                           type = "tibble", ...) {

  validate_recipes_available()

  prepped_recipe <- recipes::prep(x, training = data)

  # recipes bug?
  all_predictors <- recipes::all_predictors
  all_outcomes <- recipes::all_outcomes

  predictors <- recipes::juice(prepped_recipe, all_predictors())
  outcome <- recipes::juice(prepped_recipe, all_outcomes())

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  predictors <- retype(predictors, type)

  predictors <- add_intercept_column(predictors, intercept)

  prepare_list(predictors, outcome, prepped_recipe)
}

# ------------------------------------------------------------------------------

#' Create a default preprocessor
#'
#' A default preprocessor is a function that takes in `new_data`,
#' `intercept`, and `type`. It performs some basic preprocessing on `new_data`
#' to prepare it for ingestion into a model. A default preprocessor is used
#' in matrix and data frame methods of a model (as opposed to a recipe or
#' formula method which performs preprocessing for you).
#'
#' The preprocessor function returned from `default_preprocessor()` will do two
#' things:
#'
#' - Call [retype()] with `type` to coerce `new_data` to a specific type.
#'
#' - Call [add_intercept_column()] with `intercept` to add an intercept to
#' `new_data` if required.
#'
#' The returned function that `default_preprocessor()` creates has 3 arguments:
#'
#' - `new_data`: The data to preprocess.
#'
#' - `intercept`: A logical. Passed on to `add_intercept_column()`.
#'
#' - `type`: A single character. Passed on to `retype()`.
#'
#' @export
default_preprocessor <- function() {

  process <- function(new_data, intercept, type) {
    new_data <- retype(new_data, type)
    new_data <- add_intercept_column(new_data, intercept)
    new_data
  }

  structure(list(process = process), class = "default_preprocessor")
}

# ------------------------------------------------------------------------------
# Preparation helpers

prepare_list <- function(predictors, outcome, preprocessor) {
  list(
    predictors = predictors,
    outcome = outcome,
    preprocessor = preprocessor
  )
}
