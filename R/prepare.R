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
#' * For a data frame or matrix, this uses the `default_preprocessor()` which
#' converts the input to `type` and adds an intercept column if requested.
#' 
#' @param x A data frame, matrix, [recipes::recipe()], or formula.
#' @param y A data frame, matrix, or vector depending on `x`.
#' @param intercept A logical specifying whether or not to include an intercept.
#' @param type A single character. One of `"tibble"`, `"data.frame"`, or
#' `"matrix"` specifying the result type of the predictors.
#' @param data A data frame.
#' @param ... Currently unused.
#' 
#' @return 
#' 
#' A list containing the `predictors`, the `outcome`, and the `preprocessor`.
#' 
#' @export
prepare <- function(x, ...) {
  UseMethod("prepare")
}

#' @rdname prepare
#' @export
prepare.data.frame <- function(x, y, intercept = FALSE, 
                               type = "tibble", ...) {
  
  preprocessor <- default_preprocesser()
  x <- preprocessor$process(x, intercept, type)
  
  prepare_list(x, y, preprocessor)
}

#' @rdname prepare
#' @export
prepare.matrix <- function(x, y, intercept = FALSE, 
                           type = "tibble", ...) {

  preprocessor <- default_preprocesser()
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
  
  predictors <- retype(predictors, type)
  
  predictors <- add_intercept_column(predictors, intercept)
  
  prepare_list(predictors, outcome, prepped_recipe)
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

default_preprocesser <- function() {
  
  process <- function(new_data, intercept, type) {
    new_data <- retype(new_data, type)
    new_data <- add_intercept_column(new_data, intercept)
    new_data
  }
  
  structure(list(process = process), class = "default_preprocessor")
}