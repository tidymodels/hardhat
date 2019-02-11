#' Spruce up predictions
#'
#' `spruce()` converts predictions into a standardized format. It is generally
#' called from your prediction implementation function for the specific
#' `type` of prediction to return.
#'
#' @param type The return value from `dispatchify()` called on a character
#' `type`. This is used for appropriate dispatching.
#' @param new_data The post processed new data to make predictions for. This is
#' used to validate that the number of rows in the predictions is the same
#' as the number of rows in the original `new_data`.
#' @param .pred (`type = "response"`) A numeric vector of predictions.
#' @param .pred_class (`type = "class"`) A factor of "hard" class predictions.
#' @param .pred_levels,.prob_matrix (`type = "prob"`) `.pred_levels` should
#' be a character vector of the original levels of the outcome used in training.
#' `.prob_matrix` should be a numeric matrix of class probabilities with as many
#' columns as levels in `.pred_levels`, and in the same order.
#'
#' @return
#'
#' A tibble with as many rows as in `new_data`. The column names and number
#' of columns vary based on the `type`, but are standardized.
#'
#' @export
spruce <- function(type, new_data, ...) {
  UseMethod("spruce")
}

#' @rdname spruce
#' @export
spruce.default <- function(type, new_data, ...) {
  glubort("Unknown type: {type}.")
}

#' @rdname spruce
#' @export
spruce.response <- function(type, new_data, .pred, ...) {

  validate_is(.pred, is.numeric, "numeric", ".pred")

  predictions <- tibble(.pred = .pred)

  validate_prediction_size(predictions, new_data)

  predictions
}

#' @rdname spruce
#' @export
spruce.class <- function(type, new_data, .pred_class, ...) {

  validate_inherits(.pred_class, ".pred_class", "factor")

  predictions <- tibble(.pred_class = .pred_class)

  validate_prediction_size(predictions, new_data)

  predictions
}

#' @rdname spruce
#' @export
spruce.prob <- function(type, new_data, .pred_levels, .prob_matrix, ...) {

  # Assumes the order is correct!
  # Assumes you gave it the original levels and the class probability matrix

  validate_inherits(.pred_levels, ".pred_levels", "character")
  validate_inherits(.prob_matrix, ".prob_matrix", "matrix")

  n_levels <- length(.pred_levels)
  n_col <- ncol(.prob_matrix)

  if (n_levels != n_col) {
    glubort(
      "The number of levels ({n_levels}) must be
      equal to the number of class probability columns ({n_col}).")
  }

  .pred_levels <- paste0(".pred_", .pred_levels)

  colnames(.prob_matrix) <- .pred_levels

  predictions <- tibble::as_tibble(.prob_matrix)

  validate_prediction_size(predictions, new_data)

  predictions
}
