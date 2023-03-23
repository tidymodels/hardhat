#' Spruce up predictions
#'
#' The family of `spruce_*()` functions convert predictions into a
#' standardized format. They are generally called from a prediction
#' implementation function for the specific `type` of prediction to return.
#'
#' After running a `spruce_*()` function, you should _always_ use the validation
#' function `validate_prediction_size()` to ensure that the number of rows
#' being returned is the same as the number of rows in the input (`new_data`).
#'
#' @param pred (`type = "numeric"`) A numeric vector of predictions.
#'
#' @param pred_class (`type = "class"`) A factor of "hard" class predictions.
#'
#' @param pred_levels,prob_matrix (`type = "prob"`)
#' - `pred_levels` should be a character vector of the original levels of
#' the outcome used in training.
#' - `prob_matrix` should be a numeric matrix of class probabilities with
#' as many columns as levels in `pred_levels`, and in the same order.
#'
#' @return
#'
#' A tibble, ideally with the same number of rows as the `new_data` passed
#' to `predict()`. The column names and number of columns vary based on the
#' function used, but are standardized.
#'
#' @name spruce
NULL

#' @rdname spruce
#' @export
spruce_numeric <- function(pred) {
  if (!is.numeric(pred)) {
    stop_input_type(pred, "a numeric vector")
  }
  if (dims(pred) != 1L) {
    stop_input_type(pred, "a numeric vector")
  }

  tibble(.pred = pred)
}

#' @rdname spruce
#' @export
spruce_class <- function(pred_class) {
  check_factor(pred_class)
  tibble(.pred_class = pred_class)
}

#' @rdname spruce
#' @export
spruce_prob <- function(pred_levels, prob_matrix) {
  # Assumes the order is correct!
  # Assumes you gave it the original levels and the class probability matrix

  check_character(pred_levels)

  if (!is.matrix(prob_matrix)) {
    stop_input_type(prob_matrix, "a numeric matrix")
  }
  if (!is.numeric(prob_matrix)) {
    stop_input_type(prob_matrix, "a numeric matrix")
  }

  n_levels <- length(pred_levels)
  n_col <- ncol(prob_matrix)

  if (n_levels != n_col) {
    glubort(
      "The number of levels ({n_levels}) must be
      equal to the number of class probability columns ({n_col})."
    )
  }

  pred_levels <- paste0(".pred_", pred_levels)

  colnames(prob_matrix) <- pred_levels

  predictions <- tibble::as_tibble(prob_matrix, .name_repair = "minimal")

  predictions
}

# ------------------------------------------------------------------------------

check_factor <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (!missing(x)) {
    if (is.factor(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a factor",
    ...,
    allow_na = FALSE,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}
