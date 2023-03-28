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

#' Spruce up multi-outcome predictions
#'
#' This family of `spruce_*_multiple()` functions converts multi-outcome
#' predictions into a standardized format. They are generally called from a
#' prediction implementation function for the specific `type` of prediction to
#' return.
#'
#' @param ... Multiple vectors of predictions:
#'
#'   - For `spruce_numeric_multiple()`, numeric vectors of equal size.
#'
#'   - For `spruce_class_multiple()`, factors of "hard" class predictions of
#'     equal size.
#'
#'   - For `spruce_prob_multiple()`, tibbles of equal size, which are the result
#'     of calling [spruce_prob()] on each matrix of prediction probabilities.
#'
#'   If the `...` are named, then this name will be used as the suffix on the
#'   resulting column name, otherwise a positional index will be used.
#'
#' @returns
#' - For `spruce_numeric_multiple()`, a tibble of numeric columns named with the
#'   pattern `.pred_*`.
#'
#' - For `spruce_class_multiple()`, a tibble of factor columns named with the
#'   pattern `.pred_class_*`.
#'
#' - For `spruce_prob_multiple()`, a tibble of data frame columns named with the
#'   pattern `.pred_*`.
#'
#' @name spruce-multiple
#' @examples
#' spruce_numeric_multiple(1:3, foo = 2:4)
#'
#' spruce_class_multiple(
#'   one_step = factor(c("a", "b", "c")),
#'   two_step = factor(c("a", "c", "c"))
#' )
#'
#' one_step <- matrix(c(.3, .7, .0, .1, .3, .6), nrow = 2, byrow = TRUE)
#' two_step <- matrix(c(.2, .7, .1, .2, .4, .4), nrow = 2, byrow = TRUE)
#' binary <- matrix(c(.5, .5, .4, .6), nrow = 2, byrow = TRUE)
#'
#' spruce_prob_multiple(
#'   one_step = spruce_prob(c("a", "b", "c"), one_step),
#'   two_step = spruce_prob(c("a", "b", "c"), two_step),
#'   binary = spruce_prob(c("yes", "no"), binary)
#' )
NULL

#' @rdname spruce-multiple
#' @export
spruce_numeric_multiple <- function(...) {
  args <- list2(...)
  call <- environment()

  map(args, function(x) {
    if (!is.numeric(x) || dims(x) != 1L) {
      cli::cli_abort(
        "Each element of `...` must be a numeric vector, not {.obj_type_friendly {x}}.",
        call = call
      )
    }
  })

  size <- vec_size_common(!!!args)
  list_check_all_size(args, size, arg = "")

  names(args) <- names_as_spruce_names(names2(args), prefix = ".pred_")

  hardhat_new_tibble(args, size)
}

#' @rdname spruce-multiple
#' @export
spruce_class_multiple <- function(...) {
  args <- list2(...)
  call <- environment()

  map(args, function(x) {
    if (!is.factor(x)) {
      cli::cli_abort(
        "Each element of `...` must be a factor, not {.obj_type_friendly {x}}.",
        call = call
      )
    }
  })

  size <- vec_size_common(!!!args)
  list_check_all_size(args, size, arg = "")

  names(args) <- names_as_spruce_names(names2(args), prefix = ".pred_class_")

  hardhat_new_tibble(args, size)
}

#' @rdname spruce-multiple
#' @export
spruce_prob_multiple <- function(...) {
  args <- list2(...)
  call <- environment()

  map(args, function(x) {
    if (!tibble::is_tibble(x)) {
      cli::cli_abort(
        "Each element of `...` must be a tibble, not {.obj_type_friendly {x}}.",
        call = call
      )
    }
  })

  size <- vec_size_common(!!!args)
  list_check_all_size(args, size, arg = "")

  names(args) <- names_as_spruce_names(names2(args), prefix = ".pred_")

  hardhat_new_tibble(args, size)
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

names_as_spruce_names <- function(names, prefix) {
  missing <- which(names == "")

  if (length(missing) > 0L) {
    names[missing] <- missing
  }

  vec_paste0(prefix, names)
}

dims <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    1L
  } else {
    length(d)
  }
}
