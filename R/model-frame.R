#' Construct a model frame
#'
#' `model_frame()` is a stricter version of [stats::model.frame()]. There are
#' a number of differences, with the main being that rows are _never_ dropped
#' and the return value is a list with the frame and terms separated into
#' two distinct objects.
#'
#' @param formula A formula or terms object representing the terms of the
#' model frame.
#'
#' @param data A data frame or matrix containing the terms of `formula`.
#'
#' @return
#'
#' A named list with two elements:
#'
#' - `"data"`: A tibble containing the model frame.
#'
#' - `"terms"`: A terms object containing the terms for the model frame.
#'
#' @details
#'
#' The following explains the rationale for some of the difference in arguments
#' compared to [stats::model.frame()]:
#'
#' - `subset`: Not allowed because the number of rows before and after
#' `model_frame()` has been run should always be the same.
#'
#' - `na.action`: Not allowed and is forced to `"na.pass"` because the
#' number of rows before and after `model_frame()` has been run should always
#' be the same.
#'
#' - `drop.unused.levels`: Not allowed because it seems inconsistent for
#' `data` and the result of `model_frame()` to ever have the same factor column
#' but with different levels, unless specified though `original_levels`. If
#' this is required, it should be done through a recipe step explicitly.
#'
#' - `xlev`: Not allowed because this check should have been done ahead of
#' time. Use [scream()] to check the integrity of `data` against a training
#' set if that is required.
#'
#' - `...`: Not exposed because offsets are handled separately, and
#' it is not necessary to pass weights here any more because rows are never
#' dropped (so weights don't have to be subset alongside the rest of the
#' design matrix). If other non-predictor columns are required, use the
#' "roles" features of recipes.
#'
#' It is important to always use the results of `model_frame()` with
#' [model_matrix()] rather than [stats::model.matrix()] because the tibble
#' in the result of `model_frame()` does _not_ have a terms object attached.
#' If `model.matrix(<terms>, <tibble>)` is called directly, then a call to
#' `model.frame()` will be made automatically, which can give faulty results.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example usage
#'
#' framed <- model_frame(Species ~ Sepal.Width, iris)
#'
#' framed$data
#'
#' framed$terms
#'
#' # ---------------------------------------------------------------------------
#' # Missing values never result in dropped rows
#'
#' iris2 <- iris
#' iris2$Sepal.Width[1] <- NA
#'
#' framed2 <- model_frame(Species ~ Sepal.Width, iris2)
#'
#' head(framed2$data)
#'
#' nrow(framed2$data) == nrow(iris2)
#' @export
model_frame <- function(formula, data) {
  check_formula(formula)
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  frame <- with_na_pass(
    stats::model.frame(formula, data = data)
  )

  # Can't simplify terms env here, sometimes we need it to exist
  terms <- terms(frame)

  data <- hardhat_new_tibble(frame, size = vec_size(frame))

  list(
    data = data,
    terms = terms
  )
}
