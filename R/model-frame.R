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
#' @param original_levels Optional. A named list of character vectors.
#' These represent the assumed factor levels for factor columns in `data`.
#' If any levels are missing for a factor, then the levels are restored with a
#' warning. If there are novel levels for a factor, then they are coerced to
#' `NA` with a warning.
#'
#' @param drop_novel Optional. A logical. Passed on to
#' [enforce_new_data_novel_levels()], and only applicable if `original_levels`
#' is not `NULL`. Should novel levels found in `new_data` but not in
#' `original_levels` be coerced to `NA`? No matter the value of `drop_novel`,
#' a warning will be thrown if any novel levels are detected.
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
#' @return
#'
#' A named list with two elements:
#'
#' - `"data"`: A tibble containing the model frame.
#'
#' - `"terms"`: A terms object containing the terms for the model frame.
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
#'
#' # ---------------------------------------------------------------------------
#' # Novel and missing levels
#'
#' train <- data.frame(y = factor(c("a", "b")))
#' test_not_enough <- data.frame(y = factor("a"))
#' test_too_many <- data.frame(y = factor(c("a", "b", "c")))
#' original_levels <- get_levels(train)
#'
#' # Missing levels are recovered with a warning
#' model_frame(~y, test_not_enough, original_levels)
#'
#' # Novel levels are forced to NA with a warning
#' model_frame(~y, test_too_many, original_levels)
#'
#' model_frame(~y, test_too_many, original_levels, drop_novel = FALSE)
#'
#' @export
#'
model_frame <- function(formula, data, original_levels = NULL, drop_novel = TRUE) {

  validate_is_formula(formula)
  data <- check_is_data_like(data)
  validate_levels_list(original_levels, "original_levels")

  data <- enforce_new_data_novel_levels(data, original_levels, drop_novel)
  data <- enforce_new_data_level_recovery(data, original_levels)

  frame <- rlang::with_options(
    stats::model.frame(formula, data = data),
    na.action = "na.pass"
  )

  # Can't simplify terms env here, sometimes we need it to exist
  terms <- terms(frame)

  attr(frame, "terms") <- NULL
  data <- tibble::as_tibble(frame)

  list(
    data = data,
    terms = terms
  )

}

validate_is_formula <- function(formula) {
  validate_is(formula, rlang::is_formula, "formula")
}
