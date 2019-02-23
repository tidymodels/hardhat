#' Mold - Recipes Method
#'
#' @description
#'
#' For a recipe, `mold()` does the following:
#'
#' - Calls [recipes::prep()] to prep the recipe.
#'
#' - Calls [recipes::juice()] to extract the outcomes and predictors. These
#' are returned as tibbles.
#'
#' - If `intercept = TRUE`, adds an intercept column to the predictors.
#'
#' @inheritParams mold
#' @inheritParams mold.formula
#'
#' @param x An unprepped recipe created from [recipes::recipe()].
#'
#' @inherit mold return
#'
#' @examples
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Recipes example
#'
#' # Create a recipe that logs a predictor
#' rec <- recipe(Species ~ Sepal.Length + Sepal.Width, iris) %>%
#'    step_log(Sepal.Length)
#'
#' processed <- mold(rec, iris)
#'
#' # Sepal.Length has been logged
#' processed$predictors
#'
#' processed$outcomes
#'
#' # The underlying engine is a prepped recipe
#' processed$preprocessor$engine
#'
#' # ---------------------------------------------------------------------------
#' # With an intercept
#'
#' # You can add a predictor with `intercept = TRUE`
#' processed <- mold(rec, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' # But you also could have used a recipe step
#' rec2 <- step_intercept(rec)
#'
#' mold(rec2, iris)$predictors
#'
#' @rdname mold-recipe
NULL

# TODO - this is really engine specific information
