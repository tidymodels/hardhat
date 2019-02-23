#' Mold - Formula Method
#'
#' @description
#'
#' For a formula, `mold()` does the following:
#'
#' - Predictors
#'
#'    - The RHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ RHS`.
#'
#'    - Runs [stats::model.frame()] on the RHS formula and uses `data`.
#'
#'    - If `indicators = TRUE`, it then runs [stats::model.matrix()] on the
#'    result.
#'
#'    - If `indicators = FALSE`, factors are removed before `model.matrix()`
#'    is run, and then added back afterwards. No interactions or inline
#'    functions involving factors are allowed.
#'
#'    - If any offsets are present from using `offset()`, then they are
#'    extracted with [model_offset()].
#'
#'    - If `intercept = TRUE`, adds an intercept column.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' - Outcomes
#'
#'    - The LHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ LHS`.
#'
#'    - Runs [stats::model.frame()] on the LHS formula and uses `data`.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' @inheritParams mold.data.frame
#'
#' @param formula A formula specifying the terms of the model, with the outcomes
#' on the left-hand side of the formula, and the predictors on the right-hand
#' side.
#'
#' @param data A data frame containing the predictors and the outcomes.
#'
#' @param indicators Should factors and interactions be expanded
#' (In other words, should [stats::model.matrix()] be run)? If
#' `FALSE`, factor columns are returned without being expanded into dummy
#' variables and a warning is thrown if any interactions are detected.
#'
#' @section Differences From Base R:
#'
#' There are a number of differences from base R regarding how formulas are
#' processed by `mold()` that require some explanation.
#'
#' Multivariate outcomes can be specified on the LHS using syntax that is
#' similar to the RHS (i.e. `outcome_1 + outcome_2 ~ predictors`).
#' If any complex calculations are done on the LHS and they return matrices
#' (like [stats::poly()]), then those matrices are flattened into multiple
#' columns of the tibble after the call to `model.frame()`. While this is
#' possible, it is not recommended, and if a large amount of preprocessing is
#' required on the outcomes you are better off using a [recipes::recipe()].
#'
#' Global variables are _not_ allowed in the formula. An error will be thrown
#' if they are included. All terms in the formula should come from `data`.
#'
#' By default, intercepts are _not_ included in the predictor output from the
#' formula. To include an intercept, set `intercept = TRUE`. Having an intercept
#' argument is consistent with the other `mold()` methods. More importantly,
#' there are often modeling packages where an intercept is either always or
#' never allowed (for example, the `earth` package), and they do some fancy
#' footwork to keep the user from providing or removing an intercept.
#' This interface standardizes all of that flexibility in one place.
#'
#' @details
#'
#' While not different from base R, the behavior of expanding factors into
#' dummy variables when an intercept is _not_ present should be documented.
#'
#' - When an intercept is present, factors are expanded into `K-1` new columns,
#' where `K` is the number of levels in the factor.
#'
#' - When an intercept is _not_ present, factors are expanded into all `K`
#' columns (one-hot encoding).
#'
#' Offsets can be included in the formula method through the use of the inline
#' function [stats::offset()]. These are returned as a tibble with 1 column
#' named `".offset"` in the `$outcome` slot of the return value.
#'
#' @inherit mold return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Formula example
#'
#' processed <- mold(Sepal.Width ~ Species, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' processed$outcomes
#'
#' # ---------------------------------------------------------------------------
#' # Factors without an intercept
#'
#' # No intercept is added
#' processed <- mold(Sepal.Width ~ Species, iris)
#'
#' # So factor columns are completely expanded
#' # into all `K` columns (the number of levels)
#' processed$predictors
#'
#' # ---------------------------------------------------------------------------
#' # Global variables
#'
#' y <- rep(1, times = nrow(iris))
#'
#' # In base R, global variables are allowed in a model formula
#' frame <- model.frame(Species ~ y + Sepal.Length, iris)
#' head(frame)
#'
#' # mold() does not allow them, and throws an error
#' tryCatch(
#'   expr = mold(Species ~ y + Sepal.Length, iris),
#'   error = function(e) print(e$message)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables and interactions
#'
#' # By default, factor columns are expanded
#' # and interactions are created, both by
#' # calling model.matrix(). Some models (like
#' # tree based models) can take factors directly
#' # but still might want to use the formula method.
#' # In those cases, set `indicators = FALSE` to not
#' # run model.matrix() on factor columns. Interactions
#' # are still allowed and run on numeric columns.
#'
#' processed <- mold(
#'   ~ Species + Sepal.Width:Sepal.Length,
#'   iris,
#'   indicators = FALSE
#' )
#'
#' processed$predictors
#'
#' # An informative error is thrown when `indicators = FALSE` and
#' # factors are present in interaction terms or in inline functions
#' \dontrun{
#' mold(Sepal.Width ~ Sepal.Length:Species, iris, indicators = FALSE)
#' mold(Sepal.Width ~ paste0(Species), iris, indicators = FALSE)
#' }
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas can be specified easily
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, iris)
#' processed$outcomes
#'
#' # Inline functions on the LHS are run, but any matrix
#' # output is flattened (like what happens in `model.matrix()`)
#' # (essentially this means you don't wind up with columns
#' # in the tibble that are matrices)
#' processed <- mold(poly(Sepal.Length, degree = 2) ~ Species, iris)
#' processed$outcomes
#'
#' # TRUE
#' ncol(processed$outcomes) == 2
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets are handled specially in base R, so they deserve special
#' # treatment here as well. You can add offsets using the inline function
#' # offset()
#' processed <- mold(Sepal.Width ~ offset(Sepal.Length) + Species, iris)
#'
#' processed$offset
#'
#' # Multiple offsets can be included, and they get added together
#' processed <- mold(
#'   Sepal.Width ~ offset(Sepal.Length) + offset(Petal.Width),
#'   iris
#' )
#'
#' identical(
#'   processed$offset$.offset,
#'   iris$Sepal.Length + iris$Petal.Width
#' )
#'
#' @rdname mold-formula
#'
#' @export
NULL

# TODO - this is really engine specific information
