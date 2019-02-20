#' Construct a design matrix
#'
#' `model_matrix()` is a stricter version of [stats::model.matrix()]. Notably,
#' `model_matrix()` will _never_ drop rows, and the result will be a tibble.
#'
#' @param terms A terms object to construct a model matrix with. This is
#' typically the terms object returned from the corresponding call to
#' [model_frame()].
#'
#' @param data A tibble to construct the design matrix with. This is
#' typically the tibble returned from the corresponding call to
#' [model_frame()].
#'
#' @details
#'
#' The following explains the rationale for some of the difference in arguments
#' compared to [stats::model.matrix()]:
#'
#' - `contrasts.arg`: Set the contrasts argument, `options("contrasts")`
#' globally, or assign a contasts to the factor of interest directly using
#' [stats::contrasts()]. See the examples section.
#'
#' - `xlev`: Not allowed because `model.frame()` is never called, so it is
#' unnecessary.
#'
#' - `...`: Not allowed because the default method of `model.matrix()` does
#' not use it, and the `lm` method uses it to pass potential offsets and
#' weights through, which are handled differently in hardhat.
#'
#' @return
#'
#' A tibble containing the design matrix.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example usage
#'
#' framed <- model_frame(Sepal.Width ~ Species, iris)
#'
#' model_matrix(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Missing values never result in dropped rows
#'
#' iris2 <- iris
#' iris2$Species[1] <- NA
#'
#' framed2 <- model_frame(Sepal.Width ~ Species, iris2)
#'
#' model_matrix(framed2$terms, framed2$data)
#'
#' # ---------------------------------------------------------------------------
#' # Contrasts
#'
#' # Default contrasts
#' y <- factor(c("a", "b"))
#' x <- data.frame(y = y)
#' framed <- model_frame(~y, x)
#'
#' # Setting contrasts directly
#' y_with_contrast <- y
#' contrasts(y_with_contrast) <- contr.sum(2)
#' x2 <- data.frame(y = y_with_contrast)
#' framed2 <- model_frame(~y, x2)
#'
#' # Compare!
#' model_matrix(framed$terms, framed$data)
#' model_matrix(framed2$terms, framed2$data)
#'
#' # Also, can set the contrasts globally
#' global_override <- c(unordered = "contr.sum", ordered = "contr.poly")
#'
#' rlang::with_options(
#'   .expr = {
#'     model_matrix(framed$terms, framed$data)
#'   },
#'   contrasts = global_override
#' )
#'
#'
#' @export
model_matrix <- function(terms, data) {

  validate_is_terms(terms)
  data <- check_is_data_like(data)

  # otherwise model.matrix() will try and run model.frame() for us on data
  # but we definitely don't want this, as we have already done it and it can
  # actually error out if we don't prevent it from running
  attr(data, "terms") <- terms

  predictors <- rlang::with_options(
    model.matrix(object = terms, data = data),
    na.action = "na.pass"
  )

  predictors <- strip_model_matrix(predictors)

  tibble::as_tibble(predictors)
}

is_terms <- function(x) {
  inherits(x, "terms")
}

validate_is_terms <- function(terms) {
  validate_is(terms, is_terms, "terms object")
}
