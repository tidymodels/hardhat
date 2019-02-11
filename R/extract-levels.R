#' Extract the levels from a factor outcome
#'
#' If `y` is a factor, then `extract_outcome_levels()` will return the levels of
#' the factor as a character vector. If `y` is a data frame of factors
#' a named list of character vectors will be returned.
#'
#' @inheritParams new_base_model
#'
#' @param y The outcome. A factor or data frame of factors.
#'
#' @return
#'
#' If `variateness == "univariate"`, a single character vector for the
#' levels of `y`.
#'
#' If `variateness == "multivariate"`, a named list of character vectors
#' for the levels of each factor column of `y`.
#'
#' @examples
#'
#' y <- as.factor(letters[1:5])
#'
#' extract_outcome_levels(y, "classification", "univariate")
#'
#' extract_outcome_levels(as.data.frame(y), "classification", "univariate")
#'
#' z <- as.factor(letters[6:10])
#' y_z_df <- data.frame(y = y, z = z)
#'
#' extract_outcome_levels(y_z_df, "classification", "multivariate")
#'
#' @export
#'
extract_outcome_levels <- function(y, mode, variateness) {

  y <- extract_outcome(y, mode, variateness)

  if (mode != "classification") {
    return(NULL)
  }

  if (variateness == "univariate") {
    outcome_levels <- levels(y)
  }
  else if (variateness == "multivariate") {
    y_nms <- colnames(y)
    outcome_levels <- lapply(y, levels)
    names(outcome_levels) <- y_nms
  }

  outcome_levels
}
