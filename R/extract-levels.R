#' Extract the levels from a factor outcome
#' 
#' If `y` is a factor, or a data frame of factors, then 
#' `extract_outcome_levels()` will return the levels of the factor
#' as either a character vector, or a list of character vectors if
#' `y` is multivariate.
#' 
#' @inheritParams new_base_model
#' @param y The outcome. A factor or data frame of factors.
#' 
#' @export
#' 
extract_outcome_levels <- function(y, mode, variateness) {
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
  else {
    glubort("Unknown variateness: {variateness}.")
  }
  
  outcome_levels
}