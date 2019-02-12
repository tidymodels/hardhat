#' @importFrom rlang abort is_missing arg_match
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom zeallot %<-%
NULL

# stats related imports

#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats delete.response
#' @importFrom stats .MFclass
#' @importFrom stats .getXlevels
#' @importFrom stats lm.fit
#' @importFrom stats get_all_vars
NULL

# Required because of zeallot %<-%

utils::globalVariables(
  c("predictors", "outcomes", "preprocessor")
)
