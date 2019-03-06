#' @importFrom rlang abort is_missing arg_match :=
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom zeallot %<-%
NULL

# stats related imports

#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats delete.response
#' @importFrom stats get_all_vars
#' @importFrom stats terms
NULL

# https://github.com/r-lib/vctrs/issues/114
#' @importFrom vctrs vec_cast.data.frame
NULL

# for zeallot
utils::globalVariables(
  c(
    ".predictors",
    ".outcomes",
    "predictors",
    "outcomes",
    "ptypes",
    "ptype",
    "extras",
    "predictors_lst",
    "outcomes_lst"
  )
)
