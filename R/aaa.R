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

# R 3.1 bug https://github.com/r-lib/vctrs/issues/114
#' @importFrom vctrs vec_cast.character
#' @importFrom vctrs vec_cast.data.frame
#' @importFrom vctrs vec_cast.Date
#' @importFrom vctrs vec_cast.difftime
#' @importFrom vctrs vec_cast.double
#' @importFrom vctrs vec_cast.factor
#' @importFrom vctrs vec_cast.integer
#' @importFrom vctrs vec_cast.integer64
#' @importFrom vctrs vec_cast.list
#' @importFrom vctrs vec_cast.logical
#' @importFrom vctrs vec_cast.POSIXct
#' @importFrom vctrs vec_cast.POSIXlt
#' @importFrom vctrs vec_cast.raw
#' @importFrom vctrs vec_cast.vctrs_list_of
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
