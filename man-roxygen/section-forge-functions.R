#' @section Forge Functions:
#'
#' `blueprint$forge` should be a named list with two elements, both of which
#' are functions:
#'
#' - `clean`: A function that performs initial cleaning of `new_data`:
#'
#'    - _Arguments_:
#'
#'       - `blueprint`, `new_data`, and `outcomes`.
#'
#'    - _Output_: A named list of the following elements:
#'
#'       - `blueprint`: The blueprint, returned and potentially updated.
#'
#'       - `predictors`: A tibble containing the cleaned predictors.
#'
#'       - `outcomes`: A tibble containing the cleaned outcomes.
#'
#'       - `extras`: A named list of any extras obtained while cleaning. These
#'       are passed on to the `process()` function for further use.
#'
#' - `process`: A function that performs the actual preprocessing of the data
#' using the known information in the `blueprint`.
#'
#'    - _Arguments_:
#'
#'       - `blueprint`, `new_data`, `outcomes`, `extras`.
#'
#'    - _Output_: A named list of the following elements:
#'
#'       - `blueprint`: The blueprint, returned and potentially updated.
#'
#'       - `predictors`: A tibble of the predictors.
#'
#'       - `outcomes`: A tibble of the outcomes, or `NULL`.
#'
#'       - `extras`: Varies based on the blueprint. If the blueprint has no
#'       extra information, `NULL`. Otherwise a named list of the
#'       extra elements returned by the blueprint.
#'
#' Both `blueprint$forge$clean()` and `blueprint$forge$process()` will be called,
#' in order, from [forge()].
