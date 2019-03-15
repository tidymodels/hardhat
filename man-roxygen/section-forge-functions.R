#' @section Forge Functions:
#'
#' `engine$forge` should be a named list with two elements, both of which
#' are functions:
#'
#' - `clean`: A function that performs initial cleaning of `new_data`:
#'
#'    - _Arguments_:
#'
#'       - `engine`, `new_data`, and `outcomes`.
#'
#'    - _Output_: A named list of the following elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `predictors`: A tibble containing the cleaned predictors.
#'
#'       - `outcomes`: A tibble containing the cleaned outcomes.
#'
#'       - `extras`: A named list of any extras obtained while cleaning. These
#'       are passed on to the `process()` function for further use.
#'
#' - `process`: A function that performs the actual preprocessing of the data
#' using the known information in the `engine`.
#'
#'    - _Arguments_:
#'
#'       - `engine`, `new_data`, `outcomes`, `extras`.
#'
#'    - _Output_: A named list of the following elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `predictors`: A tibble of the predictors.
#'
#'       - `outcomes`: A tibble of the outcomes, or `NULL`.
#'
#'       - `extras`: Varies based on the engine. If the engine has no
#'       extra information, `NULL`. Otherwise a named list of the
#'       extra elements returned by the engine.
#'
#' Both `engine$forge$clean()` and `engine$forge$process()` will be called,
#' in order, from [forge()].
