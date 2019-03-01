# Lists are too deeply nested to include in the rd docs

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
#'    - _Output_: A named list of two elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `new_data`: The `new_data`, cleaned.
#'
#' - `process`: A function that performs the actual preprocessing of the data
#' using the known information in the `engine`.
#'
#'    - _Arguments_:
#'
#'       - `engine`, `new_data`, and `outcomes`.
#'
#'    - _Output_: A named list of 4 elements:
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
