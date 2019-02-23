#' @section Forge Functions:
#'
#' `engine$forge` should be a named list with two elements, both of which
#' are functions:
#'
#' - `clean`: A function that performs initial cleaning of `new_data`:
#'
#'    - _Arguments_:
#'
#'       - `engine` and `new_data`.
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
#'       - `engine` and `new_data`.
#'
#'    - _Output_: A named list of three elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `predictors`: A named list of 2 elements:
#'
#'          - `data`: The processed predictors.
#'
#'          - `offset`: An offset, or `NULL`.
#'
#'       - `outcomes`: A named list of 1 element:
#'
#'          - `data`: The processed outcomes.
#'
#' Both `engine$forge$clean()` and `engine$forge$process()` will be called,
#' in order, from [forge()].
