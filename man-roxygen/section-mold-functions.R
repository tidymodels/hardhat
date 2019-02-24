# Lists are too deeply nested to include in the rd docs

#' @section Mold Functions:
#'
#' `engine$mold` should be a named list with two elements, both of which
#' are functions:
#'
#' - `clean`: A function that performs initial cleaning of the user's input
#' data to be used in the model.
#'
#'    - _Arguments_:
#'
#'       - If this is an xy engine, `engine`, `x` and `y`.
#'
#'       - Otherwise, `engine` and `data`.
#'
#'    - _Output_: A named list of three elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - If using an xy engine:
#'
#'          - `x`: The cleaned predictor data.
#'
#'          - `y`: The cleaned outcome data.
#'
#'       - If not using an xy engine:
#'
#'          - `data`: The cleaned input data.
#'
#' - `process`: A function that performs the actual preprocessing of the data.
#'
#'    - _Arguments_:
#'
#'       - If this is an xy engine, `engine`, `x` and `y`.
#'
#'       - Otherwise, `engine` and `data`.
#'
#'    - _Output_: A named list of three elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `predictors`: A named list of 3 elements:
#'
#'          - `data`: The processed predictors.
#'
#'          - `info`: A named list of 3 elements:
#'
#'             - `names`: A character vector of names of the original
#'             predictors.
#'
#'             - `classes`: A named list where the names are the original
#'             predictors, and the values are the classes of that predictor.
#'
#'             - `levels`: A named list where the names are the original
#'             factor predictors, and the values are the levels of that
#'             factor predictor.
#'
#'          - `offset`: An offset, or `NULL`.
#'
#'       - `outcomes`: A named list of 2 elements:
#'
#'          - `data`: The processed outcomes.
#'
#'          - `info`: A named list of 3 elements:
#'
#'             - `names`: A character vector of names of the original
#'             outcomes.
#'
#'             - `classes`: A named list where the names are the original
#'             outcomes, and the values are the classes of that outcome.
#'
#'             - `levels`: A named list where the names are the original
#'             factor outcomes, and the values are the levels of that
#'             factor outcome.
#'
#' Both `engine$mold$clean()` and `engine$mold$process()` will be called,
#' in order, from [mold()].
