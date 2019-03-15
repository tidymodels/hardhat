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
#'          - `data`: The cleaned data.
#'
#' - `process`: A function that performs the actual preprocessing of the data.
#'
#'    - _Arguments_:
#'
#'       - If this is an xy engine, `engine`, `x` and `y`.
#'
#'       - Otherwise, `engine` and `data`.
#'
#'    - _Output_: A named list of 5 elements:
#'
#'       - `engine`: The engine, returned and potentially updated.
#'
#'       - `predictors`: A tibble of predictors.
#'
#'       - `outcomes`: A tibble of outcomes.
#'
#'       - `ptypes`: A named list with 2 elements, `predictors` and `outcomes`,
#'       where both elements are 0-row tibbles.
#'
#'       - `extras`: Varies based on the engine. If the engine has no
#'       extra information, `NULL`. Otherwise a named list of the
#'       extra elements returned by the engine.
#'
#' Both `engine$mold$clean()` and `engine$mold$process()` will be called,
#' in order, from [mold()].
