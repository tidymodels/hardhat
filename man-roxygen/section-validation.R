#' @section Validation:
#'
#' hardhat provides validation functions at two levels.
#'
#' - `check_*()`:  _check a condition, and return a list_. The list
#' always contains at least one element, `ok`, a logical that specifies if the
#' check passed. Each check also has check specific elements in the returned
#' list that can be used to construct meaningful error messages.
#'
#' - `validate_*()`: _check a condition, and error if it does not pass_. These
#' functions call their corresponding check function, and
#' then provide a default error message. If you, as a developer, want a
#' different error message, then call the `check_*()` function yourself,
#' and provide your own validation function.
