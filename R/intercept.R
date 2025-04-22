#' Add an intercept column to `data`
#'
#' This function adds an integer column of `1`'s to `data`.
#'
#' If a column named `name` already exists in `data`, then `data` is returned
#' unchanged and a warning is issued.
#'
#' @param data A data frame or matrix.
#'
#' @param name The name for the intercept column. Defaults to `"(Intercept)"`,
#' which is the same name that [stats::lm()] uses.
#'
#' @inheritParams validate_column_names
#'
#' @return
#'
#' `data` with an intercept column.
#'
#' @examples
#' add_intercept_column(mtcars)
#'
#' add_intercept_column(mtcars, "intercept")
#'
#' add_intercept_column(as.matrix(mtcars))
#' @export
add_intercept_column <- function(
  data,
  name = "(Intercept)",
  ...,
  call = current_env()
) {
  check_dots_empty0(...)
  check_data_frame_or_matrix(data, call = call)
  check_name(name, call = call)

  if (name %in% colnames(data)) {
    cli::cli_warn(c(
      "{.arg data} already has a column named {.val {name}}.",
      "i" = "Returning {.arg data} unchanged."
    ))

    return(data)
  }

  if (is.matrix(data)) {
    new_col <- matrix(
      data = 1L,
      nrow = nrow(data),
      dimnames = list(NULL, name)
    )

    data <- cbind(new_col, data)

    return(data)
  }

  if (is.data.frame(data)) {
    data <- tibble::add_column(data, !!name := 1L, .before = 1L)

    return(data)
  }
}

maybe_add_intercept_column <- function(
  data,
  intercept = FALSE,
  ...,
  call = caller_env()
) {
  check_dots_empty0(...)
  if (!intercept) {
    return(data)
  }

  add_intercept_column(data, call = call)
}
