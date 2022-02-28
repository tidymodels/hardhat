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
add_intercept_column <- function(data, name = "(Intercept)") {
  ok <- is.data.frame(data) || is.matrix(data)

  if (!ok) {
    glubort(
      "`data` must be a data.frame or matrix to add an intercept column, ",
      "not a '{class1(data)}'."
    )
  }

  validate_name(name)

  if (name %in% colnames(data)) {
    rlang::warn(glue::glue(
      "`data` already has a column named '{name}'. ",
      "Returning `data` unchanged."
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

maybe_add_intercept_column <- function(data, intercept = FALSE) {
  if (!intercept) {
    return(data)
  }

  add_intercept_column(data)
}

validate_name <- function(name) {
  if (length(name) > 1) {
    glubort("name should have size 1, not {length(name)}.")
  }

  validate_is(name, rlang::is_character, "character")

  invisible(name)
}
