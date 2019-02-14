#' Add an intercept column to `x`
#'
#' This function adds an integer column of `1`'s to `x`.
#'
#' If a column named `name` already exists in `x`, then `x` is returned
#' unchanged and a warning is issued.
#'
#' @param x A data frame or matrix.
#' @param name The name for the intercept column. Defaults to `"(Intercept)"`,
#' which is the same name that [stats::lm()] uses.
#'
#' @examples
#' add_intercept_column(mtcars)
#'
#' add_intercept_column(mtcars, "intercept")
#'
#' add_intercept_column(as.matrix(mtcars))
#'
#' @export
add_intercept_column <- function(x, name = "(Intercept)") {

  ok <- is.data.frame(x) || is.matrix(x)

  if (!ok) {
    glubort(
      "`x` must be a data.frame or matrix to add an intercept column, ",
      "not a '{class1(x)}'."
    )
  }

  validate_name(name)

  if (name %in% colnames(x)) {

    rlang::warn(glue::glue(
      "`x` already has a column named '{name}'. ",
      "Returning `x` unchanged."
    ))

    return(x)
  }

  if (is.matrix(x)) {

    new_col <- matrix(
      data = 1L,
      nrow = nrow(x),
      dimnames = list(NULL, name)
    )

    x <- cbind(new_col, x)

    return(x)
  }

  if (is.data.frame(x)) {

    x <- tibble::add_column(x, !!name := 1L, .before = 1L)

    return(x)
  }

}

maybe_add_intercept_column <- function(x, intercept = FALSE) {

  if (!intercept) {
    return(x)
  }

  add_intercept_column(x)
}

validate_name <- function(name) {

  if (length(name) > 1) {
    glubort("name should have size 1, not {length(name)}.")
  }

  validate_is(name, rlang::is_character, "character")

  invisible(name)
}
