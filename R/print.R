#' @export
format.default_preprocessor <- function(x, ...) "Default"

#' @export
format.recipes_preprocessor <- function(x, ...) "Recipe"

#' @export
format.terms_preprocessor <- function(x, ...) "Formula"

#' @export
print.preprocessor <- function(x, ...) {
  cat_line("{format(x)} Preprocessor:")
  cat_line("\n")
  cat_line("# Predictors: {length(x$predictors$names)}")
  cat_line("  # Outcomes: {length(x$outcomes$names)}")
  cat_line("   Intercept: {x$intercept}")
  invisible(x)
}

#' @export
print.terms_preprocessor <- function(x, ...) {
  NextMethod()
  cat_line("  Indicators: {x$indicators}")
  invisible(x)
}

cat_line <- function(..., .envir = parent.frame()) {
  lines <- paste(glue(..., .envir = .envir), "\n")
  cat(lines, sep = "")
}
