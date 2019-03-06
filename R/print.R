#' @export
format.xy_engine <- function(x, ...) "XY"

#' @export
format.recipe_engine <- function(x, ...) "Recipe"

#' @export
format.formula_engine <- function(x, ...) "Formula"

#' @export
print.hardhat_engine <- function(x, ...) {
  cat_line("{format(x)} Engine:")
  cat_line("\n")
  cat_line("# Predictors: {ncol(x$ptypes$predictors)}")
  cat_line("  # Outcomes: {ncol(x$ptypes$outcomes)}")
  cat_line("   Intercept: {x$intercept}")
  invisible(x)
}

#' @export
print.formula_engine <- function(x, ...) {
  NextMethod()
  cat_line("  Indicators: {x$indicators}")
  invisible(x)
}

cat_line <- function(..., .envir = parent.frame()) {
  lines <- paste(glue(..., .envir = .envir), "\n")
  cat(lines, sep = "")
}
