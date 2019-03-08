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
  cat_line("# Predictors: {n_engine_predictors(x)}")
  cat_line("  # Outcomes: {n_engine_outcomes(x)}")
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

n_engine_predictors <- function(x) {
  ncol(x$ptypes$predictors) %||% 0L
}

n_engine_outcomes <- function(x) {
  ncol(x$ptypes$outcomes) %||% 0L
}
