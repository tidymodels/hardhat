#' @export
format.xy_blueprint <- function(x, ...) "XY"

#' @export
format.recipe_blueprint <- function(x, ...) "Recipe"

#' @export
format.formula_blueprint <- function(x, ...) "Formula"

#' @export
print.hardhat_blueprint <- function(x, ...) {
  cat_line("{format(x)} blueprint:")
  cat_line("\n")
  cat_line("# Predictors: {n_blueprint_predictors(x)}")
  cat_line("  # Outcomes: {n_blueprint_outcomes(x)}")
  cat_line("   Intercept: {x$intercept}")
  cat_line("Novel Levels: {x$allow_novel_levels}")
  invisible(x)
}

#' @export
print.formula_blueprint <- function(x, ...) {
  NextMethod()
  cat_line("  Indicators: {x$indicators}")
  invisible(x)
}

cat_line <- function(..., .envir = parent.frame()) {
  lines <- paste(glue(..., .envir = .envir), "\n")
  cat(lines, sep = "")
}

n_blueprint_predictors <- function(x) {
  ncol(x$ptypes$predictors) %||% 0L
}

n_blueprint_outcomes <- function(x) {
  ncol(x$ptypes$outcomes) %||% 0L
}
