#' @export
format.xy_blueprint <- function(x, ...) "XY"

#' @export
format.recipe_blueprint <- function(x, ...) "Recipe"

#' @export
format.formula_blueprint <- function(x, ...) "Formula"

#' @export
print.hardhat_blueprint <- function(x, ...) {
  cli::cli_text("{format(x)} blueprint:")

  cli::cli_par()
  cli::cli_text("# Predictors: {n_blueprint_predictors(x)}")
  cli::cli_text("# Outcomes: {n_blueprint_outcomes(x)}")
  cli::cli_text("Intercept: {x$intercept}")
  cli::cli_text("Novel Levels: {x$allow_novel_levels}")
  cli::cli_text("Composition: {x$composition}")
  if (inherits(x, "formula_blueprint")) {
    cli::cli_text("Indicators: {x$indicators}")
  }
  cli::cli_end()
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
