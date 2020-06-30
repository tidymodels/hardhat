#' @param formula Either `NULL`, or a formula that specifies how the
#' predictors and outcomes should be preprocessed. This argument is set
#' automatically at [mold()] time.
#'
#' @param indicators A character string with possible values "none" (no dummy
#' variables), "traditional" (create dummy variables using the usual methods),
#' or "one-hot" (create dummy variables for all levels).
#'
#'
#' @rdname new-blueprint
#' @export
new_formula_blueprint <- function(mold,
                                  forge,
                                  intercept = TRUE,
                                  allow_novel_levels = FALSE,
                                  ptypes = NULL,
                                  formula = NULL,
                                  indicators = "traditional",
                                  ...,
                                  subclass = character()) {

  validate_is_function_set(mold)
  validate_mold_args(
    mold,
    required_clean_args = c("blueprint", "data"),
    required_process_args = c("blueprint", "data")
  )

  validate_is_formula_or_null(formula)

  indicators <- validate_indicators(indicators)

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    ptypes = ptypes,
    formula = formula,
    indicators = indicators,
    ...,
    subclass = c(subclass, "formula_blueprint")
  )

}

#' @export
refresh_blueprint.formula_blueprint <- function(blueprint) {
  do.call(new_formula_blueprint, as.list(blueprint))
}

is_formula_blueprint <- function(x) {
  inherits(x, "formula_blueprint")
}

validate_is_formula_blueprint <- function(blueprint) {
  validate_is(blueprint, is_formula_blueprint, "formula_blueprint")
}

# ------------------------------------------------------------------------------

validate_is_formula_or_null <- function(formula) {
  validate_is_or_null(formula, rlang::is_formula, "formula")
}

# ------------------------------------------------------------------------------

validate_indicators <- function(indicators) {
  indicators <- compat_indicators(indicators)
  validate_is_character(indicators, "indicators")

  n_indicators <- length(indicators)
  if (n_indicators != 1L) {
    glubort("`indicators` must have size 1, not {n_indicators}.")
  }

  rlang::arg_match(indicators, c("traditional", "none", "one-hot"))
}

# TODO: Deprecate in hardhat 0.1.6
compat_indicators <- function(indicators) {
  if (!is.logical(indicators)) {
    return(indicators)
  }

  msg <- paste0(
    "`indicators` now requires a character argument as of hardhat '0.1.4'.\n",
    "This warning will become an error in hardhat '0.1.6'.\n",
    "Update with:\n",
    "- `indicators = TRUE`  -> `indicators = \"traditional\"`\n",
    "- `indicators = FALSE` -> `indicators = \"none\"`\n"
  )

  signal_soft_deprecated(msg)

  if (rlang::is_true(indicators)) {
    "traditional"
  } else {
    "none"
  }
}
