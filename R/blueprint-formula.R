#' @param formula Either `NULL`, or a formula that specifies how the
#' predictors and outcomes should be preprocessed. This argument is set
#' automatically at [mold()] time.
#'
#' @param indicators A single character string. Control how factors are
#' expanded into dummy variable indicator columns. One of:
#'
#'   - `"traditional"` - The default. Create dummy variables using the
#'   traditional [model.matrix()] infrastructure. Generally this creates
#'   `K - 1` indicator columns for each factor, where `K` is the number of
#'   levels in that factor.
#'
#'   - `"none"` - Leave factor variables alone. No expansion is done.
#'
#'   - `"one_hot"` - Create dummy variables using a one-hot encoding approach
#'   that expands unordered factors into all `K` indicator columns, rather than
#'   `K - 1`.
#'
#' @rdname new-blueprint
#' @export
new_formula_blueprint <- function(mold,
                                  forge,
                                  intercept = FALSE,
                                  allow_novel_levels = FALSE,
                                  ptypes = NULL,
                                  formula = NULL,
                                  indicators = "traditional",
                                  composition = "tibble",
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
    composition = composition,
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

  rlang::arg_match(indicators, c("traditional", "none", "one_hot"))
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

  # Number of frames up to mimic being called from `default_formula_blueprint()`
  # Not too worried about this not always being perfectly correct since it will
  # be removed upon deprecation of logical `compat_indicators()`
  # 1) Global
  # 2) default_formula_blueprint
  # 3) new_default_formula_blueprint
  # 4) new_formula_blueprint
  # 5) validate_indicators
  # 6) compat_indicators
  # From 6), look back up 5 frames
  env <- rlang::caller_env(5)
  signal_soft_deprecated(msg, env = env)

  if (rlang::is_true(indicators)) {
    "traditional"
  } else {
    "none"
  }
}
