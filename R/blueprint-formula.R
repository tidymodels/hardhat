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
new_formula_blueprint <- function(intercept = FALSE,
                                  allow_novel_levels = FALSE,
                                  ptypes = NULL,
                                  formula = NULL,
                                  indicators = "traditional",
                                  composition = "tibble",
                                  ...,
                                  subclass = character()) {
  check_formula(formula, allow_null = TRUE)
  check_indicators(indicators)

  new_blueprint(
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

check_formula_blueprint <- function(x,
                                    ...,
                                    arg = caller_arg(x),
                                    call = caller_env()) {
  check_inherits(x, "formula_blueprint", arg = arg, call = call)
}

# ------------------------------------------------------------------------------

check_indicators <- function(indicators, error_call = caller_env()) {
  arg_match0(
    arg = indicators,
    values = c("traditional", "none", "one_hot"),
    error_call = error_call
  )
}
