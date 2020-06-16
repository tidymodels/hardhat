#' @param formula Either `NULL`, or a formula that specifies how the
#' predictors and outcomes should be preprocessed. This argument is set
#' automatically at [mold()] time.
#'
#' @param indicators A logical. Should factors be expanded into dummy variables?
#'
#' @param one_hot A logical. Should the complete set of dummy variables be generated?
#'
#' @rdname new-blueprint
#' @export
new_formula_blueprint <- function(mold,
                                  forge,
                                  intercept = FALSE,
                                  allow_novel_levels = FALSE,
                                  ptypes = NULL,
                                  formula = NULL,
                                  indicators = TRUE,
                                  one_hot = FALSE,
                                  ...,
                                  subclass = character()) {

  validate_is_function_set(mold)
  validate_mold_args(
    mold,
    required_clean_args = c("blueprint", "data"),
    required_process_args = c("blueprint", "data")
  )

  validate_is_formula_or_null(formula)
  validate_is_bool(indicators)
  validate_is_bool(one_hot)

  if (!indicators & one_hot) {
    rlang::abort("'one_hot' should be FALSE if 'indicators' is FALSE.")
  }

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    ptypes = ptypes,
    formula = formula,
    indicators = indicators,
    one_hot = one_hot,
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
