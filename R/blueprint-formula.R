#' @param formula Either `NULL`, or a formula that specifies how the
#' predictors and outcomes should be preprocessed. This argument is set
#' automatically at [mold()] time.
#'
#' @param indicators A logical. Should factors be expanded into dummy variables?
#'
#' @rdname new-blueprint
#' @export
new_formula_blueprint <- function(mold,
                                  forge,
                                  intercept = FALSE,
                                  ptypes = NULL,
                                  formula = NULL,
                                  indicators = TRUE,
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

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
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
