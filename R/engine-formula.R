#' Create a new formula engine
#'
#' This is the base class for a formula preprocessing engine. If you want to
#' create a new preprocessing engine that accepts input from the user in the
#' form of a formula, start here.
#'
#' @return
#'
#' A preprocessing engine with the class, `"formula_engine"`.
#'
#' @inheritParams new_engine
#'
#' @param formula Either `NULL`, or a formula that specifies how the
#' predictors and outcomes should be preprocessed. This argument is set
#' automatically at [mold()] time.
#'
#' @param indicators A logical. Should factors be expanded into dummy variables?
#'
#' @export
new_formula_engine <- function(mold,
                               forge,
                               intercept = FALSE,
                               info = NULL,
                               formula = NULL,
                               indicators = TRUE,
                               ...,
                               subclass = character()) {

  if (rlang::is_missing(mold)) {
    abort_no_mold()
  }

  if (rlang::is_missing(forge)) {
    abort_no_forge()
  }

  mold <- check_mold_formula(mold)
  forge <- check_forge(forge)

  validate_mold_args(
    mold,
    required_clean_args = c("engine", "data"),
    required_process_args = c("engine", "data")
  )

  validate_forge_args(forge)
  validate_is_formula_or_null(formula)
  validate_is_bool(indicators)

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    formula = formula,
    indicators = indicators,
    ...,
    subclass = c(subclass, "formula_engine")
  )

}

#' @export
refresh_engine.formula_engine <- function(engine) {
  do.call(new_formula_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

check_mold_formula <- function(mold) {

  validate_has_function_set_structure(mold)

  if (is.null(mold$clean)) {
    mold$clean <- get_default_mold_formula_clean()
  }

  mold
}

get_default_mold_formula_clean <- function() {

  function(engine, data) {
    list(
      engine = engine,
      data = data
    )
  }

}

# ------------------------------------------------------------------------------

validate_is_formula_or_null <- function(formula) {
  validate_is_or_null(formula, rlang::is_formula, "formula")
}
