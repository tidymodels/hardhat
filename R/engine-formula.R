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
