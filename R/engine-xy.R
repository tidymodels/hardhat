new_xy_engine <- function(mold,
                          forge,
                          intercept = FALSE,
                          info = NULL,
                          outcomes = NULL,
                          ...,
                          subclass = character()) {

  if (rlang::is_missing(mold)) {
    glubort(
      "`mold` must be supplied in the form: ",
      "list(clean = <function>, process = <function>)."
    )
  }

  if (rlang::is_missing(forge)) {
    glubort(
      "`forge` must be supplied in the form: ",
      "list(clean = <function>, process = <function>)."
    )
  }

  mold <- check_mold_xy(mold)
  forge <- check_forge(forge)

  validate_mold_xy_args(mold)
  validate_forge_args(forge)

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    outcomes = outcomes,
    ...,
    subclass = c(subclass, "xy_engine")
  )

}


# ------------------------------------------------------------------------------

check_mold_xy <- function(mold) {

  validate_has_function_set_structure(mold)

  if (is.null(mold$clean)) {
    mold$clean <- get_default_mold_xy_clean()
  }

  mold
}

get_default_mold_xy_clean <- function() {

  function(engine, x, y) {
    list(
      engine = engine,
      x = x,
      y = y
    )
  }

}

validate_mold_xy_args <- function(mold) {

  required_clean_args <- c("engine", "x", "y")
  actual_clean_args <- rlang::fn_fmls_names(mold$clean)

  if (!identical(actual_clean_args, required_clean_args)) {
    required_clean_args <- glue_quote_collapse(required_clean_args)

    glubort(
      "`mold$clean()` must have the following arguments: {required_clean_args}."
    )
  }

  required_process_args <- c("engine", "x", "y")
  actual_process_args <- rlang::fn_fmls_names(mold$process)

  if (!identical(required_process_args, actual_process_args)) {
    required_process_args <- glue_quote_collapse(required_process_args)

    glubort(
      "`mold$process()` must have the following arguments: {required_process_args}."
    )
  }

  invisible(mold)
}

# ------------------------------------------------------------------------------

check_forge <- function(forge) {

  validate_has_function_set_structure(forge)

  if (is.null(forge$clean)) {
    forge$clean <- get_default_forge_clean()
  }

  forge
}

get_default_forge_clean <- function() {

  function(engine, new_data) {
    list(
      engine = engine,
      new_data
    )
  }

}

validate_forge_args <- function(forge) {

  required_args <- c("engine", "new_data")

  actual_clean_args <- rlang::fn_fmls_names(forge$clean)

  if (!identical(actual_clean_args, required_args)) {
    required_args <- glue_quote_collapse(required_args)

    glubort(
      "`forge$clean()` must have the following arguments: {required_args}."
    )
  }

  actual_process_args <- rlang::fn_fmls_names(forge$process)

  if (!identical(required_args, actual_process_args)) {
    required_args <- glue_quote_collapse(required_args)

    glubort(
      "`forge$process()` must have the following arguments: {required_args}."
    )
  }

  invisible(forge)
}
