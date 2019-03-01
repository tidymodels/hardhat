#' @rdname new-engine
#' @export
new_xy_engine <- function(mold,
                          forge,
                          intercept = FALSE,
                          info = NULL,
                          ...,
                          subclass = character()) {

  if (rlang::is_missing(mold)) {
    abort_no_mold()
  }

  if (rlang::is_missing(forge)) {
    abort_no_forge()
  }

  mold <- check_mold_xy(mold)
  forge <- check_forge(forge)

  validate_mold_args(
    mold = mold,
    required_clean_args = c("engine", "x", "y"),
    required_process_args = c("engine", "x", "y")
  )

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    ...,
    subclass = c(subclass, "xy_engine")
  )

}

#' @export
refresh_engine.xy_engine <- function(engine) {
  do.call(new_xy_engine, as.list(engine))
}

is_xy_engine <- function(x) {
  inherits(x, "xy_engine")
}

validate_is_xy_engine <- function(engine) {
  validate_is(engine, is_xy_engine, "xy_engine")
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

validate_mold_args <- function(mold, required_clean_args, required_process_args) {

  actual_clean_args <- rlang::fn_fmls_names(mold$clean)

  if (!identical(actual_clean_args, required_clean_args)) {
    required_clean_args <- glue_quote_collapse(required_clean_args)

    glubort(
      "`mold$clean()` must have the following arguments: {required_clean_args}."
    )
  }

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

  required_args <- c("engine", "new_data", "outcomes")

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

abort_no_mold <- function() {
  glubort(
    "`mold` must be supplied in the form: ",
    "list(clean = <function>, process = <function>)."
  )
}

abort_no_forge <- function() {
  glubort(
    "`forge` must be supplied in the form: ",
    "list(clean = <function>, process = <function>)."
  )
}
