#' @rdname new-blueprint
#' @export
new_xy_blueprint <- function(mold,
                          forge,
                          intercept = FALSE,
                          ptypes = NULL,
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
    required_clean_args = c("blueprint", "x", "y"),
    required_process_args = c("blueprint", "x", "y")
  )

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    ptypes = ptypes,
    ...,
    subclass = c(subclass, "xy_blueprint")
  )

}

#' @export
refresh_blueprint.xy_blueprint <- function(blueprint) {
  do.call(new_xy_blueprint, as.list(blueprint))
}

is_xy_blueprint <- function(x) {
  inherits(x, "xy_blueprint")
}

validate_is_xy_blueprint <- function(blueprint) {
  validate_is(blueprint, is_xy_blueprint, "xy_blueprint")
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

  function(blueprint, x, y) {
    list(
      blueprint = blueprint,
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

  function(blueprint, new_data) {
    list(
      blueprint = blueprint,
      new_data
    )
  }

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
