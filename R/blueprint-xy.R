#' @rdname new-blueprint
#' @export
new_xy_blueprint <- function(mold,
                             forge,
                             intercept = FALSE,
                             allow_novel_levels = FALSE,
                             composition = "tibble",
                             ptypes = NULL,
                             ...,
                             subclass = character()) {

  validate_is_function_set(mold)
  validate_mold_args(
    mold = mold,
    required_clean_args = c("blueprint", "x", "y"),
    required_process_args = c("blueprint", "x", "y")
  )

  new_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    composition = composition,
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
