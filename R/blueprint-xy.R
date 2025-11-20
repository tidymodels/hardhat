#' @rdname new-blueprint
#' @export
new_xy_blueprint <- function(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "tibble",
  ptypes = NULL,
  ...,
  subclass = character()
) {
  new_blueprint(
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

check_xy_blueprint <- function(
  x,
  ...,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_inherits(x, "xy_blueprint", arg = arg, call = call)
}
