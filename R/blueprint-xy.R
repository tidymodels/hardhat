#' @rdname new-blueprint
#' @export
new_xy_blueprint <- function(intercept = FALSE,
                             allow_novel_levels = FALSE,
                             composition = "tibble",
                             ptypes = NULL,
                             ...,
                             subclass = character()) {
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

is_xy_blueprint <- function(x) {
  inherits(x, "xy_blueprint")
}

validate_is_xy_blueprint <- function(blueprint) {
  validate_is(blueprint, is_xy_blueprint, "xy_blueprint")
}
