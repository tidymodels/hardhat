# nocov start - compat-purrr (last updated: rlang 0.2.0)

# This file serves as a reference for compatibility functions for
# purrr. They are not drop-in replacements but allow a similar style
# of programming. This is useful in cases where purrr is too heavy a
# package to depend on. Please find the most recent version in rlang's
# repository.

map <- function(.x, .f, ...) {
  lapply(.x, .f, ...)
}
map_mold <- function(.x, .f, .mold, ...) {
  out <- vapply(.x, .f, .mold, ..., USE.NAMES = FALSE)
  names(out) <- names(.x)
  out
}
map_lgl <- function(.x, .f, ...) {
  map_mold(.x, .f, logical(1), ...)
}
map_int <- function(.x, .f, ...) {
  map_mold(.x, .f, integer(1), ...)
}
map_dbl <- function(.x, .f, ...) {
  map_mold(.x, .f, double(1), ...)
}
map_chr <- function(.x, .f, ...) {
  map_mold(.x, .f, character(1), ...)
}
map_cpl <- function(.x, .f, ...) {
  map_mold(.x, .f, complex(1), ...)
}

walk <- function(.x, .f, ...) {
  map(.x, .f, ...)
  invisible(.x)
}

pluck <- function(.x, .f) {
  map(.x, `[[`, .f)
}
pluck_lgl <- function(.x, .f) {
  map_lgl(.x, `[[`, .f)
}
pluck_int <- function(.x, .f) {
  map_int(.x, `[[`, .f)
}
pluck_dbl <- function(.x, .f) {
  map_dbl(.x, `[[`, .f)
}
pluck_chr <- function(.x, .f) {
  map_chr(.x, `[[`, .f)
}
pluck_cpl <- function(.x, .f) {
  map_cpl(.x, `[[`, .f)
}

map2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}
map2_lgl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "logical")
}
map2_int <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "integer")
}
map2_dbl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "double")
}
map2_chr <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "character")
}
map2_cpl <- function(.x, .y, .f, ...) {
  as.vector(map2(.x, .y, .f, ...), "complex")
}

args_recycle <- function(args) {
  lengths <- map_int(args, length)
  n <- max(lengths)

  stopifnot(all(lengths == 1L | lengths == n))
  to_recycle <- lengths == 1L
  args[to_recycle] <- map(args[to_recycle], function(x) rep.int(x, n))

  args
}

pmap <- function(.l, .f, ...) {
  args <- args_recycle(.l)
  do.call("mapply", c(
    FUN = list(quote(.f)),
    args, MoreArgs = quote(list(...)),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  ))
}

# nocov end
