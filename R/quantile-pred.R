#' Create a vector containing sets of quantiles
#'
#' `quantile_pred()` is a special vector class used to efficiently store
#' predictions from a quantile regression model. It requires the same quantile
#' levels for each row being predicted.
#'
#' @param values A matrix of values. Each column should correspond to one of
#'   the quantile levels.
#' @param quantile_levels A vector of probabilities corresponding to `values`.
#' @param x An object produced by `quantile_pred()`.
#' @param .rows,.name_repair,rownames Arguments not used but required by the
#' original S3 method.
#' @param ... Not currently used.
#'
#' @export
#' @return
#'   * `quantile_pred()` returns a vector of values associated with the
#' quantile levels.
#'   * `extract_quantile_levels()` returns a numeric vector of levels.
#'   * `as_tibble()` returns a tibble with rows `".pred_quantile"`,
#'   `".quantile_levels"`, and `".row"`.
#'   * `as.matrix()` returns an unnamed matrix with rows as samples, columns as
#'   quantile levels, and entries are predictions.
#' @examples
#' .pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
#'
#' unclass(.pred_quantile)
#'
#' # Access the underlying information
#' extract_quantile_levels(.pred_quantile)
#'
#' # Matrix format
#' as.matrix(.pred_quantile)
#'
#' # Tidy format
#' library(tibble)
#' as_tibble(.pred_quantile)
quantile_pred <- function(values, quantile_levels = double()) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  check_quantile_levels(quantile_levels)
  check_quantile_pred_inputs(values, quantile_levels)

  rownames(values) <- NULL
  colnames(values) <- NULL
  values <- lapply(vctrs::vec_chop(values), drop)
  new_quantile_pred(values, quantile_levels)
}

new_quantile_pred <- function(values = list(), quantile_levels = double()) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  vctrs::new_vctr(
    values, quantile_levels = quantile_levels, class = "quantile_pred"
  )
}

#' @export
#' @rdname quantile_pred
extract_quantile_levels <- function(x) {
  if (!inherits(x, "quantile_pred")) {
    cli::cli_abort(
      "{.arg x} must be a {.cls quantile_pred} object, not
      {.obj_type_friendly {x}}."
    )
  }
  attr(x, "quantile_levels")
}

#' @export
#' @rdname quantile_pred
as_tibble.quantile_pred <-
  function (x, ..., .rows = NULL, .name_repair = "minimal", rownames = NULL) {
    lvls <- attr(x, "quantile_levels")
    n_samp <- length(x)
    n_quant <- length(lvls)
    tibble::new_tibble(list(
      .pred_quantile = unlist(x),
      .quantile_levels = rep(lvls, n_samp),
      .row = rep(1:n_samp, each = n_quant)
    ))
  }

#' @export
#' @rdname quantile_pred
as.matrix.quantile_pred <- function(x, ...) {
  num_samp <- length(x)
  matrix(unlist(x), nrow = num_samp, byrow = TRUE)
}

#' @export
format.quantile_pred <- function(x, digits = 3L, ...) {
  quantile_levels <- attr(x, "quantile_levels")
  if (length(quantile_levels) == 1L) {
    x <- unlist(x)
    out <- signif(x, digits = digits)
    out[is.na(x)] <- NA_real_
  } else {
    m <- median(x, na.rm = TRUE)
    out <- paste0("[", signif(m, digits = digits), "]")
  }
  out
}

#' @export
median.quantile_pred <- function(x, ...) {
  lvls <- attr(x, "quantile_levels")
  loc_median <- (abs(lvls - 0.5) < sqrt(.Machine$double.eps))
  if (any(loc_median)) {
    return(map_dbl(x, ~ .x[min(which(loc_median))]))
  }
  if (length(lvls) < 2 || min(lvls) > 0.5 || max(lvls) < 0.5) {
    return(rep(NA, vctrs::vec_size(x)))
  }
  map_dbl(x, ~ stats::approx(lvls, .x, xout = 0.5)$y)
}

#' @export
vec_ptype_abbr.quantile_pred <- function(x, ...) {
  n_lvls <- length(attr(x, "quantile_levels"))
  cli::format_inline("qtl{?s}({n_lvls})")
}

#' @export
vec_ptype_full.quantile_pred <- function(x, ...) "quantiles"

#' @export
obj_print_footer.quantile_pred <- function(x, digits = 3, ...) {
  lvls <- attr(x, "quantile_levels")
  footer <- cli::format_inline("# Quantile {cli::qty(length(lvls))}level{?s}:")
  cat(footer, format(lvls, digits = digits), "\n", sep = " ")
}


# ------------------------------------------------------------------------------
# Checking functions

check_quantile_pred_inputs <- function(values, levels, call = caller_env()) {
  check_inherits(values, "matrix", call = call)

  num_lvls <- length(levels)

  if (ncol(values) != num_lvls) {
    cli::cli_abort(
      "The number of columns in {.arg values} must be equal to the length of
        {.arg quantile_levels}.", call = call
    )
  }

  invisible(TRUE)
}

#' Check levels of quantiles
#' @param levels The quantile levels.
#' @param call Call shown in the error messages.
#' @return Invisible `TRUE`
#' @keywords internal
#' @details
#' Checks the levels for their data type, range, uniqueness, order and missingness.
#' @export
check_quantile_levels <- function(levels, call = rlang::caller_env()) {
  # data type, range, etc
  check_quantile_level_values(levels, arg = "quantile_levels", call = call)

  # uniqueness
  is_dup <- duplicated(levels)
  if (any(is_dup)) {
    redund <- levels[is_dup]
    redund <- unique(redund)
    redund <- signif(redund, digits = 5)
    cli::cli_abort(
      c(
        "Quantile levels must be unique.",
        i = "The following {cli::qty(length(redund))}value{?s} {?was/were}
            repeated: {redund}."
      ),
      call = call
    )
  }

  # order
  if (is.unsorted(levels)) {
    cli::cli_abort(
      "{.arg quantile_levels} must be sorted in increasing order.",
      call = call
    )
  }

  invisible(TRUE)
}

check_quantile_level_values <- function(levels, arg, call) {
  if (is.null(levels)) {
    cli::cli_abort("{.arg {arg}} cannot be {.val NULL}.", call = call)
  }
  for (val in levels) {
    check_number_decimal(
      val,
      min = 0,
      max = 1,
      arg = arg,
      call = call,
      allow_na = FALSE,
      allow_null = FALSE,
      allow_infinite = FALSE
    )
  }
  invisible(TRUE)
}
