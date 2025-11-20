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
#'   * `is_quantile_pred()` tests for the "quantile_pred" class
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
  values <- list(quantile_values = values)
  new_quantile_pred(values, quantile_levels)
}

new_quantile_pred <- function(
  values = list(quantile_values = matrix(double(), 0L, 0L)),
  quantile_levels = double()
) {
  quantile_levels <- vctrs::vec_cast(quantile_levels, double())
  vctrs::new_rcrd(
    values,
    quantile_levels = quantile_levels,
    class = "quantile_pred"
  )
}


#' @export
#' @rdname quantile_pred
is_quantile_pred <- function(x) {
  inherits(x, "quantile_pred")
}

#' @export
#' @rdname quantile_pred
extract_quantile_levels <- function(x) {
  if (!is_quantile_pred(x)) {
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
  function(x, ..., .rows = NULL, .name_repair = "minimal", rownames = NULL) {
    lvls <- attr(x, "quantile_levels")
    n_samp <- length(x)
    n_quant <- length(lvls)
    tibble::new_tibble(list(
      .pred_quantile = `dim<-`(t(field(x, "quantile_values")), NULL),
      .quantile_levels = rep(lvls, n_samp),
      .row = rep(1:n_samp, each = n_quant)
    ))
  }

#' @export
#' @rdname quantile_pred
as.matrix.quantile_pred <- function(x, ...) {
  field(x, "quantile_values")
}

#' @export
format.quantile_pred <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- 3L
  }
  quantile_levels <- attr(x, "quantile_levels")
  if (length(quantile_levels) == 1L) {
    x <- field(x, "quantile_values")
    dim(x) <- NULL
    out <- signif(x, digits = digits)
  } else {
    m <- median(x, na.rm = TRUE)
    out <- paste0("[", signif(m, digits = digits), "]")
  }
  out
}

#' @export
median.quantile_pred <- function(x, ...) {
  lvls <- attr(x, "quantile_levels")
  vals <- field(x, "quantile_values")
  loc_median <- (abs(lvls - 0.5) < sqrt(.Machine$double.eps))
  if (any(loc_median)) {
    return(vals[, min(which(loc_median))])
  }
  if (length(lvls) < 2 || min(lvls) > 0.5 || max(lvls) < 0.5) {
    return(rep(NA, vctrs::vec_size(x)))
  }
  map_dbl(vec_seq_along(vals), function(row_i) {
    stats::approx(lvls, vals[row_i, ], xout = 0.5)$y
  })
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

#' @export
vec_proxy_compare.quantile_pred <- function(x, ...) {
  # Using a proxy-based lexicographical order doesn't make sense for binary
  # comparison operators. (A partial order could be implemented by directly
  # overriding the binary comparison operators, but would conflict with the
  # lexicographical total order used for sorting.)
  cli::cli_abort(
    "
    `vec_proxy_compare`, `<`, `<=`, `>`, and `>=` are not supported for
    `quantile_pred`s.
  ",
    class = "hardhat_error_comparing_quantile_preds"
  )
}

#' @export
vec_proxy_order.quantile_pred <- function(x, ...) {
  # Like {vctrs}' list treatment, allow for (lexicographical) ordering based on
  # `quantile_pred`s, even though we disallow using this order for binary
  # comparison operators.
  vec_proxy(x)
}

# ------------------------------------------------------------------------------
# ptype-related functions

format_quantile_levels <- function(quantile_levels) {
  # Make sure that we format levels with enough sig figs to detect minor
  # differences. Specifically, format them with enough sig figs that we recover
  # their precise values.
  result <- formatC(quantile_levels, digits = 3)
  for (digits in 4:17) {
    # 17 significant digits should be enough to disambiguate
    imprecise <- as.numeric(result) != quantile_levels
    if (!any(imprecise)) {
      break
    }
    result[imprecise] <- formatC(quantile_levels[imprecise], digits = digits)
  }
  result <- trimws(result)
  result
}

validate_preds_have_same_quantile_levels <- function(
  x,
  y,
  action,
  x_arg,
  y_arg,
  call
) {
  x_quantile_levels <- attr(x, "quantile_levels")
  y_quantile_levels <- attr(y, "quantile_levels")
  if (!identical(x_quantile_levels, y_quantile_levels)) {
    x_formatted_levels <- format_quantile_levels(x_quantile_levels)
    y_formatted_levels <- format_quantile_levels(y_quantile_levels)
    stop_incompatible_type(
      x,
      y,
      action = action,
      x_arg = x_arg,
      y_arg = y_arg,
      details = cli::format_error(c(
        "They have different sets of quantile levels:",
        "*" = '1st set of quantile levels: {x_formatted_levels}',
        "*" = '2nd set of quantile levels: {y_formatted_levels}'
      )),
      call = call
    )
  }
}

#' @export
vec_ptype2.quantile_pred.quantile_pred <-
  function(
    x,
    y,
    ...,
    x_arg = caller_arg(x),
    y_arg = caller_arg(y),
    call = caller_env()
  ) {
    validate_preds_have_same_quantile_levels(
      x,
      y,
      "combine",
      x_arg,
      y_arg,
      call
    )
    field(x, "quantile_values") <- vec_ptype2(
      field(x, "quantile_values"),
      field(y, "quantile_values")
    )
    x
  }

#' @export
vec_cast.quantile_pred.quantile_pred <-
  function(
    x,
    to,
    ...,
    x_arg = caller_arg(x),
    to_arg = "",
    call = caller_env()
  ) {
    validate_preds_have_same_quantile_levels(
      x,
      to,
      "convert",
      x_arg,
      to_arg,
      call
    )
    field(x, "quantile_values") <- vec_cast(
      field(x, "quantile_values"),
      field(to, "quantile_values")
    )
    x
  }

# ------------------------------------------------------------------------------
# Checking functions

check_quantile_pred_inputs <- function(values, levels, call = caller_env()) {
  check_inherits(values, "matrix", call = call)

  num_lvls <- length(levels)

  if (ncol(values) != num_lvls) {
    cli::cli_abort(
      "The number of columns in {.arg values} must be equal to the length of
        {.arg quantile_levels}.",
      call = call
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


# vctrs behaviours --------------------------------------------------------

#' @export
#' @keywords internal
vec_ptype2.quantile_pred.quantile_pred <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = caller_env()
) {
  x_levels <- extract_quantile_levels(x)
  y_levels <- extract_quantile_levels(y)
  if (all(y_levels %in% x_levels)) {
    return(x)
  }
  if (all(x_levels %in% y_levels)) {
    return(y)
  }
  stop_incompatible_type(
    x,
    y,
    x_arg = x_arg,
    y_arg = y_arg,
    details = "`quantile_levels` must be compatible (a superset/subset relation)."
  )
}

#' @export
vec_cast.quantile_pred.quantile_pred <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = ""
) {
  x_lvls <- extract_quantile_levels(x)
  to_lvls <- extract_quantile_levels(to)
  x_in_to <- x_lvls %in% to_lvls
  to_in_x <- to_lvls %in% x_lvls

  old_qdata <- as.matrix(x)[, x_in_to]
  new_qdata <- matrix(NA, nrow = vec_size(x), ncol = length(to_lvls))
  new_qdata[, to_in_x] <- old_qdata
  quantile_pred(new_qdata, quantile_levels = to_lvls)
}


#' @export
#' @method vec_math quantile_pred
vec_math.quantile_pred <- function(.fn, .x, ...) {
  fn <- .fn
  .fn <- getExportedValue("base", .fn)
  if (
    fn %in%
      c("any", "all", "prod", "sum", "cumsum", "cummax", "cummin", "cumprod")
  ) {
    cli::cli_abort(
      "{.fn {fn}} is not a supported operation for {.cls quantile_pred}."
    )
  }
  quantile_levels <- .x %@% "quantile_levels"
  .x <- as.matrix(.x)
  quantile_pred(.fn(.x), quantile_levels)
}

#' @export
#' @method vec_arith quantile_pred
vec_arith.quantile_pred <- function(op, x, y, ...) {
  UseMethod("vec_arith.quantile_pred", y)
}

#' @export
#' @method vec_arith.quantile_pred default
vec_arith.quantile_pred.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.quantile_pred numeric
vec_arith.quantile_pred.numeric <- function(op, x, y, ...) {
  op_fn <- getExportedValue("base", op)
  l <- vctrs::vec_recycle_common(x = x, y = y)
  out <- op_fn(as.matrix(l$x), l$y)
  quantile_pred(out, attr(x, "quantile_levels"))
}

#' @export
#' @method vec_arith.numeric quantile_pred
vec_arith.numeric.quantile_pred <- function(op, x, y, ...) {
  op_fn <- getExportedValue("base", op)
  l <- vctrs::vec_recycle_common(x = x, y = y)
  out <- op_fn(l$x, as.matrix(l$y))
  quantile_pred(out, y %@% "quantile_levels")
}
