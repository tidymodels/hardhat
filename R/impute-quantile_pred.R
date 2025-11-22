#' Restrict numeric data to the interval \[lower, upper\]
#'
#' @param x a numeric vector
#' @param lower number, the lower bound
#' @param upper number, the upper bound
#' @param ... unused
#' @export
#'
#' @return An object of the same type as `x`
#'
#' @keywords internal
snap <- function(x, lower, upper, ...) {
  UseMethod("snap")
}

#' @export
snap.default <- function(x, lower, upper, ...) {
  cli::cli_abort(
    "No {.fn snap} method provided for {.obj_type_friendly {x}}."
  )
}

#' @export
snap.numeric <- function(x, lower, upper, ...) {
  rlang::check_dots_empty()
  check_number_decimal(lower)
  check_number_decimal(upper)

  pmin(pmax(x, lower), upper)
}

#' @export
snap.quantile_pred <- function(x, lower, upper, ...) {
  rlang::check_dots_empty()
  if (vec_size(x) == 0) {
    return(x)
  }
  values <- as.matrix(x)
  quantile_levels <- extract_quantile_levels(x)
  values <- map(vctrs::vec_chop(values), ~ snap(.x, lower, upper))
  quantile_pred(do.call(rbind, values), quantile_levels = quantile_levels)
}


#' Impute additional quantiles from a `quantile_pred`
#'
#' While a [quantile_pred] describes evaluations for the inverse
#' cummulative distribution function (CDF, sometimes called the "quantile
#' function") at particular quantile levels, this is not enough
#' to fully describe the distribution. For example,
#' ```r
#' p <- c(.1, .5, .9)
#' quantile_pred(matrix(qnorm(p), nrow = 1), p)
#' ```
#' encapsulates the 10%, 50%, and 90% quantile levels of the standard normal distribution.
#' But, what if we need, say, the 25% and 75% levels? This function imputes
#' them if possible.
#'
#' @details
#' If `probs` is simply a subset of `quantile_levels` that already exist in `x`,
#' then these will be returned (up to numeric error). Small errors are possible
#' due to difficulties matching double vectors.
#'
#' For `probs` that do not exist in `x`, these will be interpolated or
#' extrapolated as needed. The process has 3 steps.
#'
#' First, by default (`middle = "cubic"`), missing _internal_ quantile levels are
#' interpolated using a cubic spline fit to the observed values + quantile
#' levels with
#' [stats::splinefun]. Second, if cubic interpolation fails (or if
#' `middle = "linear"`), linear interpolation is used via [stats::approx].
#' Finally, missing _external_ quantile levels (those outside the range of
#' `quantile_levels`) are extrapolated. This is done using a linear fit on the
#' logistic scale to the two closest tail values.
#'
#' This procedure results in sorted quantiles that interpolate/extrapolate
#' smoothly, while also enforcing heavy tails beyond the range.
#'
#' Optionally, the resulting quantiles can be constrained to a compact interval
#' using `lower` and/or `upper`. This is done after extrapolation, so it may
#' result in multiple quantile levels having the same value (a CDF with a spike).
#'
#'
#' @param x an object of class [quantile_pred]
#' @param probs vector. probabilities at which to evaluate the inverse CDF
#' @param lower number. lower bound for the resulting values
#' @param upper number. upper bound for the resulting values
#' @param middle character. if `middle = 'cubic'` (the default), a cubic
#' spline is used for interpolation where possible; `middle='linear'`
#' interpolates linearly; see Details below.
#'
#' @returns A matrix with `length(probs)` columns and `length(x)` rows. Each
#'   row contains the inverse CDF (quantile function) given by `x`,
#'  extrapolated/interpolated to `probs`.
#' @export
#'
#' @examples
#' p <- c(.1, .5, .9)
#' qp <- quantile_pred(matrix(c(qnorm(p), qexp(p)), nrow = 2, byrow = TRUE), p)
#' impute_quantiles(qp, p)
#' as.matrix(qp) # same as the imputation
#'
#' p1 <- c(.05, .25, .75, .95)
#' impute_quantiles(qp, p1)
#' rbind(qnorm(p1), qexp(p1)) # exact values, for comparison
impute_quantiles <- function(
  x,
  probs,
  lower = -Inf,
  upper = Inf,
  middle = c("cubic", "linear")
) {
  if (!is_quantile_pred(x)) {
    cli::cli_abort(
      "{.arg x} must be a {.cls quantile_pred} object, not
      {.obj_type_friendly {x}}."
    )
  }
  if (length(extract_quantile_levels(x)) < 2) {
    cli::cli_abort(
      "Quantile interpolation is not possible when fewer than 2 quantiles
      are avaliable."
    )
  }
  probs # better error when missing, in older R versions
  if (is.unsorted(probs)) {
    probs <- sort(probs)
  }
  check_quantile_level_values(probs, "probs", call = caller_env())
  check_number_decimal(lower)
  check_number_decimal(upper)
  if (lower >= upper) {
    cli::cli_abort(
      "{.arg lower} ({lower}) must be less than {.arg upper} ({upper})."
    )
  }
  middle <- rlang::arg_match(middle)
  snap(impute_quantile_internal(x, probs, middle), lower, upper)
}

impute_quantile_internal <- function(x, probs_out, middle) {
  probs_in <- extract_quantile_levels(x)
  vals_in <- as.matrix(x)
  if (all(probs_out %in% probs_in) && !anyNA(vals_in)) {
    return(vals_in[, match(probs_out, probs_in), drop = FALSE])
  }
  vals_out <- map(
    vctrs::vec_chop(vals_in),
    ~ impute_quantiles_single(.x, probs_in, probs_out, middle)
  )
  vals_out <- do.call(rbind, vals_out)
  vals_out
}

impute_quantiles_single <- function(vals_in, probs_in, probs_out, middle) {
  vals_out <- rep(NA, length(probs_out))
  good <- !is.na(vals_in)
  if (!any(good)) {
    return(vals_out)
  }
  vals_in <- vals_in[good]
  probs_in <- probs_in[good]

  # in case we only have one point, and it matches something we wanted
  if (length(good) < 2) {
    matched_one <- probs_out %in% probs_in
    vals_out[matched_one] <- vals_in[matched_one]
    return(vals_out)
  }

  below <- probs_out < min(probs_in)
  above <- probs_out > max(probs_in)
  interior <- !below & !above

  if (any(interior)) {
    if (middle == "cubic") {
      result <- tryCatch(
        {
          interp_fun <- stats::splinefun(probs_in, vals_in, method = "hyman")
          quartiles <- interp_fun(c(.25, .5, .75))
        },
        error = function(e) {
          return(NA)
        }
      )
      if (any(is.na(result))) middle <- "linear"
    }
    if (middle == "linear") {
      interp_fun <- function(probs) stats::approx(probs_in, vals_in, probs)$y
      quartiles <- interp_fun(c(.25, .5, .75))
    }
    vals_out[interior] <- interp_fun(probs_out[interior])
  }
  if (any(below) || any(above)) {
    interior_data <- data.frame(
      probs = c(probs_in, probs_out[interior]),
      vals = c(vals_in, vals_out[interior])
    )
    interior_data <- interior_data[vctrs::vec_unique_loc(interior_data$probs), ]
    interior_data <- interior_data[vctrs::vec_order(interior_data$probs), ]
  }
  if (any(below)) {
    left_tail_data <- utils::head(interior_data, 2)
    vals_out[below] <- tail_extrapolate(probs_out[below], left_tail_data)
  }
  if (any(above)) {
    right_tail_data <- utils::tail(interior_data, 2)
    vals_out[above] <- tail_extrapolate(probs_out[above], right_tail_data)
  }
  vals_out
}

logit <- function(p) {
  p <- pmax(pmin(p, 1), 0)
  log(p) - log(1 - p)
}

# extrapolates linearly on the logistic scale using
# the two points nearest the tail
tail_extrapolate <- function(tail_probs, interior) {
  if (nrow(interior) == 1L) {
    return(rep(interior$vals[1], length(tail_probs)))
  }
  x <- logit(interior$probs)
  x0 <- logit(tail_probs)
  y <- interior$vals
  m <- diff(y) / diff(x)
  m * (x0 - x[1]) + y[1]
}
