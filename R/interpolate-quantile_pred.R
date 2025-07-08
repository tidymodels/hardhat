#' Restrict numeric data to the interval \[lower, upper\]
#'
#' @param x a numeric vector
#' @param lower number, the lower bound
#' @param upper number, the upper bound
#' @param ... unused
#' @export
#'
#' @return An object ot the same type as `x`
#'
#' @keywords internal
snap <- function(x, lower, upper, ...) {
  UseMethod("snap")
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
  values <- as.matrix(x)
  quantile_levels <- extract_quantile_levels(x)
  values <- map(vctrs::vec_chop(values), ~ snap(.x, lower, upper))
  quantile_pred(do.call(rbind, values), quantile_levels = quantile_levels)
}




#' Compute quantiles from a `quantile_pred`
#'
#' While a `hardhat::quantile_pred` describes evaluations for the inverse
#' cummulative distribution function (CDF, sometimes called the "quantile
#' function") at particular quantiles, this is not enough
#' to fully describe the distribution. For example,
#' ```r
#' p <- c(.1, .5, .9)
#' quantile_pred(matrix(qnorm(p), nrow = 1), p)
#' ```
#' encapsulates the 10%, 50%, and 90% quantiles of the standard normal distribution.
#' But, what if we need, say, the 25% and 75% quantiles? This function imputes
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
#' First, by default (`middle = "cubic"`), missing _internal_ quantiles are
#' interpolated using a cubic spline fit to the observed quantiles with
#' `stats::splinefun()`. Second, if cubic interpolation fails (or if
#' `middle = "linear"`), linear interpolation is used via `stats::approx()`.
#' Finally, missing _external_ quantiles (those outside the range of
#' `quantile_levels`) are extrapolated. This is done using a linear fit on the
#' logistic scale to the two closest tail probabilities.
#'
#' This procedure results in sorted quantiles that interpolate/extrapolate
#' smoothly, while also enforcing heavy tails if none are provided.
#'
#' Optionally, the set of quantiles can be constrained to a compact interval
#' using `lower` and/or `upper`.
#'
#'
#' @param x an object of class `quantile_pred`
#' @param probs vector. probabilities at which to evaluate the inverse CDF
#' @param lower number. lower bound for the resulting values
#' @param upper number. upper bound for the resulting values
#' @param middle character.
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
    probs = seq(0, 1, 0.25),
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
  if (is.unsorted(probs)) probs <- sort(probs)
  check_quantile_levels(probs)
  check_number_decimal(lower)
  check_number_decimal(upper)
  if (lower > upper) {
    cli::cli_abort("`lower` must be less than `upper`.")
  }
  middle <- rlang::arg_match(middle)
  snap(impute_quantile_internal(x, probs, middle), lower, upper)
}

impute_quantile_internal <- function(x, tau_out, middle) {
  tau <- extract_quantile_levels(x)
  qvals <- as.matrix(x)
  if (all(tau_out %in% tau) && !anyNA(qvals)) {
    return(qvals[, match(tau_out, tau), drop = FALSE])
  }
  qvals_out <- map(
    vctrs::vec_chop(qvals),
    ~ impute_quantiles_single(.x, tau, tau_out, middle)
  )
  qvals_out <- do.call(rbind, qvals_out)
  qvals_out
}

impute_quantiles_single <- function(qvals, tau, tau_out, middle) {
  qvals_out <- rep(NA, length(tau_out))
  good <- !is.na(qvals)
  if (!any(good)) {
    return(qvals_out)
  }
  qvals <- qvals[good]
  tau <- tau[good]

  # in case we only have one point, and it matches something we wanted
  if (length(good) < 2) {
    matched_one <- tau_out %in% tau
    qvals_out[matched_one] <- qvals[matched_one]
    return(qvals_out)
  }

  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr

  if (middle == "cubic") {
    method <- "cubic"
    result <- tryCatch(
      {
        Q <- stats::splinefun(tau, qvals, method = "hyman")
        quartiles <- Q(c(.25, .5, .75))
      },
      error = function(e) {
        return(NA)
      }
    )
  }
  if (middle == "linear" || any(is.na(result))) {
    method <- "linear"
    quartiles <- stats::approx(tau, qvals, c(.25, .5, .75))$y
  }
  if (any(indm)) {
    qvals_out[indm] <- switch(
      method,
      linear = stats::approx(tau, qvals, tau_out[indm])$y,
      cubic = Q(tau_out[indm])
    )
  }
  if (any(indl) || any(indr)) {
    qv <- data.frame(
      q = c(tau, tau_out[indm]),
      v = c(qvals, qvals_out[indm])
    ) %>%
      dplyr::distinct(q, .keep_all = TRUE) %>%
      dplyr::arrange(q)
  }
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(tau_out[indl], utils::head(qv, 2))
  }
  if (any(indr)) {
    qvals_out[indr] <- tail_extrapolate(tau_out[indr], utils::tail(qv, 2))
  }
  qvals_out
}

logit <- function(p) {
  p <- pmax(pmin(p, 1), 0)
  log(p) - log(1 - p)
}

# extrapolates linearly on the logistic scale using
# the two points nearest the tail
tail_extrapolate <- function(tau_out, qv) {
  if (nrow(qv) == 1L) return(rep(qv$v[1], length(tau_out)))
  x <- logit(qv$q)
  x0 <- logit(tau_out)
  y <- qv$v
  m <- diff(y) / diff(x)
  m * (x0 - x[1]) + y[1]
}
