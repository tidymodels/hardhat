# Impute additional quantiles from a `quantile_pred`

While a
[quantile_pred](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
describes evaluations for the inverse cummulative distribution function
(CDF, sometimes called the "quantile function") at particular quantile
levels, this is not enough to fully describe the distribution. For
example,

    p <- c(.1, .5, .9)
    quantile_pred(matrix(qnorm(p), nrow = 1), p)

encapsulates the 10%, 50%, and 90% quantile levels of the standard
normal distribution. But, what if we need, say, the 25% and 75% levels?
This function imputes them if possible.

## Usage

``` r
impute_quantiles(
  x,
  probs,
  lower = -Inf,
  upper = Inf,
  middle = c("cubic", "linear")
)
```

## Arguments

- x:

  an object of class
  [quantile_pred](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)

- probs:

  vector. probabilities at which to evaluate the inverse CDF

- lower:

  number. lower bound for the resulting values

- upper:

  number. upper bound for the resulting values

- middle:

  character. if `middle = 'cubic'` (the default), a cubic spline is used
  for interpolation where possible; `middle='linear'` interpolates
  linearly; see Details below.

## Value

A matrix with `length(probs)` columns and `length(x)` rows. Each row
contains the inverse CDF (quantile function) given by `x`,
extrapolated/interpolated to `probs`.

## Details

If `probs` is simply a subset of `quantile_levels` that already exist in
`x`, then these will be returned (up to numeric error). Small errors are
possible due to difficulties matching double vectors.

For `probs` that do not exist in `x`, these will be interpolated or
extrapolated as needed. The process has 3 steps.

First, by default (`middle = "cubic"`), missing *internal* quantile
levels are interpolated using a cubic spline fit to the observed
values + quantile levels with
[stats::splinefun](https://rdrr.io/r/stats/splinefun.html). Second, if
cubic interpolation fails (or if `middle = "linear"`), linear
interpolation is used via
[stats::approx](https://rdrr.io/r/stats/approxfun.html). Finally,
missing *external* quantile levels (those outside the range of
`quantile_levels`) are extrapolated. This is done using a linear fit on
the logistic scale to the two closest tail values.

This procedure results in sorted quantiles that interpolate/extrapolate
smoothly, while also enforcing heavy tails beyond the range.

Optionally, the resulting quantiles can be constrained to a compact
interval using `lower` and/or `upper`. This is done after extrapolation,
so it may result in multiple quantile levels having the same value (a
CDF with a spike).

## Examples

``` r
p <- c(.1, .5, .9)
qp <- quantile_pred(matrix(c(qnorm(p), qexp(p)), nrow = 2, byrow = TRUE), p)
impute_quantiles(qp, p)
#>            [,1]      [,2]     [,3]
#> [1,] -1.2815516 0.0000000 1.281552
#> [2,]  0.1053605 0.6931472 2.302585
as.matrix(qp) # same as the imputation
#>            [,1]      [,2]     [,3]
#> [1,] -1.2815516 0.0000000 1.281552
#> [2,]  0.1053605 0.6931472 2.302585

p1 <- c(.05, .25, .75, .95)
impute_quantiles(qp, p1)
#>             [,1]       [,2]      [,3]     [,4]
#> [1,] -1.60841635 -0.8009697 0.8009697 1.608416
#> [2,]  0.03687326  0.2060558 1.5793211 2.794509
rbind(qnorm(p1), qexp(p1)) # exact values, for comparison
#>             [,1]       [,2]      [,3]     [,4]
#> [1,] -1.64485363 -0.6744898 0.6744898 1.644854
#> [2,]  0.05129329  0.2876821 1.3862944 2.995732
```
