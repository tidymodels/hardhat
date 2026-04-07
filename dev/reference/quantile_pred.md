# Create a vector containing sets of quantiles

`quantile_pred()` is a special vector class used to efficiently store
predictions from a quantile regression model. It requires the same
quantile levels for each row being predicted.

## Usage

``` r
quantile_pred(values, quantile_levels = double())

is_quantile_pred(x)

extract_quantile_levels(x)

# S3 method for class 'quantile_pred'
as_tibble(x, ..., .rows = NULL, .name_repair = "minimal", rownames = NULL)

# S3 method for class 'quantile_pred'
as.matrix(x, ...)
```

## Arguments

- values:

  A matrix of values. Each column should correspond to one of the
  quantile levels.

- quantile_levels:

  A vector of probabilities corresponding to `values`.

- x:

  An object produced by `quantile_pred()`.

- ...:

  Not currently used.

- .rows, .name_repair, rownames:

  Arguments not used but required by the original S3 method.

## Value

- `quantile_pred()` returns a vector of values associated with the
  quantile levels.

- `extract_quantile_levels()` returns a numeric vector of levels.

- [`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  returns a tibble with rows `".pred_quantile"`, `".quantile_levels"`,
  and `".row"`.

- [`as.matrix()`](https://rdrr.io/r/base/matrix.html) returns an unnamed
  matrix with rows as samples, columns as quantile levels, and entries
  are predictions.

- `is_quantile_pred()` tests for the "quantile_pred" class

## Examples

``` r
.pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))

unclass(.pred_quantile)
#> $quantile_values
#>            [,1]        [,2]       [,3]       [,4]
#> [1,]  0.5060559 -0.47719270 -0.1102855  0.1340882
#> [2,] -0.5747400 -0.99838644 -0.5110095 -0.4906859
#> [3,] -0.5466319 -0.77625389 -0.9111954 -0.4405479
#> [4,] -0.5644520  0.06445882 -0.8371717  0.4595894
#> [5,] -0.8900378  0.95949406  2.4158352 -0.6937202
#> 
#> attr(,"quantile_levels")
#> [1] 0.2 0.4 0.6 0.8

# Access the underlying information
extract_quantile_levels(.pred_quantile)
#> [1] 0.2 0.4 0.6 0.8

# Matrix format
as.matrix(.pred_quantile)
#>            [,1]        [,2]       [,3]       [,4]
#> [1,]  0.5060559 -0.47719270 -0.1102855  0.1340882
#> [2,] -0.5747400 -0.99838644 -0.5110095 -0.4906859
#> [3,] -0.5466319 -0.77625389 -0.9111954 -0.4405479
#> [4,] -0.5644520  0.06445882 -0.8371717  0.4595894
#> [5,] -0.8900378  0.95949406  2.4158352 -0.6937202

# Tidy format
library(tibble)
as_tibble(.pred_quantile)
#> # A tibble: 20 × 3
#>    .pred_quantile .quantile_levels  .row
#>             <dbl>            <dbl> <int>
#>  1         0.506               0.2     1
#>  2        -0.477               0.4     1
#>  3        -0.110               0.6     1
#>  4         0.134               0.8     1
#>  5        -0.575               0.2     2
#>  6        -0.998               0.4     2
#>  7        -0.511               0.6     2
#>  8        -0.491               0.8     2
#>  9        -0.547               0.2     3
#> 10        -0.776               0.4     3
#> 11        -0.911               0.6     3
#> 12        -0.441               0.8     3
#> 13        -0.564               0.2     4
#> 14         0.0645              0.4     4
#> 15        -0.837               0.6     4
#> 16         0.460               0.8     4
#> 17        -0.890               0.2     5
#> 18         0.959               0.4     5
#> 19         2.42                0.6     5
#> 20        -0.694               0.8     5
```
