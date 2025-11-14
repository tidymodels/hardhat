# Create a vector containing sets of quantiles

`quantile_pred()` is a special vector class used to efficiently store
predictions from a quantile regression model. It requires the same
quantile levels for each row being predicted.

## Usage

``` r
quantile_pred(values, quantile_levels = double())

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

## Examples

``` r
.pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))

unclass(.pred_quantile)
#> [[1]]
#> [1] -1.448205  1.102298 -1.167619  1.449496
#> 
#> [[2]]
#> [1]  0.5747557 -0.4755931 -2.1800396 -1.0686427
#> 
#> [[3]]
#> [1] -1.0236557 -0.7094400 -1.3409932 -0.8553646
#> 
#> [[4]]
#> [1] -0.0151383 -0.5012581 -0.2942939 -0.2806230
#> 
#> [[5]]
#> [1] -0.9359486 -1.6290935 -0.4658975 -0.9943401
#> 
#> attr(,"quantile_levels")
#> [1] 0.2 0.4 0.6 0.8

# Access the underlying information
extract_quantile_levels(.pred_quantile)
#> [1] 0.2 0.4 0.6 0.8

# Matrix format
as.matrix(.pred_quantile)
#>            [,1]       [,2]       [,3]       [,4]
#> [1,] -1.4482049  1.1022975 -1.1676193  1.4494963
#> [2,]  0.5747557 -0.4755931 -2.1800396 -1.0686427
#> [3,] -1.0236557 -0.7094400 -1.3409932 -0.8553646
#> [4,] -0.0151383 -0.5012581 -0.2942939 -0.2806230
#> [5,] -0.9359486 -1.6290935 -0.4658975 -0.9943401

# Tidy format
library(tibble)
as_tibble(.pred_quantile)
#> # A tibble: 20 × 3
#>    .pred_quantile .quantile_levels  .row
#>             <dbl>            <dbl> <int>
#>  1        -1.45                0.2     1
#>  2         1.10                0.4     1
#>  3        -1.17                0.6     1
#>  4         1.45                0.8     1
#>  5         0.575               0.2     2
#>  6        -0.476               0.4     2
#>  7        -2.18                0.6     2
#>  8        -1.07                0.8     2
#>  9        -1.02                0.2     3
#> 10        -0.709               0.4     3
#> 11        -1.34                0.6     3
#> 12        -0.855               0.8     3
#> 13        -0.0151              0.2     4
#> 14        -0.501               0.4     4
#> 15        -0.294               0.6     4
#> 16        -0.281               0.8     4
#> 17        -0.936               0.2     5
#> 18        -1.63                0.4     5
#> 19        -0.466               0.6     5
#> 20        -0.994               0.8     5
```
