# Standardize the outcome

Most of the time, the input to a model should be flexible enough to
capture a number of different input types from the user. `standardize()`
focuses on capturing the flexibility in the *outcome*.

## Usage

``` r
standardize(y)
```

## Arguments

- y:

  The outcome. This can be:

  - A factor vector

  - A numeric vector

  - A 1D numeric array

  - A numeric matrix with column names

  - A 2D numeric array with column names

  - A data frame with numeric or factor columns

## Value

All possible values of `y` are transformed into a `tibble` for
standardization. Vectors are transformed into a `tibble` with a single
column named `".outcome"`.

## Details

`standardize()` is called from
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) when
using an XY interface (i.e. a `y` argument was supplied).

## Examples

``` r
standardize(1:5)
#> # A tibble: 5 × 1
#>   .outcome
#>      <int>
#> 1        1
#> 2        2
#> 3        3
#> 4        4
#> 5        5

standardize(factor(letters[1:5]))
#> # A tibble: 5 × 1
#>   .outcome
#>   <fct>   
#> 1 a       
#> 2 b       
#> 3 c       
#> 4 d       
#> 5 e       

mat <- matrix(1:10, ncol = 2)
colnames(mat) <- c("a", "b")
standardize(mat)
#> # A tibble: 5 × 2
#>       a     b
#>   <int> <int>
#> 1     1     6
#> 2     2     7
#> 3     3     8
#> 4     4     9
#> 5     5    10

df <- data.frame(x = 1:5, y = 6:10)
standardize(df)
#> # A tibble: 5 × 2
#>       x     y
#>   <int> <int>
#> 1     1     6
#> 2     2     7
#> 3     3     8
#> 4     4     9
#> 5     5    10
```
