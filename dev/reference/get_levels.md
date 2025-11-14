# Extract factor levels from a data frame

`get_levels()` extracts the levels from any factor columns in `data`. It
is mainly useful for extracting the original factor levels from the
predictors in the training set. `get_outcome_levels()` is a small
wrapper around `get_levels()` for extracting levels from a factor
outcome that first calls
[`standardize()`](https://hardhat.tidymodels.org/dev/reference/standardize.md)
on `y`.

## Usage

``` r
get_levels(data)

get_outcome_levels(y)
```

## Arguments

- data:

  A data.frame to extract levels from.

- y:

  The outcome. This can be:

  - A factor vector

  - A numeric vector

  - A 1D numeric array

  - A numeric matrix with column names

  - A 2D numeric array with column names

  - A data frame with numeric or factor columns

## Value

A named list with as many elements as there are factor columns in `data`
or `y`. The names are the names of the factor columns, and the values
are character vectors of the levels.

If there are no factor columns, `NULL` is returned.

## See also

[`stats::.getXlevels()`](https://rdrr.io/r/stats/checkMFClasses.html)

## Examples

``` r
# Factor columns are returned with their levels
get_levels(iris)
#> $Species
#> [1] "setosa"     "versicolor" "virginica" 
#> 

# No factor columns
get_levels(mtcars)
#> NULL

# standardize() is first run on `y`
# which converts the input to a data frame
# with an automatically named column, `".outcome"`
get_outcome_levels(y = factor(letters[1:5]))
#> $.outcome
#> [1] "a" "b" "c" "d" "e"
#> 
```
