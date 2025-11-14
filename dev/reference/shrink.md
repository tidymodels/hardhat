# Subset only required columns

`shrink()` subsets `data` to only contain the required columns specified
by the prototype, `ptype`.

## Usage

``` r
shrink(data, ptype, ..., call = current_env())
```

## Arguments

- data:

  A data frame containing the data to subset.

- ptype:

  A data frame prototype containing the required columns.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A tibble containing the required columns.

## Details

`shrink()` is called by
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
before
[`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md) and
before the actual processing is done.

## Examples

``` r
# ---------------------------------------------------------------------------
# Setup

train <- iris[1:100, ]
test <- iris[101:150, ]

# ---------------------------------------------------------------------------
# shrink()

# mold() is run at model fit time
# and a formula preprocessing blueprint is recorded
x <- mold(log(Sepal.Width) ~ Species, train)

# Inside the result of mold() are the prototype tibbles
# for the predictors and the outcomes
ptype_pred <- x$blueprint$ptypes$predictors
ptype_out <- x$blueprint$ptypes$outcomes

# Pass the test data, along with a prototype, to
# shrink() to extract the prototype columns
shrink(test, ptype_pred)
#> # A tibble: 50 × 1
#>    Species  
#>    <fct>    
#>  1 virginica
#>  2 virginica
#>  3 virginica
#>  4 virginica
#>  5 virginica
#>  6 virginica
#>  7 virginica
#>  8 virginica
#>  9 virginica
#> 10 virginica
#> # ℹ 40 more rows

# To extract the outcomes, just use the
# outcome prototype
shrink(test, ptype_out)
#> # A tibble: 50 × 1
#>    Sepal.Width
#>          <dbl>
#>  1         3.3
#>  2         2.7
#>  3         3  
#>  4         2.9
#>  5         3  
#>  6         3  
#>  7         2.5
#>  8         2.9
#>  9         2.5
#> 10         3.6
#> # ℹ 40 more rows

# shrink() makes sure that the columns
# required by `ptype` actually exist in the data
# and errors nicely when they don't
test2 <- subset(test, select = -Species)
try(shrink(test2, ptype_pred))
#> Error in shrink(test2, ptype_pred) : 
#>   The required column "Species" is missing.
```
