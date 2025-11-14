# Extract a prototype

`extract_ptype()` extracts a tibble with 0 rows from `data`. This
contains all of the required information about column names, classes,
and factor levels that are required to check the structure of new data
at prediction time.

## Usage

``` r
extract_ptype(data, ..., call = current_env())
```

## Arguments

- data:

  A data frame or matrix.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A 0 row slice of `data` after converting it to a tibble.

## Details

`extract_ptype()` is useful when creating a new preprocessing
`blueprint`. It extracts the required information that will be used by
the validation checks at prediction time.

## Examples

``` r
hardhat:::extract_ptype(iris)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: Sepal.Length <dbl>, Sepal.Width <dbl>,
#> #   Petal.Length <dbl>, Petal.Width <dbl>, Species <fct>
```
