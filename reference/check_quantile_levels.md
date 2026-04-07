# Check levels of quantiles

Check levels of quantiles

## Usage

``` r
check_quantile_levels(levels, call = rlang::caller_env())
```

## Arguments

- levels:

  The quantile levels.

- call:

  Call shown in the error messages.

## Value

Invisible `TRUE`

## Details

Checks the levels for their data type, range, uniqueness, order and
missingness.
