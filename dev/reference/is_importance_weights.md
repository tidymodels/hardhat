# Is `x` an importance weights vector?

**\[experimental\]**

`is_importance_weights()` checks if `x` inherits from
`"hardhat_importance_weights"`.

## Usage

``` r
is_importance_weights(x)
```

## Arguments

- x:

  An object.

## Value

A single `TRUE` or `FALSE`.

## Examples

``` r
is_importance_weights(1)
#> [1] FALSE
is_importance_weights(frequency_weights(1))
#> [1] FALSE
is_importance_weights(importance_weights(1))
#> [1] TRUE
```
