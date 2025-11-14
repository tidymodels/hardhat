# Is `x` a frequency weights vector?

**\[experimental\]**

`is_frequency_weights()` checks if `x` inherits from
`"hardhat_frequency_weights"`.

## Usage

``` r
is_frequency_weights(x)
```

## Arguments

- x:

  An object.

## Value

A single `TRUE` or `FALSE`.

## Examples

``` r
is_frequency_weights(1)
#> [1] FALSE
is_frequency_weights(frequency_weights(1))
#> [1] TRUE
is_frequency_weights(importance_weights(1))
#> [1] FALSE
```
