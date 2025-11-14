# Is `x` a case weights vector?

**\[experimental\]**

`is_case_weights()` checks if `x` inherits from
`"hardhat_case_weights"`.

## Usage

``` r
is_case_weights(x)
```

## Arguments

- x:

  An object.

## Value

A single `TRUE` or `FALSE`.

## Examples

``` r
is_case_weights(1)
#> [1] FALSE
is_case_weights(frequency_weights(1))
#> [1] TRUE
```
