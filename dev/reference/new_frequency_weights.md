# Construct a frequency weights vector

**\[experimental\]**

`new_frequency_weights()` is a developer oriented function for
constructing a new frequency weights vector. Generally, you should use
[`frequency_weights()`](https://hardhat.tidymodels.org/dev/reference/frequency_weights.md)
instead.

## Usage

``` r
new_frequency_weights(x = integer(), ..., class = character())
```

## Arguments

- x:

  An integer vector.

- ...:

  Name-value pairs defining attributes

- class:

  Name of subclass.

## Value

A new frequency weights vector.

## Examples

``` r
new_frequency_weights()
#> <frequency_weights[0]>
new_frequency_weights(1:5)
#> <frequency_weights[5]>
#> [1] 1 2 3 4 5
```
