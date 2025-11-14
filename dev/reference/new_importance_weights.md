# Construct an importance weights vector

**\[experimental\]**

`new_importance_weights()` is a developer oriented function for
constructing a new importance weights vector. Generally, you should use
[`importance_weights()`](https://hardhat.tidymodels.org/dev/reference/importance_weights.md)
instead.

## Usage

``` r
new_importance_weights(x = double(), ..., class = character())
```

## Arguments

- x:

  A double vector.

- ...:

  Name-value pairs defining attributes

- class:

  Name of subclass.

## Value

A new importance weights vector.

## Examples

``` r
new_importance_weights()
#> <importance_weights[0]>
new_importance_weights(c(1.5, 2.3, 10))
#> <importance_weights[3]>
#> [1]  1.5  2.3 10.0
```
