# Importance weights

**\[experimental\]**

`importance_weights()` creates a vector of importance weights which
allow you to apply a context dependent weight to your observations.
Importance weights are supplied as a non-negative double vector, where
fractional values are allowed.

## Usage

``` r
importance_weights(x)
```

## Arguments

- x:

  A double vector.

## Value

A new importance weights vector.

## Details

Importance weights focus on how much each row of the data set should
influence model estimation. These can be based on data or arbitrarily
set to achieve some goal.

In tidymodels, importance weights only affect the model estimation and
*supervised* recipes steps. They are not used with yardstick functions
for calculating measures of model performance.

## See also

[`frequency_weights()`](https://hardhat.tidymodels.org/dev/reference/frequency_weights.md)

## Examples

``` r
importance_weights(c(1.5, 2.3, 10))
#> <importance_weights[3]>
#> [1]  1.5  2.3 10.0
```
