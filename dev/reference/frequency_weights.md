# Frequency weights

**\[experimental\]**

`frequency_weights()` creates a vector of frequency weights which allow
you to compactly repeat an observation a set number of times. Frequency
weights are supplied as a non-negative integer vector, where only whole
numbers are allowed.

## Usage

``` r
frequency_weights(x)
```

## Arguments

- x:

  An integer vector.

## Value

A new frequency weights vector.

## Details

Frequency weights are integers that denote how many times a particular
row of the data has been observed. They help compress redundant rows
into a single entry.

In tidymodels, frequency weights are used for all parts of the
preprocessing, model fitting, and performance estimation operations.

## See also

[`importance_weights()`](https://hardhat.tidymodels.org/dev/reference/importance_weights.md)

## Examples

``` r
# Record that the first observation has 10 replicates, the second has 12
# replicates, and so on
frequency_weights(c(10, 12, 2, 1))
#> <frequency_weights[4]>
#> [1] 10 12  2  1

# Fractional values are not allowed
try(frequency_weights(c(1.5, 2.3, 10)))
#> Error in frequency_weights(c(1.5, 2.3, 10)) : 
#>   Can't convert from `x` <double> to <integer> due to loss of precision.
#> • Locations: 1, 2
```
