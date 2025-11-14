# Constructor for a base model

A **model** is a *scalar object*, as classified in [Advanced
R](https://adv-r.hadley.nz/s3.html#object-styles). As such, it takes
uniquely named elements in `...` and combines them into a list with a
class of `class`. This entire object represent a single model.

## Usage

``` r
new_model(..., blueprint = default_xy_blueprint(), class = character())
```

## Arguments

- ...:

  Name-value pairs for elements specific to the model defined by
  `class`.

- blueprint:

  A preprocessing `blueprint` returned from a call to
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).

- class:

  A character vector representing the class of the model.

## Value

A new scalar model object, represented as a classed list with named
elements specified in `...`.

## Details

Because every model should have multiple interfaces, including formula
and `recipes` interfaces, all models should have a `blueprint` that can
process new data when
[`predict()`](https://rdrr.io/r/stats/predict.html) is called. The
easiest way to generate an blueprint with all of the information
required at prediction time is to use the one that is returned from a
call to
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).

## Examples

``` r
new_model(
  custom_element = "my-elem",
  blueprint = default_xy_blueprint(),
  class = "custom_model"
)
#> <custom_model>
#> $custom_element
#> [1] "my-elem"
#> 
```
