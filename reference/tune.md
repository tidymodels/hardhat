# Mark arguments for tuning

`tune()` is an argument placeholder to be used with the recipes,
parsnip, and tune packages. It marks recipes step and parsnip model
arguments for tuning.

## Usage

``` r
tune(id = "")
```

## Arguments

- id:

  A single character value that can be used to differentiate parameters
  that are used in multiple places but have the same name, or if the
  user wants to add a note to the specified parameter.

## Value

A call object that echos the user's input.

## See also

`tune::tune_grid()`, `tune::tune_bayes()`

## Examples

``` r
tune()
#> tune()
tune("your name here")
#> tune("your name here")

# In practice, `tune()` is used alongside recipes or parsnip to mark
# specific arguments for tuning
library(recipes)

recipe(mpg ~ ., data = mtcars) |>
  step_normalize(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors, num_comp = tune())
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:    1
#> predictor: 10
#> 
#> ── Operations 
#> • Centering and scaling for: all_numeric_predictors()
#> • PCA extraction with: all_numeric_predictors
```
