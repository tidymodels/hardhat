# Forge prediction-ready data

`forge()` applies the transformations requested by the specific
`blueprint` on a set of `new_data`. This `new_data` contains new
predictors (and potentially outcomes) that will be used to generate
predictions.

All blueprints have consistent return values with the others, but each
is unique enough to have its own help page. Click through below to learn
how to use each one in conjunction with `forge()`.

- XY Method -
  [`default_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md)

- Formula Method -
  [`default_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_formula_blueprint.md)

- Recipes Method -
  [`default_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_recipe_blueprint.md)

## Usage

``` r
forge(new_data, blueprint, ..., outcomes = FALSE)
```

## Arguments

- new_data:

  A data frame or matrix of predictors to process. If `outcomes = TRUE`,
  this should also contain the outcomes to process.

- blueprint:

  A preprocessing `blueprint`.

- ...:

  Not used.

- outcomes:

  A logical. Should the outcomes be processed and returned as well?

## Value

A named list with 3 elements:

- `predictors`: A tibble containing the preprocessed `new_data`
  predictors.

- `outcomes`: If `outcomes = TRUE`, a tibble containing the preprocessed
  outcomes found in `new_data`. Otherwise, `NULL`.

- `extras`: Either `NULL` if the blueprint returns no extra information,
  or a named list containing the extra information.

## Details

If the outcomes are present in `new_data`, they can optionally be
processed and returned in the `outcomes` slot of the returned list by
setting `outcomes = TRUE`. This is very useful when doing cross
validation where you need to preprocess the outcomes of a test set
before computing performance.

## Examples

``` r
# See the blueprint specific documentation linked above
# for various ways to call forge with different
# blueprints.

train <- iris[1:100, ]
test <- iris[101:150, ]

# Formula
processed <- mold(
  log(Sepal.Width) ~ Species,
  train,
  blueprint = default_formula_blueprint(indicators = "none")
)

forge(test, processed$blueprint, outcomes = TRUE)
#> $predictors
#> # A tibble: 50 × 1
#>    Species  
#>    <fct>    
#>  1 virginica
#>  2 virginica
#>  3 virginica
#>  4 virginica
#>  5 virginica
#>  6 virginica
#>  7 virginica
#>  8 virginica
#>  9 virginica
#> 10 virginica
#> # ℹ 40 more rows
#> 
#> $outcomes
#> # A tibble: 50 × 1
#>    `log(Sepal.Width)`
#>                 <dbl>
#>  1              1.19 
#>  2              0.993
#>  3              1.10 
#>  4              1.06 
#>  5              1.10 
#>  6              1.10 
#>  7              0.916
#>  8              1.06 
#>  9              0.916
#> 10              1.28 
#> # ℹ 40 more rows
#> 
#> $extras
#> $extras$offset
#> NULL
#> 
#> 
```
