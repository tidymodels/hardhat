# Mold data for modeling

`mold()` applies the appropriate processing steps required to get
training data ready to be fed into a model. It does this through the use
of various *blueprints* that understand how to preprocess data that come
in various forms, such as a formula or a recipe.

All blueprints have consistent return values with the others, but each
is unique enough to have its own help page. Click through below to learn
how to use each one in conjunction with `mold()`.

- XY Method -
  [`default_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md)

- Formula Method -
  [`default_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_formula_blueprint.md)

- Recipes Method -
  [`default_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_recipe_blueprint.md)

## Usage

``` r
mold(x, ...)
```

## Arguments

- x:

  An object. See the method specific implementations linked in the
  Description for more information.

- ...:

  Not used.

## Value

A named list containing 4 elements:

- `predictors`: A tibble containing the molded predictors to be used in
  the model.

- `outcomes`: A tibble containing the molded outcomes to be used in the
  model.

- `blueprint`: A method specific `"hardhat_blueprint"` object for use
  when making predictions.

- `extras`: Either `NULL` if the blueprint returns no extra information,
  or a named list containing the extra information.

## Examples

``` r
# See the method specific documentation linked in Description
# for the details of each blueprint, and more examples.

# XY
mold(iris["Sepal.Width"], iris$Species)
#> $predictors
#> # A tibble: 150 × 1
#>    Sepal.Width
#>          <dbl>
#>  1         3.5
#>  2         3  
#>  3         3.2
#>  4         3.1
#>  5         3.6
#>  6         3.9
#>  7         3.4
#>  8         3.4
#>  9         2.9
#> 10         3.1
#> # ℹ 140 more rows
#> 
#> $outcomes
#> # A tibble: 150 × 1
#>    .outcome
#>    <fct>   
#>  1 setosa  
#>  2 setosa  
#>  3 setosa  
#>  4 setosa  
#>  5 setosa  
#>  6 setosa  
#>  7 setosa  
#>  8 setosa  
#>  9 setosa  
#> 10 setosa  
#> # ℹ 140 more rows
#> 
#> $blueprint
#> XY blueprint:
#> # Predictors: 1
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
#> 
#> $extras
#> NULL
#> 

# Formula
mold(Species ~ Sepal.Width, iris)
#> $predictors
#> # A tibble: 150 × 1
#>    Sepal.Width
#>          <dbl>
#>  1         3.5
#>  2         3  
#>  3         3.2
#>  4         3.1
#>  5         3.6
#>  6         3.9
#>  7         3.4
#>  8         3.4
#>  9         2.9
#> 10         3.1
#> # ℹ 140 more rows
#> 
#> $outcomes
#> # A tibble: 150 × 1
#>    Species
#>    <fct>  
#>  1 setosa 
#>  2 setosa 
#>  3 setosa 
#>  4 setosa 
#>  5 setosa 
#>  6 setosa 
#>  7 setosa 
#>  8 setosa 
#>  9 setosa 
#> 10 setosa 
#> # ℹ 140 more rows
#> 
#> $blueprint
#> Formula blueprint:
#> # Predictors: 1
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> Indicators: traditional
#> 
#> 
#> $extras
#> $extras$offset
#> NULL
#> 
#> 

# Recipe
library(recipes)
mold(recipe(Species ~ Sepal.Width, iris), iris)
#> $predictors
#> # A tibble: 150 × 1
#>    Sepal.Width
#>          <dbl>
#>  1         3.5
#>  2         3  
#>  3         3.2
#>  4         3.1
#>  5         3.6
#>  6         3.9
#>  7         3.4
#>  8         3.4
#>  9         2.9
#> 10         3.1
#> # ℹ 140 more rows
#> 
#> $outcomes
#> # A tibble: 150 × 1
#>    Species
#>    <fct>  
#>  1 setosa 
#>  2 setosa 
#>  3 setosa 
#>  4 setosa 
#>  5 setosa 
#>  6 setosa 
#>  7 setosa 
#>  8 setosa 
#>  9 setosa 
#> 10 setosa 
#> # ℹ 140 more rows
#> 
#> $blueprint
#> Recipe blueprint:
#> # Predictors: 1
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
#> 
#> $extras
#> $extras$roles
#> NULL
#> 
#> 
```
