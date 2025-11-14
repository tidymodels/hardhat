# Update a preprocessing blueprint

`update_blueprint()` is the correct way to alter elements of an existing
`blueprint` object. It has two benefits over just doing
`blueprint$elem <- new_elem`.

- The name you are updating *must* already exist in the blueprint. This
  prevents you from accidentally updating non-existent elements.

- The constructor for the blueprint is automatically run after the
  update by
  [`refresh_blueprint()`](https://hardhat.tidymodels.org/dev/reference/refresh_blueprint.md)
  to ensure that the blueprint is still valid.

## Usage

``` r
update_blueprint(blueprint, ...)
```

## Arguments

- blueprint:

  A preprocessing blueprint.

- ...:

  Name-value pairs of *existing* elements in `blueprint` that should be
  updated.

## Examples

``` r
blueprint <- default_xy_blueprint()

# `intercept` defaults to FALSE
blueprint
#> XY blueprint:
#> # Predictors: 0
#> # Outcomes: 0
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 

update_blueprint(blueprint, intercept = TRUE)
#> XY blueprint:
#> # Predictors: 0
#> # Outcomes: 0
#> Intercept: TRUE
#> Novel Levels: FALSE
#> Composition: tibble
#> 

# Can't update non-existent elements
try(update_blueprint(blueprint, intercpt = TRUE))
#> Error in update_blueprint(blueprint, intercpt = TRUE) : 
#>   All elements of `...` must already exist.
#> ℹ The following fields are new: "intercpt".

# Can't add non-valid elements
try(update_blueprint(blueprint, intercept = 1))
#> Error in new_blueprint(intercept = intercept, allow_novel_levels = allow_novel_levels,  : 
#>   `intercept` must be `TRUE` or `FALSE`, not the number 1.
```
