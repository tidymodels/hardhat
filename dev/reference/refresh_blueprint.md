# Refresh a preprocessing blueprint

`refresh_blueprint()` is a developer facing generic function that is
called at the end of
[`update_blueprint()`](https://hardhat.tidymodels.org/dev/reference/update_blueprint.md).
It simply is a wrapper around the method specific `new_*_blueprint()`
function that runs the updated blueprint through the constructor again
to ensure that all of the elements of the blueprint are still valid
after the update.

## Usage

``` r
refresh_blueprint(blueprint)
```

## Arguments

- blueprint:

  A preprocessing blueprint.

## Value

`blueprint` is returned after a call to the corresponding constructor.

## Details

If you implement your own custom `blueprint`, you should export a
`refresh_blueprint()` method that just calls the constructor for your
blueprint and passes through all of the elements of the blueprint to the
constructor.

## Examples

``` r
blueprint <- default_xy_blueprint()

# This should never be done manually, but is essentially
# what `update_blueprint(blueprint, intercept = TRUE)` does for you
blueprint$intercept <- TRUE

# Then update_blueprint() will call refresh_blueprint()
# to ensure that the structure is correct
refresh_blueprint(blueprint)
#> XY blueprint:
#> # Predictors: 0
#> # Outcomes: 0
#> Intercept: TRUE
#> Novel Levels: FALSE
#> Composition: tibble
#> 

# So you can't do something like...
blueprint_bad <- blueprint
blueprint_bad$intercept <- 1

# ...because the constructor will catch it
try(refresh_blueprint(blueprint_bad))
#> Error in new_blueprint(intercept = intercept, allow_novel_levels = allow_novel_levels,  : 
#>   `intercept` must be `TRUE` or `FALSE`, not the number 1.

# And update_blueprint() catches this automatically
try(update_blueprint(blueprint, intercept = 1))
#> Error in new_blueprint(intercept = intercept, allow_novel_levels = allow_novel_levels,  : 
#>   `intercept` must be `TRUE` or `FALSE`, not the number 1.
```
