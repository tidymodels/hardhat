# `mold()` according to a blueprint

This is a developer facing function that is *only* used if you are
creating your own blueprint subclass. It is called from
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) and
dispatches off the S3 class of the `blueprint`. This gives you an
opportunity to mold the data in a way that is specific to your
blueprint.

`run_mold()` will be called with different arguments depending on the
interface to
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) that is
used:

- XY interface:

  - `run_mold(blueprint, x = x, y = y)`

- Formula interface:

  - `run_mold(blueprint, data = data)`

  - Additionally, the `blueprint` will have been updated to contain the
    `formula`.

- Recipe interface:

  - `run_mold(blueprint, data = data)`

  - Additionally, the `blueprint` will have been updated to contain the
    `recipe`.

If you write a blueprint subclass for
[`new_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
[`new_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
or
[`new_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
then your `run_mold()` method signature must match whichever interface
listed above will be used.

If you write a completely new blueprint inheriting only from
[`new_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
and write a new
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) method
(because you aren't using an xy, formula, or recipe interface), then you
will have full control over how `run_mold()` will be called.

## Usage

``` r
run_mold(blueprint, ...)

# S3 method for class 'default_formula_blueprint'
run_mold(blueprint, ..., data, call = caller_env())

# S3 method for class 'default_recipe_blueprint'
run_mold(blueprint, ..., data, call = caller_env())

# S3 method for class 'default_xy_blueprint'
run_mold(blueprint, ..., x, y, call = caller_env())
```

## Arguments

- blueprint:

  A preprocessing blueprint.

- ...:

  Not used. Required for extensibility.

- data:

  A data frame or matrix containing the outcomes and predictors.

- call:

  The call used for errors and warnings.

- x:

  A data frame or matrix containing the predictors.

- y:

  A data frame, matrix, or vector containing the outcomes.

## Value

`run_mold()` methods return the object that is then immediately returned
from [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).
See the return value section of
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) to
understand what the structure of the return value should look like.

## Examples

``` r
bp <- default_xy_blueprint()

outcomes <- mtcars["mpg"]
predictors <- mtcars
predictors$mpg <- NULL

run_mold(bp, x = predictors, y = outcomes)
#> $predictors
#> # A tibble: 32 × 10
#>      cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2     6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows
#> 
#> $outcomes
#> # A tibble: 32 × 1
#>      mpg
#>    <dbl>
#>  1  21  
#>  2  21  
#>  3  22.8
#>  4  21.4
#>  5  18.7
#>  6  18.1
#>  7  14.3
#>  8  24.4
#>  9  22.8
#> 10  19.2
#> # ℹ 22 more rows
#> 
#> $blueprint
#> XY blueprint:
#> # Predictors: 10
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
#> 
#> $extras
#> NULL
#> 
```
