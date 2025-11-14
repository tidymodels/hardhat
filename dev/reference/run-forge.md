# `forge()` according to a blueprint

This is a developer facing function that is *only* used if you are
creating your own blueprint subclass. It is called from
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) and
dispatches off the S3 class of the `blueprint`. This gives you an
opportunity to forge the new data in a way that is specific to your
blueprint.

`run_forge()` is always called from
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) with
the same arguments, unlike
[`run_mold()`](https://hardhat.tidymodels.org/dev/reference/run-mold.md),
because there aren't different interfaces for calling
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).
`run_forge()` is always called as:

`run_forge(blueprint, new_data = new_data, outcomes = outcomes)`

If you write a blueprint subclass for
[`new_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
[`new_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
[`new_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
or
[`new_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md),
then your `run_forge()` method signature must match this.

## Usage

``` r
run_forge(blueprint, new_data, ..., outcomes = FALSE)

# S3 method for class 'default_formula_blueprint'
run_forge(blueprint, new_data, ..., outcomes = FALSE, call = caller_env())

# S3 method for class 'default_recipe_blueprint'
run_forge(blueprint, new_data, ..., outcomes = FALSE, call = caller_env())

# S3 method for class 'default_xy_blueprint'
run_forge(blueprint, new_data, ..., outcomes = FALSE, call = caller_env())
```

## Arguments

- blueprint:

  A preprocessing `blueprint`.

- new_data:

  A data frame or matrix of predictors to process. If `outcomes = TRUE`,
  this should also contain the outcomes to process.

- ...:

  Not used.

- outcomes:

  A logical. Should the outcomes be processed and returned as well?

- call:

  The call used for errors and warnings.

## Value

`run_forge()` methods return the object that is then immediately
returned from
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md). See
the return value section of
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) to
understand what the structure of the return value should look like.

## Examples

``` r
bp <- default_xy_blueprint()

outcomes <- mtcars["mpg"]
predictors <- mtcars
predictors$mpg <- NULL

mold <- run_mold(bp, x = predictors, y = outcomes)

run_forge(mold$blueprint, new_data = predictors)
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
#> NULL
#> 
#> $extras
#> NULL
#> 
```
