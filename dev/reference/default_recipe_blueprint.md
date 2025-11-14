# Default recipe blueprint

This pages holds the details for the recipe preprocessing blueprint.
This is the blueprint used by default from
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) if `x`
is a recipe.

## Usage

``` r
default_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  strings_as_factors = TRUE,
  composition = "tibble"
)

# S3 method for class 'recipe'
mold(x, data, ..., blueprint = NULL)
```

## Arguments

- intercept:

  A logical. Should an intercept be included in the processed data? This
  information is used by the `process` function in the `mold` and
  `forge` function list.

- allow_novel_levels:

  A logical. Should novel factor levels be allowed at prediction time?
  This information is used by the `clean` function in the `forge`
  function list, and is passed on to
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md).

- fresh:

  Should already trained operations be re-trained when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  called?

- strings_as_factors:

  Should character columns be converted to factors when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  called?

- composition:

  Either "tibble", "matrix", or "dgCMatrix" for the format of the
  processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
  the predictors must be numeric after the preprocessing method has been
  applied; otherwise an error is thrown.

- x:

  An unprepped recipe created from
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).

- data:

  A data frame or matrix containing the outcomes and predictors.

- ...:

  Not used.

- blueprint:

  A preprocessing `blueprint`. If left as `NULL`, then a
  `default_recipe_blueprint()` is used.

## Value

For `default_recipe_blueprint()`, a recipe blueprint.

## Mold

When [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
used with the default recipe blueprint:

- It calls
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  to prep the recipe.

- It calls
  [`recipes::juice()`](https://recipes.tidymodels.org/reference/juice.html)
  to extract the outcomes and predictors. These are returned as tibbles.

- If `intercept = TRUE`, adds an intercept column to the predictors.

## Forge

When [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
is used with the default recipe blueprint:

- It calls
  [`shrink()`](https://hardhat.tidymodels.org/dev/reference/shrink.md)
  to trim `new_data` to only the required columns and coerce `new_data`
  to a tibble.

- It calls
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md)
  to perform validation on the structure of the columns of `new_data`.

- It calls
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)
  on the `new_data` using the prepped recipe used during training.

- It adds an intercept column onto `new_data` if `intercept = TRUE`.

## Examples

``` r
# example code

library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step

# ---------------------------------------------------------------------------
# Setup

train <- iris[1:100, ]
test <- iris[101:150, ]

# ---------------------------------------------------------------------------
# Recipes example

# Create a recipe that logs a predictor
rec <- recipe(Species ~ Sepal.Length + Sepal.Width, train) |>
  step_log(Sepal.Length)

processed <- mold(rec, train)

# Sepal.Length has been logged
processed$predictors
#> # A tibble: 100 × 2
#>    Sepal.Length Sepal.Width
#>           <dbl>       <dbl>
#>  1         1.63         3.5
#>  2         1.59         3  
#>  3         1.55         3.2
#>  4         1.53         3.1
#>  5         1.61         3.6
#>  6         1.69         3.9
#>  7         1.53         3.4
#>  8         1.61         3.4
#>  9         1.48         2.9
#> 10         1.59         3.1
#> # ℹ 90 more rows

processed$outcomes
#> # A tibble: 100 × 1
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
#> # ℹ 90 more rows

# The underlying blueprint is a prepped recipe
processed$blueprint$recipe
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 2
#> 
#> ── Training information 
#> Training data contained 100 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Log transformation on: Sepal.Length | Trained

# Call forge() with the blueprint and the test data
# to have it preprocess the test data in the same way
forge(test, processed$blueprint)
#> $predictors
#> # A tibble: 50 × 2
#>    Sepal.Length Sepal.Width
#>           <dbl>       <dbl>
#>  1         1.84         3.3
#>  2         1.76         2.7
#>  3         1.96         3  
#>  4         1.84         2.9
#>  5         1.87         3  
#>  6         2.03         3  
#>  7         1.59         2.5
#>  8         1.99         2.9
#>  9         1.90         2.5
#> 10         1.97         3.6
#> # ℹ 40 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$roles
#> NULL
#> 
#> 

# Use `outcomes = TRUE` to also extract the preprocessed outcome!
# This logged the Sepal.Length column of `new_data`
forge(test, processed$blueprint, outcomes = TRUE)
#> $predictors
#> # A tibble: 50 × 2
#>    Sepal.Length Sepal.Width
#>           <dbl>       <dbl>
#>  1         1.84         3.3
#>  2         1.76         2.7
#>  3         1.96         3  
#>  4         1.84         2.9
#>  5         1.87         3  
#>  6         2.03         3  
#>  7         1.59         2.5
#>  8         1.99         2.9
#>  9         1.90         2.5
#> 10         1.97         3.6
#> # ℹ 40 more rows
#> 
#> $outcomes
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
#> $extras
#> $extras$roles
#> NULL
#> 
#> 

# ---------------------------------------------------------------------------
# With an intercept

# You can add an intercept with `intercept = TRUE`
processed <- mold(rec, train, blueprint = default_recipe_blueprint(intercept = TRUE))

processed$predictors
#> # A tibble: 100 × 3
#>    `(Intercept)` Sepal.Length Sepal.Width
#>            <int>        <dbl>       <dbl>
#>  1             1         1.63         3.5
#>  2             1         1.59         3  
#>  3             1         1.55         3.2
#>  4             1         1.53         3.1
#>  5             1         1.61         3.6
#>  6             1         1.69         3.9
#>  7             1         1.53         3.4
#>  8             1         1.61         3.4
#>  9             1         1.48         2.9
#> 10             1         1.59         3.1
#> # ℹ 90 more rows

# But you also could have used a recipe step
rec2 <- step_intercept(rec)

mold(rec2, iris)$predictors
#> # A tibble: 150 × 3
#>    intercept Sepal.Length Sepal.Width
#>        <int>        <dbl>       <dbl>
#>  1         1         1.63         3.5
#>  2         1         1.59         3  
#>  3         1         1.55         3.2
#>  4         1         1.53         3.1
#>  5         1         1.61         3.6
#>  6         1         1.69         3.9
#>  7         1         1.53         3.4
#>  8         1         1.61         3.4
#>  9         1         1.48         2.9
#> 10         1         1.59         3.1
#> # ℹ 140 more rows

# ---------------------------------------------------------------------------
# Matrix output for predictors

# You can change the `composition` of the predictor data set
bp <- default_recipe_blueprint(composition = "dgCMatrix")
processed <- mold(rec, train, blueprint = bp)
class(processed$predictors)
#> [1] "dgCMatrix"
#> attr(,"package")
#> [1] "Matrix"

# ---------------------------------------------------------------------------
# Non standard roles

# If you have custom recipes roles, they are assumed to be required at
# `bake()` time when passing in `new_data`. This is an assumption that both
# recipes and hardhat makes, meaning that those roles are required at
# `forge()` time as well.
rec_roles <- recipe(train) |>
  update_role(Sepal.Width, new_role = "predictor") |>
  update_role(Species, new_role = "outcome") |>
  update_role(Sepal.Length, new_role = "id") |>
  update_role(Petal.Length, new_role = "important")

processed_roles <- mold(rec_roles, train)

# The custom roles will be in the `mold()` result in case you need
# them for modeling.
processed_roles$extras
#> $roles
#> $roles$id
#> # A tibble: 100 × 1
#>    Sepal.Length
#>           <dbl>
#>  1          5.1
#>  2          4.9
#>  3          4.7
#>  4          4.6
#>  5          5  
#>  6          5.4
#>  7          4.6
#>  8          5  
#>  9          4.4
#> 10          4.9
#> # ℹ 90 more rows
#> 
#> $roles$important
#> # A tibble: 100 × 1
#>    Petal.Length
#>           <dbl>
#>  1          1.4
#>  2          1.4
#>  3          1.3
#>  4          1.5
#>  5          1.4
#>  6          1.7
#>  7          1.4
#>  8          1.5
#>  9          1.4
#> 10          1.5
#> # ℹ 90 more rows
#> 
#> $roles$`NA`
#> # A tibble: 100 × 1
#>    Petal.Width
#>          <dbl>
#>  1         0.2
#>  2         0.2
#>  3         0.2
#>  4         0.2
#>  5         0.2
#>  6         0.4
#>  7         0.3
#>  8         0.2
#>  9         0.2
#> 10         0.1
#> # ℹ 90 more rows
#> 
#> 

# And they are in the `forge()` result
forge(test, processed_roles$blueprint)$extras
#> $roles
#> $roles$id
#> # A tibble: 50 × 1
#>    Sepal.Length
#>           <dbl>
#>  1          6.3
#>  2          5.8
#>  3          7.1
#>  4          6.3
#>  5          6.5
#>  6          7.6
#>  7          4.9
#>  8          7.3
#>  9          6.7
#> 10          7.2
#> # ℹ 40 more rows
#> 
#> $roles$important
#> # A tibble: 50 × 1
#>    Petal.Length
#>           <dbl>
#>  1          6  
#>  2          5.1
#>  3          5.9
#>  4          5.6
#>  5          5.8
#>  6          6.6
#>  7          4.5
#>  8          6.3
#>  9          5.8
#> 10          6.1
#> # ℹ 40 more rows
#> 
#> $roles$`NA`
#> # A tibble: 50 × 1
#>    Petal.Width
#>          <dbl>
#>  1         2.5
#>  2         1.9
#>  3         2.1
#>  4         1.8
#>  5         2.2
#>  6         2.1
#>  7         1.7
#>  8         1.8
#>  9         1.8
#> 10         2.5
#> # ℹ 40 more rows
#> 
#> 

# If you remove a column with a custom role from the test data, then you
# won't be able to `forge()` even though this recipe technically didn't
# use that column in any steps
test2 <- test
test2$Petal.Length <- NULL
try(forge(test2, processed_roles$blueprint))
#> Error in forge(test2, processed_roles$blueprint) : 
#>   The required column "Petal.Length" is missing.

# Most of the time, if you find yourself in the above scenario, then we
# suggest that you remove `Petal.Length` from the data that is supplied to
# the recipe. If that isn't an option, you can declare that that column
# isn't required at `bake()` time by using `update_role_requirements()`
rec_roles <- update_role_requirements(rec_roles, "important", bake = FALSE)
processed_roles <- mold(rec_roles, train)
forge(test2, processed_roles$blueprint)
#> $predictors
#> # A tibble: 50 × 1
#>    Sepal.Width
#>          <dbl>
#>  1         3.3
#>  2         2.7
#>  3         3  
#>  4         2.9
#>  5         3  
#>  6         3  
#>  7         2.5
#>  8         2.9
#>  9         2.5
#> 10         3.6
#> # ℹ 40 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$roles
#> $extras$roles$id
#> # A tibble: 50 × 1
#>    Sepal.Length
#>           <dbl>
#>  1          6.3
#>  2          5.8
#>  3          7.1
#>  4          6.3
#>  5          6.5
#>  6          7.6
#>  7          4.9
#>  8          7.3
#>  9          6.7
#> 10          7.2
#> # ℹ 40 more rows
#> 
#> $extras$roles$important
#> # A tibble: 50 × 0
#> 
#> $extras$roles$`NA`
#> # A tibble: 50 × 1
#>    Petal.Width
#>          <dbl>
#>  1         2.5
#>  2         1.9
#>  3         2.1
#>  4         1.8
#>  5         2.2
#>  6         2.1
#>  7         1.7
#>  8         1.8
#>  9         1.8
#> 10         2.5
#> # ℹ 40 more rows
#> 
#> 
#> 
```
