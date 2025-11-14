# Default XY blueprint

This pages holds the details for the XY preprocessing blueprint. This is
the blueprint used by default from
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) if `x`
and `y` are provided separately (i.e. the XY interface is used).

## Usage

``` r
default_xy_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "tibble"
)

# S3 method for class 'data.frame'
mold(x, y, ..., blueprint = NULL)

# S3 method for class 'matrix'
mold(x, y, ..., blueprint = NULL)
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

- composition:

  Either "tibble", "matrix", or "dgCMatrix" for the format of the
  processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
  the predictors must be numeric after the preprocessing method has been
  applied; otherwise an error is thrown.

- x:

  A data frame or matrix containing the predictors.

- y:

  A data frame, matrix, or vector containing the outcomes.

- ...:

  Not used.

- blueprint:

  A preprocessing `blueprint`. If left as `NULL`, then a
  `default_xy_blueprint()` is used.

## Value

For `default_xy_blueprint()`, an XY blueprint.

## Details

As documented in
[`standardize()`](https://hardhat.tidymodels.org/dev/reference/standardize.md),
if `y` is a *vector*, then the returned outcomes tibble has 1 column
with a standardized name of `".outcome"`.

The one special thing about the XY method's forge function is the
behavior of `outcomes = TRUE` when a *vector* `y` value was provided to
the original call to
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md). In
that case,
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
converts `y` into a tibble, with a default name of `.outcome`. This is
the column that
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) will
look for in `new_data` to preprocess. See the examples section for a
demonstration of this.

## Mold

When [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
used with the default xy blueprint:

- It converts `x` to a tibble.

- It adds an intercept column to `x` if `intercept = TRUE`.

- It runs
  [`standardize()`](https://hardhat.tidymodels.org/dev/reference/standardize.md)
  on `y`.

## Forge

When [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
is used with the default xy blueprint:

- It calls
  [`shrink()`](https://hardhat.tidymodels.org/dev/reference/shrink.md)
  to trim `new_data` to only the required columns and coerce `new_data`
  to a tibble.

- It calls
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md)
  to perform validation on the structure of the columns of `new_data`.

- It adds an intercept column onto `new_data` if `intercept = TRUE`.

## Examples

``` r
# ---------------------------------------------------------------------------
# Setup

train <- iris[1:100, ]
test <- iris[101:150, ]

train_x <- train["Sepal.Length"]
train_y <- train["Species"]

test_x <- test["Sepal.Length"]
test_y <- test["Species"]

# ---------------------------------------------------------------------------
# XY Example

# First, call mold() with the training data
processed <- mold(train_x, train_y)

# Then, call forge() with the blueprint and the test data
# to have it preprocess the test data in the same way
forge(test_x, processed$blueprint)
#> $predictors
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
#> $outcomes
#> NULL
#> 
#> $extras
#> NULL
#> 

# ---------------------------------------------------------------------------
# Intercept

processed <- mold(train_x, train_y, blueprint = default_xy_blueprint(intercept = TRUE))

forge(test_x, processed$blueprint)
#> $predictors
#> # A tibble: 50 × 2
#>    `(Intercept)` Sepal.Length
#>            <int>        <dbl>
#>  1             1          6.3
#>  2             1          5.8
#>  3             1          7.1
#>  4             1          6.3
#>  5             1          6.5
#>  6             1          7.6
#>  7             1          4.9
#>  8             1          7.3
#>  9             1          6.7
#> 10             1          7.2
#> # ℹ 40 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> NULL
#> 

# ---------------------------------------------------------------------------
# XY Method and forge(outcomes = TRUE)

# You can request that the new outcome columns are preprocessed as well, but
# they have to be present in `new_data`!

processed <- mold(train_x, train_y)

# Can't do this!
try(forge(test_x, processed$blueprint, outcomes = TRUE))
#> Error in forge(test_x, processed$blueprint, outcomes = TRUE) : 
#>   The required column "Species" is missing.

# Need to use the full test set, including `y`
forge(test, processed$blueprint, outcomes = TRUE)
#> $predictors
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
#> NULL
#> 

# With the XY method, if the Y value used in `mold()` is a vector,
# then a column name of `.outcome` is automatically generated.
# This name is what forge() looks for in `new_data`.

# Y is a vector!
y_vec <- train_y$Species

processed_vec <- mold(train_x, y_vec)

# This throws an informative error that tell you
# to include an `".outcome"` column in `new_data`.
try(forge(iris, processed_vec$blueprint, outcomes = TRUE))
#> Error in forge(iris, processed_vec$blueprint, outcomes = TRUE) : 
#>   The following required columns are missing: ".outcome".
#> ℹ This indicates that `mold()` was called with a vector for `y`.
#> ℹ When this is the case, and the outcome columns are requested in
#>   `forge()`, `new_data` must include a column with the automatically
#>   generated name, `.outcome`, containing the outcome.

test2 <- test
test2$.outcome <- test2$Species
test2$Species <- NULL

# This works, and returns a tibble in the $outcomes slot
forge(test2, processed_vec$blueprint, outcomes = TRUE)
#> $predictors
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
#> $outcomes
#> # A tibble: 50 × 1
#>    .outcome 
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
#> NULL
#> 

# ---------------------------------------------------------------------------
# Matrix output for predictors

# You can change the `composition` of the predictor data set
bp <- default_xy_blueprint(composition = "dgCMatrix")
processed <- mold(train_x, train_y, blueprint = bp)
class(processed$predictors)
#> [1] "dgCMatrix"
#> attr(,"package")
#> [1] "Matrix"
```
