# Default formula blueprint

This pages holds the details for the formula preprocessing blueprint.
This is the blueprint used by default from
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) if `x`
is a formula.

## Usage

``` r
default_formula_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  indicators = "traditional",
  composition = "tibble"
)

# S3 method for class 'formula'
mold(formula, data, ..., blueprint = NULL)
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

- indicators:

  A single character string. Control how factors are expanded into dummy
  variable indicator columns. One of:

  - `"traditional"` - The default. Create dummy variables using the
    traditional
    [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    infrastructure. Generally this creates `K - 1` indicator columns for
    each factor, where `K` is the number of levels in that factor.

  - `"none"` - Leave factor variables alone. No expansion is done.

  - `"one_hot"` - Create dummy variables using a one-hot encoding
    approach that expands unordered factors into all `K` indicator
    columns, rather than `K - 1`.

- composition:

  Either "tibble", "matrix", or "dgCMatrix" for the format of the
  processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
  the predictors must be numeric after the preprocessing method has been
  applied; otherwise an error is thrown.

- formula:

  A formula specifying the predictors and the outcomes.

- data:

  A data frame or matrix containing the outcomes and predictors.

- ...:

  Not used.

- blueprint:

  A preprocessing `blueprint`. If left as `NULL`, then a
  `default_formula_blueprint()` is used.

## Value

For `default_formula_blueprint()`, a formula blueprint.

## Details

While not different from base R, the behavior of expanding factors into
dummy variables when `indicators = "traditional"` and an intercept is
*not* present is not always intuitive and should be documented.

- When an intercept is present, factors are expanded into `K-1` new
  columns, where `K` is the number of levels in the factor.

- When an intercept is *not* present, the first factor is expanded into
  all `K` columns (one-hot encoding), and the remaining factors are
  expanded into `K-1` columns. This behavior ensures that meaningful
  predictions can be made for the reference level of the first factor,
  but is not the exact "no intercept" model that was requested. Without
  this behavior, predictions for the reference level of the first factor
  would always be forced to `0` when there is no intercept.

Offsets can be included in the formula method through the use of the
inline function
[`stats::offset()`](https://rdrr.io/r/stats/offset.html). These are
returned as a tibble with 1 column named `".offset"` in the
`$extras$offset` slot of the return value.

## Mold

When [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
used with the default formula blueprint:

- Predictors

  - The RHS of the `formula` is isolated, and converted to its own 1
    sided formula: `~ RHS`.

  - Runs
    [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
    on the RHS formula and uses `data`.

  - If `indicators = "traditional"`, it then runs
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    on the result.

  - If `indicators = "none"`, factors are removed before
    [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) is
    run, and then added back afterwards. No interactions or inline
    functions involving factors are allowed.

  - If `indicators = "one_hot"`, it then runs
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    on the result using a contrast function that creates indicator
    columns for all levels of all factors.

  - If any offsets are present from using
    [`offset()`](https://rdrr.io/r/stats/offset.html), then they are
    extracted with
    [`model_offset()`](https://hardhat.tidymodels.org/dev/reference/model_offset.md).

  - If `intercept = TRUE`, adds an intercept column.

  - Coerces the result of the above steps to a tibble.

- Outcomes

  - The LHS of the `formula` is isolated, and converted to its own 1
    sided formula: `~ LHS`.

  - Runs
    [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
    on the LHS formula and uses `data`.

  - Coerces the result of the above steps to a tibble.

## Forge

When [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
is used with the default formula blueprint:

- It calls
  [`shrink()`](https://hardhat.tidymodels.org/dev/reference/shrink.md)
  to trim `new_data` to only the required columns and coerce `new_data`
  to a tibble.

- It calls
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md)
  to perform validation on the structure of the columns of `new_data`.

- Predictors

  - It runs
    [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
    on `new_data` using the stored terms object corresponding to the
    *predictors*.

  - If, in the original
    [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
    call, `indicators = "traditional"` was set, it then runs
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    on the result.

  - If, in the original
    [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
    call, `indicators = "none"` was set, it runs
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    on the result without the factor columns, and then adds them on
    afterwards.

  - If, in the original
    [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
    call, `indicators = "one_hot"` was set, it runs
    [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    on the result with a contrast function that includes indicators for
    all levels of all factor columns.

  - If any offsets are present from using
    [`offset()`](https://rdrr.io/r/stats/offset.html) in the original
    call to
    [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md),
    then they are extracted with
    [`model_offset()`](https://hardhat.tidymodels.org/dev/reference/model_offset.md).

  - If `intercept = TRUE` in the original call to
    [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md),
    then an intercept column is added.

  - It coerces the result of the above steps to a tibble.

- Outcomes

  - It runs
    [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
    on `new_data` using the stored terms object corresponding to the
    *outcomes*.

  - Coerces the result to a tibble.

## Differences From Base R

There are a number of differences from base R regarding how formulas are
processed by
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) that
require some explanation.

Multivariate outcomes can be specified on the LHS using syntax that is
similar to the RHS (i.e. `outcome_1 + outcome_2 ~ predictors`). If any
complex calculations are done on the LHS and they return matrices (like
[`stats::poly()`](https://rdrr.io/r/stats/poly.html)), then those
matrices are flattened into multiple columns of the tibble after the
call to [`model.frame()`](https://rdrr.io/r/stats/model.frame.html).
While this is possible, it is not recommended, and if a large amount of
preprocessing is required on the outcomes, then you are better off using
a
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).

Global variables are *not* allowed in the formula. An error will be
thrown if they are included. All terms in the formula should come from
`data`. If you need to use inline functions in the formula, the safest
way to do so is to prefix them with their package name, like
`pkg::fn()`. This ensures that the function will always be available at
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) (fit)
and [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
(prediction) time. That said, if the package is *attached* (i.e. with
[`library()`](https://rdrr.io/r/base/library.html)), then you should be
able to use the inline function without the prefix.

By default, intercepts are *not* included in the predictor output from
the formula. To include an intercept, set
`blueprint = default_formula_blueprint(intercept = TRUE)`. The rationale
for this is that many packages either always require or never allow an
intercept (for example, the `earth` package), and they do a large amount
of extra work to keep the user from supplying one or removing it. This
interface standardizes all of that flexibility in one place.

## Examples

``` r
# ---------------------------------------------------------------------------

data("hardhat-example-data")

# ---------------------------------------------------------------------------
# Formula Example

# Call mold() with the training data
processed <- mold(
  log(num_1) ~ num_2 + fac_1,
  example_train,
  blueprint = default_formula_blueprint(intercept = TRUE)
)

# Then, call forge() with the blueprint and the test data
# to have it preprocess the test data in the same way
forge(example_test, processed$blueprint)
#> $predictors
#> # A tibble: 2 × 4
#>   `(Intercept)` num_2 fac_1b fac_1c
#>           <dbl> <dbl>  <dbl>  <dbl>
#> 1             1 0.967      0      0
#> 2             1 0.761      0      1
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$offset
#> NULL
#> 
#> 

# Use `outcomes = TRUE` to also extract the preprocessed outcome
forge(example_test, processed$blueprint, outcomes = TRUE)
#> $predictors
#> # A tibble: 2 × 4
#>   `(Intercept)` num_2 fac_1b fac_1c
#>           <dbl> <dbl>  <dbl>  <dbl>
#> 1             1 0.967      0      0
#> 2             1 0.761      0      1
#> 
#> $outcomes
#> # A tibble: 2 × 1
#>   `log(num_1)`
#>          <dbl>
#> 1         3.00
#> 2         3.04
#> 
#> $extras
#> $extras$offset
#> NULL
#> 
#> 

# ---------------------------------------------------------------------------
# Factors without an intercept

# No intercept is added by default
processed <- mold(num_1 ~ fac_1 + fac_2, example_train)

# So, for factor columns, the first factor is completely expanded into all
# `K` columns (the number of levels), and the subsequent factors are expanded
# into `K - 1` columns.
processed$predictors
#> # A tibble: 12 × 4
#>    fac_1a fac_1b fac_1c fac_2B
#>     <dbl>  <dbl>  <dbl>  <dbl>
#>  1      1      0      0      0
#>  2      1      0      0      1
#>  3      1      0      0      0
#>  4      1      0      0      1
#>  5      0      1      0      0
#>  6      0      1      0      1
#>  7      0      1      0      0
#>  8      0      1      0      1
#>  9      0      0      1      0
#> 10      0      0      1      1
#> 11      0      0      1      0
#> 12      0      0      1      1

# In the above example, `fac_1` is expanded into all three columns,
# `fac_2` is not. This behavior comes from `model.matrix()`, and is somewhat
# known in the R community, but can lead to a model that is difficult to
# interpret since the corresponding p-values are testing wildly different
# hypotheses.

# To get all indicators for all columns (irrespective of the intercept),
# use the `indicators = "one_hot"` option
processed <- mold(
  num_1 ~ fac_1 + fac_2,
  example_train,
  blueprint = default_formula_blueprint(indicators = "one_hot")
)

processed$predictors
#> # A tibble: 12 × 5
#>    fac_1a fac_1b fac_1c fac_2A fac_2B
#>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
#>  1      1      0      0      1      0
#>  2      1      0      0      0      1
#>  3      1      0      0      1      0
#>  4      1      0      0      0      1
#>  5      0      1      0      1      0
#>  6      0      1      0      0      1
#>  7      0      1      0      1      0
#>  8      0      1      0      0      1
#>  9      0      0      1      1      0
#> 10      0      0      1      0      1
#> 11      0      0      1      1      0
#> 12      0      0      1      0      1

# It is not possible to construct a no-intercept model that expands all
# factors into `K - 1` columns using the formula method. If required, a
# recipe could be used to construct this model.

# ---------------------------------------------------------------------------
# Global variables

y <- rep(1, times = nrow(example_train))

# In base R, global variables are allowed in a model formula
frame <- model.frame(fac_1 ~ y + num_2, example_train)
head(frame)
#>   fac_1 y num_2
#> 1     a 1 0.579
#> 2     a 1 0.338
#> 3     a 1 0.206
#> 4     a 1 0.546
#> 5     b 1 0.964
#> 6     b 1 0.631

# mold() does not allow them, and throws an error
try(mold(fac_1 ~ y + num_2, example_train))
#> Error in mold(fac_1 ~ y + num_2, example_train) : 
#>   The following predictor was not found in `data`: "y".

# ---------------------------------------------------------------------------
# Dummy variables and interactions

# By default, factor columns are expanded
# and interactions are created, both by
# calling `model.matrix()`. Some models (like
# tree based models) can take factors directly
# but still might want to use the formula method.
# In those cases, set `indicators = "none"` to not
# run `model.matrix()` on factor columns. Interactions
# are still allowed and are run on numeric columns.

bp_no_indicators <- default_formula_blueprint(indicators = "none")

processed <- mold(
  ~ fac_1 + num_1:num_2,
  example_train,
  blueprint = bp_no_indicators
)

processed$predictors
#> # A tibble: 12 × 2
#>    `num_1:num_2` fac_1
#>            <dbl> <fct>
#>  1         0.579 a    
#>  2         0.676 a    
#>  3         0.618 a    
#>  4         2.18  a    
#>  5         4.82  b    
#>  6         3.79  b    
#>  7         5.66  b    
#>  8         1.66  b    
#>  9         2.84  c    
#> 10         0.83  c    
#> 11         6.81  c    
#> 12         7.42  c    

# An informative error is thrown when `indicators = "none"` and
# factors are present in interaction terms or in inline functions
try(mold(num_1 ~ num_2:fac_1, example_train, blueprint = bp_no_indicators))
#> Error in mold(num_1 ~ num_2:fac_1, example_train, blueprint = bp_no_indicators) : 
#>   Interaction terms involving factors or characters have been
#> detected on the RHS of `formula`. These are not allowed when
#> `indicators = "none"`.
#> ℹ Interactions terms involving factors were detected for "fac_1" in
#>   `num_2:fac_1`.
try(mold(num_1 ~ paste0(fac_1), example_train, blueprint = bp_no_indicators))
#> Error in mold(num_1 ~ paste0(fac_1), example_train, blueprint = bp_no_indicators) : 
#>   Functions involving factors or characters have been detected on
#> the RHS of `formula`. These are not allowed when `indicators =
#> "none"`.
#> ℹ Functions involving factors were detected for "fac_1" in
#>   `paste0(fac_1)`.

# ---------------------------------------------------------------------------
# Multivariate outcomes

# Multivariate formulas can be specified easily
processed <- mold(num_1 + log(num_2) ~ fac_1, example_train)
processed$outcomes
#> # A tibble: 12 × 2
#>    num_1 `log(num_2)`
#>    <int>        <dbl>
#>  1     1      -0.546 
#>  2     2      -1.08  
#>  3     3      -1.58  
#>  4     4      -0.605 
#>  5     5      -0.0367
#>  6     6      -0.460 
#>  7     7      -0.213 
#>  8     8      -1.57  
#>  9     9      -1.15  
#> 10    10      -2.49  
#> 11    11      -0.480 
#> 12    12      -0.481 

# Inline functions on the LHS are run, but any matrix
# output is flattened (like what happens in `model.matrix()`)
# (essentially this means you don't wind up with columns
# in the tibble that are matrices)
processed <- mold(poly(num_2, degree = 2) ~ fac_1, example_train)
processed$outcomes
#> # A tibble: 12 × 2
#>    `poly(num_2, degree = 2).1` `poly(num_2, degree = 2).2`
#>                          <dbl>                       <dbl>
#>  1                      0.0981                      -0.254
#>  2                     -0.177                       -0.157
#>  3                     -0.327                        0.108
#>  4                      0.0604                      -0.270
#>  5                      0.537                        0.634
#>  6                      0.157                       -0.209
#>  7                      0.359                        0.120
#>  8                     -0.325                        0.103
#>  9                     -0.202                       -0.124
#> 10                     -0.468                        0.492
#> 11                      0.144                       -0.221
#> 12                      0.143                       -0.222

# TRUE
ncol(processed$outcomes) == 2
#> [1] TRUE

# Multivariate formulas specified in mold()
# carry over into forge()
forge(example_test, processed$blueprint, outcomes = TRUE)
#> $predictors
#> # A tibble: 2 × 3
#>   fac_1a fac_1b fac_1c
#>    <dbl>  <dbl>  <dbl>
#> 1      1      0      0
#> 2      0      0      1
#> 
#> $outcomes
#> # A tibble: 2 × 2
#>   `poly(num_2, degree = 2).1` `poly(num_2, degree = 2).2`
#>                         <dbl>                       <dbl>
#> 1                       0.541                     0.646  
#> 2                       0.306                     0.00619
#> 
#> $extras
#> $extras$offset
#> NULL
#> 
#> 

# ---------------------------------------------------------------------------
# Offsets

# Offsets are handled specially in base R, so they deserve special
# treatment here as well. You can add offsets using the inline function
# `offset()`
processed <- mold(num_1 ~ offset(num_2) + fac_1, example_train)

processed$extras$offset
#> # A tibble: 12 × 1
#>    .offset
#>      <dbl>
#>  1   0.579
#>  2   0.338
#>  3   0.206
#>  4   0.546
#>  5   0.964
#>  6   0.631
#>  7   0.808
#>  8   0.208
#>  9   0.316
#> 10   0.083
#> 11   0.619
#> 12   0.618

# Multiple offsets can be included, and they get added together
processed <- mold(
  num_1 ~ offset(num_2) + offset(num_3),
  example_train
)

identical(
  processed$extras$offset$.offset,
  example_train$num_2 + example_train$num_3
)
#> [1] TRUE

# Forging test data will also require
# and include the offset
forge(example_test, processed$blueprint)
#> $predictors
#> # A tibble: 2 × 0
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$offset
#> # A tibble: 2 × 1
#>   .offset
#>     <dbl>
#> 1   1.06 
#> 2   0.802
#> 
#> 

# ---------------------------------------------------------------------------
# Intercept only

# Because `1` and `0` are intercept modifying terms, they are
# not allowed in the formula and are instead controlled by the
# `intercept` argument of the blueprint. To use an intercept
# only formula, you should supply `NULL` on the RHS of the formula.
mold(
  ~NULL,
  example_train,
  blueprint = default_formula_blueprint(intercept = TRUE)
)
#> $predictors
#> # A tibble: 12 × 1
#>    `(Intercept)`
#>            <dbl>
#>  1             1
#>  2             1
#>  3             1
#>  4             1
#>  5             1
#>  6             1
#>  7             1
#>  8             1
#>  9             1
#> 10             1
#> 11             1
#> 12             1
#> 
#> $outcomes
#> # A tibble: 12 × 0
#> 
#> $blueprint
#> Formula blueprint:
#> # Predictors: 0
#> # Outcomes: 0
#> Intercept: TRUE
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

# ---------------------------------------------------------------------------
# Matrix output for predictors

# You can change the `composition` of the predictor data set
bp <- default_formula_blueprint(composition = "dgCMatrix")
processed <- mold(log(num_1) ~ num_2 + fac_1, example_train, blueprint = bp)
class(processed$predictors)
#> [1] "dgCMatrix"
#> attr(,"package")
#> [1] "Matrix"
```
