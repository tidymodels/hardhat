# Ensure that predictions have the correct number of rows

validate - asserts the following:

- The size of `pred` must be the same as the size of `new_data`.

check - returns the following:

- `ok` A logical. Does the check pass?

- `size_new_data` A single numeric. The size of `new_data`.

- `size_pred` A single numeric. The size of `pred`.

## Usage

``` r
validate_prediction_size(pred, new_data)

check_prediction_size(pred, new_data, ..., call = caller_env())
```

## Arguments

- pred:

  A tibble. The predictions to return from any prediction `type`. This
  is often created using one of the spruce functions, like
  [`spruce_numeric()`](https://hardhat.tidymodels.org/dev/reference/spruce.md).

- new_data:

  A data frame of new predictors and possibly outcomes.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

`validate_prediction_size()` returns `pred` invisibly.

`check_prediction_size()` returns a named list of three components,
`ok`, `size_new_data`, and `size_pred`.

## Details

This validation function is one that is more developer focused rather
than user focused. It is a final check to be used right before a value
is returned from your specific
[`predict()`](https://rdrr.io/r/stats/predict.html) method, and is
mainly a "good practice" sanity check to ensure that your prediction
blueprint always returns the same number of rows as `new_data`, which is
one of the modeling conventions this package tries to promote.

## Validation

hardhat provides validation functions at two levels.

- `check_*()`: *check a condition, and return a list*. The list always
  contains at least one element, `ok`, a logical that specifies if the
  check passed. Each check also has check specific elements in the
  returned list that can be used to construct meaningful error messages.

- `validate_*()`: *check a condition, and error if it does not pass*.
  These functions call their corresponding check function, and then
  provide a default error message. If you, as a developer, want a
  different error message, then call the `check_*()` function yourself,
  and provide your own validation function.

## See also

Other validation functions:
[`validate_column_names()`](https://hardhat.tidymodels.org/dev/reference/validate_column_names.md),
[`validate_no_formula_duplication()`](https://hardhat.tidymodels.org/dev/reference/validate_no_formula_duplication.md),
[`validate_outcomes_are_binary()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_binary.md),
[`validate_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md),
[`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md),
[`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
# Say new_data has 5 rows
new_data <- mtcars[1:5, ]

# And somehow you generate predictions
# for those 5 rows
pred_vec <- 1:5

# Then you use `spruce_numeric()` to clean
# up these numeric predictions
pred <- spruce_numeric(pred_vec)

pred
#> # A tibble: 5 × 1
#>   .pred
#>   <int>
#> 1     1
#> 2     2
#> 3     3
#> 4     4
#> 5     5

# Use this check to ensure that
# the number of rows or pred match new_data
check_prediction_size(pred, new_data)
#> $ok
#> [1] TRUE
#> 
#> $size_new_data
#> [1] 5
#> 
#> $size_pred
#> [1] 5
#> 

# An informative error message is thrown
# if the rows are different
try(validate_prediction_size(spruce_numeric(1:4), new_data))
#> Error in validate_prediction_size(spruce_numeric(1:4), new_data) : 
#>   The size of `new_data` (5) must match the size of `pred` (4).
```
