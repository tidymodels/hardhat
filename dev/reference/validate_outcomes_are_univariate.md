# Ensure that the outcome is univariate

validate - asserts the following:

- `outcomes` must have 1 column. Atomic vectors are treated as 1 column
  matrices.

check - returns the following:

- `ok` A logical. Does the check pass?

- `n_cols` A single numeric. The actual number of columns.

## Usage

``` r
validate_outcomes_are_univariate(outcomes)

check_outcomes_are_univariate(outcomes)
```

## Arguments

- outcomes:

  An object to check.

## Value

`validate_outcomes_are_univariate()` returns `outcomes` invisibly.

`check_outcomes_are_univariate()` returns a named list of two
components, `ok` and `n_cols`.

## Details

The expected way to use this validation function is to supply it the
`$outcomes` element of the result of a call to
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).

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
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
validate_outcomes_are_univariate(data.frame(x = 1))

try(validate_outcomes_are_univariate(mtcars))
#> Error in validate_outcomes_are_univariate(mtcars) : 
#>   The outcome must be univariate, but 11 columns were found.
```
