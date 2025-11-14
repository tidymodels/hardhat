# Ensure that the outcome has only factor columns

validate - asserts the following:

- `outcomes` must have factor columns.

check - returns the following:

- `ok` A logical. Does the check pass?

- `bad_classes` A named list. The names are the names of problematic
  columns, and the values are the classes of the matching column.

## Usage

``` r
validate_outcomes_are_factors(outcomes)

check_outcomes_are_factors(outcomes, ..., call = caller_env())
```

## Arguments

- outcomes:

  An object to check.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

`validate_outcomes_are_factors()` returns `outcomes` invisibly.

`check_outcomes_are_factors()` returns a named list of two components,
`ok` and `bad_classes`.

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
[`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md),
[`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md),
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
# Not a factor column.
check_outcomes_are_factors(data.frame(x = 1))
#> $ok
#> [1] FALSE
#> 
#> $bad_classes
#> $bad_classes$x
#> [1] "numeric"
#> 
#> 

# All good
check_outcomes_are_factors(data.frame(x = factor(c("A", "B"))))
#> $ok
#> [1] TRUE
#> 
#> $bad_classes
#> list()
#> 
```
