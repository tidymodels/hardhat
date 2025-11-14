# Ensure that the outcome has binary factors

validate - asserts the following:

- `outcomes` must have binary factor columns.

check - returns the following:

- `ok` A logical. Does the check pass?

- `bad_cols` A character vector. The names of the columns with problems.

- `num_levels` An integer vector. The actual number of levels of the
  columns with problems.

## Usage

``` r
validate_outcomes_are_binary(outcomes)

check_outcomes_are_binary(outcomes, ..., call = caller_env())
```

## Arguments

- outcomes:

  An object to check.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

`validate_outcomes_are_binary()` returns `outcomes` invisibly.

`check_outcomes_are_binary()` returns a named list of three components,
`ok`, `bad_cols`, and `num_levels`.

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
[`validate_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md),
[`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md),
[`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md),
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
# Not a binary factor. 0 levels
check_outcomes_are_binary(data.frame(x = 1))
#> $ok
#> [1] FALSE
#> 
#> $bad_cols
#> [1] "x"
#> 
#> $num_levels
#> [1] 0
#> 

# Not a binary factor. 1 level
check_outcomes_are_binary(data.frame(x = factor("A")))
#> $ok
#> [1] FALSE
#> 
#> $bad_cols
#> [1] "x"
#> 
#> $num_levels
#> [1] 1
#> 

# All good
check_outcomes_are_binary(data.frame(x = factor(c("A", "B"))))
#> $ok
#> [1] TRUE
#> 
#> $bad_cols
#> character(0)
#> 
#> $num_levels
#> integer(0)
#> 
```
