# Ensure no duplicate terms appear in `formula`

validate - asserts the following:

- `formula` must not have duplicates terms on the left and right hand
  side of the formula.

check - returns the following:

- `ok` A logical. Does the check pass?

- `duplicates` A character vector. The duplicate terms.

## Usage

``` r
validate_no_formula_duplication(formula, original = FALSE)

check_no_formula_duplication(formula, original = FALSE)
```

## Arguments

- formula:

  A formula to check.

- original:

  A logical. Should the original names be checked, or should the names
  after processing be used? If `FALSE`, `y ~ log(y)` is allowed because
  the names are `"y"` and `"log(y)"`, if `TRUE`, `y ~ log(y)` is not
  allowed because the original names are both `"y"`.

## Value

`validate_no_formula_duplication()` returns `formula` invisibly.

`check_no_formula_duplication()` returns a named list of two components,
`ok` and `duplicates`.

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
[`validate_outcomes_are_binary()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_binary.md),
[`validate_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md),
[`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md),
[`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md),
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
# All good
check_no_formula_duplication(y ~ x)
#> $ok
#> [1] TRUE
#> 
#> $duplicates
#> character(0)
#> 

# Not good!
check_no_formula_duplication(y ~ y)
#> $ok
#> [1] FALSE
#> 
#> $duplicates
#> [1] "y"
#> 

# This is generally okay
check_no_formula_duplication(y ~ log(y))
#> $ok
#> [1] TRUE
#> 
#> $duplicates
#> character(0)
#> 

# But you can be more strict
check_no_formula_duplication(y ~ log(y), original = TRUE)
#> $ok
#> [1] FALSE
#> 
#> $duplicates
#> [1] "y"
#> 

# This would throw an error
try(validate_no_formula_duplication(log(y) ~ log(y)))
#> Error in validate_no_formula_duplication(log(y) ~ log(y)) : 
#>   Terms must not be duplicated on the left- and right-hand side
#> of the `formula`.
#> ℹ The following duplicated term was found: "log(y)"
```
