# Ensure that `data` contains required column names

validate - asserts the following:

- The column names of `data` must contain all `original_names`.

check - returns the following:

- `ok` A logical. Does the check pass?

- `missing_names` A character vector. The missing column names.

## Usage

``` r
validate_column_names(data, original_names, ..., call = current_env())

check_column_names(data, original_names)
```

## Arguments

- data:

  A data frame to check.

- original_names:

  A character vector. The original column names.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

`validate_column_names()` returns `data` invisibly.

`check_column_names()` returns a named list of two components, `ok`, and
`missing_names`.

## Details

A special error is thrown if the missing column is named `".outcome"`.
This only happens in the case where
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
called using the xy-method, and a *vector* `y` value is supplied rather
than a data frame or matrix. In that case, `y` is coerced to a data
frame, and the automatic name `".outcome"` is added, and this is what is
looked for in
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md). If
this happens, and the user tries to request outcomes using
`forge(..., outcomes = TRUE)` but the supplied `new_data` does not
contain the required `".outcome"` column, a special error is thrown
telling them what to do. See the examples!

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
[`validate_no_formula_duplication()`](https://hardhat.tidymodels.org/dev/reference/validate_no_formula_duplication.md),
[`validate_outcomes_are_binary()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_binary.md),
[`validate_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md),
[`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md),
[`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md),
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md),
[`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)

## Examples

``` r
# ---------------------------------------------------------------------------

original_names <- colnames(mtcars)

test <- mtcars
bad_test <- test[, -c(3, 4)]

# All good
check_column_names(test, original_names)
#> $ok
#> [1] TRUE
#> 
#> $missing_names
#> character(0)
#> 

# Missing 2 columns
check_column_names(bad_test, original_names)
#> $ok
#> [1] FALSE
#> 
#> $missing_names
#> [1] "disp" "hp"  
#> 

# Will error
try(validate_column_names(bad_test, original_names))
#> Error in validate_column_names(bad_test, original_names) : 
#>   The required columns "disp" and "hp" are missing.

# ---------------------------------------------------------------------------
# Special error when `.outcome` is missing

train <- iris[1:100, ]
test <- iris[101:150, ]

train_x <- subset(train, select = -Species)
train_y <- train$Species

# Here, y is a vector
processed <- mold(train_x, train_y)

# So the default column name is `".outcome"`
processed$outcomes
#> # A tibble: 100 × 1
#>    .outcome
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

# It doesn't affect forge() normally
forge(test, processed$blueprint)
#> $predictors
#> # A tibble: 50 × 4
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>           <dbl>       <dbl>        <dbl>       <dbl>
#>  1          6.3         3.3          6           2.5
#>  2          5.8         2.7          5.1         1.9
#>  3          7.1         3            5.9         2.1
#>  4          6.3         2.9          5.6         1.8
#>  5          6.5         3            5.8         2.2
#>  6          7.6         3            6.6         2.1
#>  7          4.9         2.5          4.5         1.7
#>  8          7.3         2.9          6.3         1.8
#>  9          6.7         2.5          5.8         1.8
#> 10          7.2         3.6          6.1         2.5
#> # ℹ 40 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> NULL
#> 

# But if the outcome is requested, and `".outcome"`
# is not present in `new_data`, an error is thrown
# with very specific instructions
try(forge(test, processed$blueprint, outcomes = TRUE))
#> Error in forge(test, processed$blueprint, outcomes = TRUE) : 
#>   The following required columns are missing: ".outcome".
#> ℹ This indicates that `mold()` was called with a vector for `y`.
#> ℹ When this is the case, and the outcome columns are requested in
#>   `forge()`, `new_data` must include a column with the automatically
#>   generated name, `.outcome`, containing the outcome.

# To get this to work, just create an .outcome column in new_data
test$.outcome <- test$Species

forge(test, processed$blueprint, outcomes = TRUE)
#> $predictors
#> # A tibble: 50 × 4
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width
#>           <dbl>       <dbl>        <dbl>       <dbl>
#>  1          6.3         3.3          6           2.5
#>  2          5.8         2.7          5.1         1.9
#>  3          7.1         3            5.9         2.1
#>  4          6.3         2.9          5.6         1.8
#>  5          6.5         3            5.8         2.2
#>  6          7.6         3            6.6         2.1
#>  7          4.9         2.5          4.5         1.7
#>  8          7.3         2.9          6.3         1.8
#>  9          6.7         2.5          5.8         1.8
#> 10          7.2         3.6          6.1         2.5
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
```
