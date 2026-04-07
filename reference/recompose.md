# Recompose a data frame into another form

`recompose()` takes a data frame and converts it into one of:

- A tibble

- A data frame

- A matrix

- A sparse matrix (using the Matrix package)

This is an internal function used only by hardhat and recipes.

## Usage

``` r
recompose(data, ..., composition = "tibble", call = caller_env())
```

## Arguments

- data:

  A data frame.

- ...:

  These dots are for future extensions and must be empty.

- composition:

  One of:

  - `"tibble"` to convert to a tibble.

  - `"data.frame"` to convert to a base data frame.

  - `"matrix"` to convert to a matrix. All columns must be numeric.

  - `"dgCMatrix"` to convert to a sparse matrix. All columns must be
    numeric, and the Matrix package must be installed.

- call:

  The call used for errors and warnings.

## Value

The output type is determined from the `composition`.

## Examples

``` r
df <- vctrs::data_frame(x = 1)

recompose(df)
#> # A tibble: 1 × 1
#>       x
#>   <dbl>
#> 1     1
recompose(df, composition = "matrix")
#>      x
#> [1,] 1

# All columns must be numeric to convert to a matrix
df <- vctrs::data_frame(x = 1, y = "a")
try(recompose(df, composition = "matrix"))
#> Error in eval(expr, envir) : 
#>   `data` must only contain numeric columns.
#> ℹ This column isn't numeric: "y".
```
