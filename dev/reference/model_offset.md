# Extract a model offset

`model_offset()` extracts a numeric offset from a model frame. It is
inspired by
[`stats::model.offset()`](https://rdrr.io/r/stats/model.extract.html),
but has nicer error messages and is slightly stricter.

## Usage

``` r
model_offset(terms, data, ..., call = caller_env())
```

## Arguments

- terms:

  A `"terms"` object corresponding to `data`, returned from a call to
  [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md).

- data:

  A data frame returned from a call to
  [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md).

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A numeric vector representing the offset.

## Details

If a column that has been tagged as an offset is not numeric, a nice
error message is thrown telling you exactly which column was
problematic.

[`stats::model.offset()`](https://rdrr.io/r/stats/model.extract.html)
also allows for a column named `"(offset)"` to be considered an offset
along with any others that have been tagged by
[`stats::offset()`](https://rdrr.io/r/stats/offset.html). However,
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
does not recognize these columns as offsets (so it doesn't remove them
as it should). Because of this inconsistency, columns named `"(offset)"`
are *not* treated specially by `model_offset()`.

## Examples

``` r
x <- model.frame(Species ~ offset(Sepal.Width), iris)

model_offset(terms(x), x)
#>   [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 3.7 3.4 3.0 3.0 4.0 4.4
#>  [17] 3.9 3.5 3.8 3.8 3.4 3.7 3.6 3.3 3.4 3.0 3.4 3.5 3.4 3.2 3.1 3.4
#>  [33] 4.1 4.2 3.1 3.2 3.5 3.6 3.0 3.4 3.5 2.3 3.2 3.5 3.8 3.0 3.8 3.2
#>  [49] 3.7 3.3 3.2 3.2 3.1 2.3 2.8 2.8 3.3 2.4 2.9 2.7 2.0 3.0 2.2 2.9
#>  [65] 2.9 3.1 3.0 2.7 2.2 2.5 3.2 2.8 2.5 2.8 2.9 3.0 2.8 3.0 2.9 2.6
#>  [81] 2.4 2.4 2.7 2.7 3.0 3.4 3.1 2.3 3.0 2.5 2.6 3.0 2.6 2.3 2.7 3.0
#>  [97] 2.9 2.9 2.5 2.8 3.3 2.7 3.0 2.9 3.0 3.0 2.5 2.9 2.5 3.6 3.2 2.7
#> [113] 3.0 2.5 2.8 3.2 3.0 3.8 2.6 2.2 3.2 2.8 2.8 2.7 3.3 3.2 2.8 3.0
#> [129] 2.8 3.0 2.8 3.8 2.8 2.8 2.6 3.0 3.4 3.1 3.0 3.1 3.1 3.1 2.7 3.2
#> [145] 3.3 3.0 2.5 3.0 3.4 3.0

xx <- model.frame(Species ~ offset(Sepal.Width) + offset(Sepal.Length), iris)

model_offset(terms(xx), xx)
#>   [1]  8.6  7.9  7.9  7.7  8.6  9.3  8.0  8.4  7.3  8.0  9.1  8.2  7.8
#>  [14]  7.3  9.8 10.1  9.3  8.6  9.5  8.9  8.8  8.8  8.2  8.4  8.2  8.0
#>  [27]  8.4  8.7  8.6  7.9  7.9  8.8  9.3  9.7  8.0  8.2  9.0  8.5  7.4
#>  [40]  8.5  8.5  6.8  7.6  8.5  8.9  7.8  8.9  7.8  9.0  8.3 10.2  9.6
#>  [53] 10.0  7.8  9.3  8.5  9.6  7.3  9.5  7.9  7.0  8.9  8.2  9.0  8.5
#>  [66]  9.8  8.6  8.5  8.4  8.1  9.1  8.9  8.8  8.9  9.3  9.6  9.6  9.7
#>  [79]  8.9  8.3  7.9  7.9  8.5  8.7  8.4  9.4  9.8  8.6  8.6  8.0  8.1
#>  [92]  9.1  8.4  7.3  8.3  8.7  8.6  9.1  7.6  8.5  9.6  8.5 10.1  9.2
#> [105]  9.5 10.6  7.4 10.2  9.2 10.8  9.7  9.1  9.8  8.2  8.6  9.6  9.5
#> [118] 11.5 10.3  8.2 10.1  8.4 10.5  9.0 10.0 10.4  9.0  9.1  9.2 10.2
#> [131] 10.2 11.7  9.2  9.1  8.7 10.7  9.7  9.5  9.0 10.0  9.8 10.0  8.5
#> [144] 10.0 10.0  9.7  8.8  9.5  9.6  8.9

# Problematic columns are caught with intuitive errors
tryCatch(
  expr = {
    x <- model.frame(~ offset(Species), iris)
    model_offset(terms(x), x)
  },
  error = function(e) {
    print(e$message)
  }
)
#> [1] "Column \033[34m\"offset(Species)\"\033[39m is tagged as an offset and thus must be\n        numeric, not a \033[34m<factor>\033[39m object."
```
