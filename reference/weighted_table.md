# Weighted table

`weighted_table()` computes a weighted contingency table based on
factors provided in `...` and a double vector of weights provided in
`weights`. It can be seen as a weighted extension to
[`base::table()`](https://rdrr.io/r/base/table.html) and an alternative
to [`stats::xtabs()`](https://rdrr.io/r/stats/xtabs.html).

`weighted_table()` always uses the *exact* set of levels returned by
[`levels()`](https://rdrr.io/r/base/levels.html) when constructing the
table. This results in the following properties:

- Missing values found in the factors are never included in the table
  unless there is an explicit `NA` factor level. If needed, this can be
  added to a factor with
  [`base::addNA()`](https://rdrr.io/r/base/factor.html) or
  `forcats::fct_expand(x, NA)`.

- Levels found in the factors that aren't actually used in the
  underlying data are included in the table with a value of `0`. If
  needed, you can drop unused factor levels by re-running your factor
  through [`factor()`](https://rdrr.io/r/base/factor.html), or by
  calling `forcats::fct_drop()`.

See the examples section for more information about these properties.

## Usage

``` r
weighted_table(..., weights, na_remove = FALSE)
```

## Arguments

- ...:

  Factors of equal length to use in the weighted table. If the `...` are
  named, those names will propagate onto the "dimnames names" of the
  resulting table. At least one factor must be provided.

- weights:

  A double vector of weights used to fill the cells of the weighted
  table. This must be the same length as the factors provided in `...`.

- na_remove:

  A single `TRUE` or `FALSE` for handling whether or not missing values
  in `weights` should be removed when summing up the weights.

## Value

The weighted table as an array of double values.

## Details

The result of `weighted_table()` does not have a `"table"` class
attached to it. It is only a double array. This is because "table"
objects are defined as containing integer counts, but weighted tables
can utilize fractional weights.

## Examples

``` r
x <- factor(c("x", "y", "z", "x", "x", "y"))
y <- factor(c("a", "b", "a", "a", "b", "b"))
w <- c(1.5, 2, 1.1, .5, 3, 2)

weighted_table(x = x, y = y, weights = w)
#>    y
#> x     a b
#>   x 2.0 3
#>   y 0.0 4
#>   z 1.1 0

# ---------------------------------------------------------------------------
# If `weights` contains missing values, then missing values will be
# propagated into the weighted table
x <- factor(c("x", "y", "y"))
y <- factor(c("a", "b", "b"))
w <- c(1, NA, 3)

weighted_table(x = x, y = y, weights = w)
#>    y
#> x   a  b
#>   x 1  0
#>   y 0 NA

# You can remove the missing values while summing up the weights with
# `na_remove = TRUE`
weighted_table(x = x, y = y, weights = w, na_remove = TRUE)
#>    y
#> x   a b
#>   x 1 0
#>   y 0 3

# ---------------------------------------------------------------------------
# If there are missing values in the factors, those typically don't show
# up in the weighted table
x <- factor(c("x", NA, "y", "x"))
y <- factor(c("a", "b", "a", NA))
w <- 1:4

weighted_table(x = x, y = y, weights = w)
#>    y
#> x   a b
#>   x 1 0
#>   y 3 0

# This is because the missing values aren't considered explicit levels
levels(x)
#> [1] "x" "y"

# You can force them to show up in the table by using `addNA()` ahead of time
# (or `forcats::fct_expand(x, NA)`)
x <- addNA(x, ifany = TRUE)
y <- addNA(y, ifany = TRUE)
levels(x)
#> [1] "x" "y" NA 

weighted_table(x = x, y = y, weights = w)
#>       y
#> x      a b <NA>
#>   x    1 0    4
#>   y    3 0    0
#>   <NA> 0 2    0

# ---------------------------------------------------------------------------
# If there are levels in your factors that aren't actually used in the
# underlying data, then they will still show up in the table with a `0` value
x <- factor(c("x", "y", "x"), levels = c("x", "y", "z"))
y <- factor(c("a", "b", "a"), levels = c("a", "b", "c"))
w <- 1:3

weighted_table(x = x, y = y, weights = w)
#>    y
#> x   a b c
#>   x 4 0 0
#>   y 0 2 0
#>   z 0 0 0

# If you want to drop these empty factor levels from the result, you can
# rerun `factor()` ahead of time to drop them (or `forcats::fct_drop()`)
x <- factor(x)
y <- factor(y)
levels(x)
#> [1] "x" "y"

weighted_table(x = x, y = y, weights = w)
#>    y
#> x   a b
#>   x 4 0
#>   y 0 2
```
