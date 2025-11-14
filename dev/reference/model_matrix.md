# Construct a design matrix

`model_matrix()` is a stricter version of
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html).
Notably, `model_matrix()` will *never* drop rows, and the result will be
a tibble.

## Usage

``` r
model_matrix(terms, data, ..., call = current_env())
```

## Arguments

- terms:

  A terms object to construct a model matrix with. This is typically the
  terms object returned from the corresponding call to
  [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md).

- data:

  A tibble to construct the design matrix with. This is typically the
  tibble returned from the corresponding call to
  [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md).

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A tibble containing the design matrix.

## Details

The following explains the rationale for some of the difference in
arguments compared to
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html):

- `contrasts.arg`: Set the contrasts argument, `options("contrasts")`
  globally, or assign a contrast to the factor of interest directly
  using [`stats::contrasts()`](https://rdrr.io/r/stats/contrasts.html).
  See the examples section.

- `xlev`: Not allowed because
  [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) is never
  called, so it is unnecessary.

- `...`: Not allowed because the default method of
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) does not
  use it, and the `lm` method uses it to pass potential offsets and
  weights through, which are handled differently in hardhat.

## Examples

``` r
# ---------------------------------------------------------------------------
# Example usage

framed <- model_frame(Sepal.Width ~ Species, iris)

model_matrix(framed$terms, framed$data)
#> # A tibble: 150 × 3
#>    `(Intercept)` Speciesversicolor Speciesvirginica
#>            <dbl>             <dbl>            <dbl>
#>  1             1                 0                0
#>  2             1                 0                0
#>  3             1                 0                0
#>  4             1                 0                0
#>  5             1                 0                0
#>  6             1                 0                0
#>  7             1                 0                0
#>  8             1                 0                0
#>  9             1                 0                0
#> 10             1                 0                0
#> # ℹ 140 more rows

# ---------------------------------------------------------------------------
# Missing values never result in dropped rows

iris2 <- iris
iris2$Species[1] <- NA

framed2 <- model_frame(Sepal.Width ~ Species, iris2)

model_matrix(framed2$terms, framed2$data)
#> # A tibble: 150 × 3
#>    `(Intercept)` Speciesversicolor Speciesvirginica
#>            <dbl>             <dbl>            <dbl>
#>  1             1                NA               NA
#>  2             1                 0                0
#>  3             1                 0                0
#>  4             1                 0                0
#>  5             1                 0                0
#>  6             1                 0                0
#>  7             1                 0                0
#>  8             1                 0                0
#>  9             1                 0                0
#> 10             1                 0                0
#> # ℹ 140 more rows

# ---------------------------------------------------------------------------
# Contrasts

# Default contrasts
y <- factor(c("a", "b"))
x <- data.frame(y = y)
framed <- model_frame(~y, x)

# Setting contrasts directly
y_with_contrast <- y
contrasts(y_with_contrast) <- contr.sum(2)
x2 <- data.frame(y = y_with_contrast)
framed2 <- model_frame(~y, x2)

# Compare!
model_matrix(framed$terms, framed$data)
#> # A tibble: 2 × 2
#>   `(Intercept)`    yb
#>           <dbl> <dbl>
#> 1             1     0
#> 2             1     1
model_matrix(framed2$terms, framed2$data)
#> # A tibble: 2 × 2
#>   `(Intercept)`    y1
#>           <dbl> <dbl>
#> 1             1     1
#> 2             1    -1

# Also, can set the contrasts globally
global_override <- c(unordered = "contr.sum", ordered = "contr.poly")

rlang::with_options(
  .expr = {
    model_matrix(framed$terms, framed$data)
  },
  contrasts = global_override
)
#> # A tibble: 2 × 2
#>   `(Intercept)`    y1
#>           <dbl> <dbl>
#> 1             1     1
#> 2             1    -1
```
