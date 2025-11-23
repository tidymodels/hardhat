# Construct a model frame

`model_frame()` is a stricter version of
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html).
There are a number of differences, with the main being that rows are
*never* dropped and the return value is a list with the frame and terms
separated into two distinct objects.

## Usage

``` r
model_frame(formula, data, ..., call = current_env())
```

## Arguments

- formula:

  A formula or terms object representing the terms of the model frame.

- data:

  A data frame or matrix containing the terms of `formula`.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A named list with two elements:

- `"data"`: A tibble containing the model frame.

- `"terms"`: A terms object containing the terms for the model frame.

## Details

The following explains the rationale for some of the difference in
arguments compared to
[`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html):

- `subset`: Not allowed because the number of rows before and after
  `model_frame()` has been run should always be the same.

- `na.action`: Not allowed and is forced to `"na.pass"` because the
  number of rows before and after `model_frame()` has been run should
  always be the same.

- `drop.unused.levels`: Not allowed because it seems inconsistent for
  `data` and the result of `model_frame()` to ever have the same factor
  column but with different levels, unless specified though
  `original_levels`. If this is required, it should be done through a
  recipe step explicitly.

- `xlev`: Not allowed because this check should have been done ahead of
  time. Use
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md)
  to check the integrity of `data` against a training set if that is
  required.

- `...`: Not exposed because offsets are handled separately, and it is
  not necessary to pass weights here any more because rows are never
  dropped (so weights don't have to be subset alongside the rest of the
  design matrix). If other non-predictor columns are required, use the
  "roles" features of recipes.

It is important to always use the results of `model_frame()` with
[`model_matrix()`](https://hardhat.tidymodels.org/dev/reference/model_matrix.md)
rather than
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
because the tibble in the result of `model_frame()` does *not* have a
terms object attached. If `model.matrix(<terms>, <tibble>)` is called
directly, then a call to
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) will be made
automatically, which can give faulty results.

## Examples

``` r
# ---------------------------------------------------------------------------
# Example usage

framed <- model_frame(Species ~ Sepal.Width, iris)

framed$data
#> # A tibble: 150 × 2
#>    Species Sepal.Width
#>    <fct>         <dbl>
#>  1 setosa          3.5
#>  2 setosa          3  
#>  3 setosa          3.2
#>  4 setosa          3.1
#>  5 setosa          3.6
#>  6 setosa          3.9
#>  7 setosa          3.4
#>  8 setosa          3.4
#>  9 setosa          2.9
#> 10 setosa          3.1
#> # ℹ 140 more rows

framed$terms
#> Species ~ Sepal.Width
#> attr(,"variables")
#> list(Species, Sepal.Width)
#> attr(,"factors")
#>             Sepal.Width
#> Species               0
#> Sepal.Width           1
#> attr(,"term.labels")
#> [1] "Sepal.Width"
#> attr(,"order")
#> [1] 1
#> attr(,"intercept")
#> [1] 1
#> attr(,"response")
#> [1] 1
#> attr(,".Environment")
#> <environment: 0x564776d0b588>
#> attr(,"predvars")
#> list(Species, Sepal.Width)
#> attr(,"dataClasses")
#>     Species Sepal.Width 
#>    "factor"   "numeric" 

# ---------------------------------------------------------------------------
# Missing values never result in dropped rows

iris2 <- iris
iris2$Sepal.Width[1] <- NA

framed2 <- model_frame(Species ~ Sepal.Width, iris2)

head(framed2$data)
#> # A tibble: 6 × 2
#>   Species Sepal.Width
#>   <fct>         <dbl>
#> 1 setosa         NA  
#> 2 setosa          3  
#> 3 setosa          3.2
#> 4 setosa          3.1
#> 5 setosa          3.6
#> 6 setosa          3.9

nrow(framed2$data) == nrow(iris2)
#> [1] TRUE
```
