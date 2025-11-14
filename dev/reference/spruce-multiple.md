# Spruce up multi-outcome predictions

This family of `spruce_*_multiple()` functions converts multi-outcome
predictions into a standardized format. They are generally called from a
prediction implementation function for the specific `type` of prediction
to return.

## Usage

``` r
spruce_numeric_multiple(...)

spruce_class_multiple(...)

spruce_prob_multiple(...)
```

## Arguments

- ...:

  Multiple vectors of predictions:

  - For `spruce_numeric_multiple()`, numeric vectors of equal size.

  - For `spruce_class_multiple()`, factors of "hard" class predictions
    of equal size.

  - For `spruce_prob_multiple()`, tibbles of equal size, which are the
    result of calling
    [`spruce_prob()`](https://hardhat.tidymodels.org/dev/reference/spruce.md)
    on each matrix of prediction probabilities.

  If the `...` are named, then this name will be used as the suffix on
  the resulting column name, otherwise a positional index will be used.

## Value

- For `spruce_numeric_multiple()`, a tibble of numeric columns named
  with the pattern `.pred_*`.

- For `spruce_class_multiple()`, a tibble of factor columns named with
  the pattern `.pred_class_*`.

- For `spruce_prob_multiple()`, a tibble of data frame columns named
  with the pattern `.pred_*`.

## Examples

``` r
spruce_numeric_multiple(1:3, foo = 2:4)
#> # A tibble: 3 × 2
#>   .pred_1 .pred_foo
#>     <int>     <int>
#> 1       1         2
#> 2       2         3
#> 3       3         4

spruce_class_multiple(
  one_step = factor(c("a", "b", "c")),
  two_step = factor(c("a", "c", "c"))
)
#> # A tibble: 3 × 2
#>   .pred_class_one_step .pred_class_two_step
#>   <fct>                <fct>               
#> 1 a                    a                   
#> 2 b                    c                   
#> 3 c                    c                   

one_step <- matrix(c(.3, .7, .0, .1, .3, .6), nrow = 2, byrow = TRUE)
two_step <- matrix(c(.2, .7, .1, .2, .4, .4), nrow = 2, byrow = TRUE)
binary <- matrix(c(.5, .5, .4, .6), nrow = 2, byrow = TRUE)

spruce_prob_multiple(
  one_step = spruce_prob(c("a", "b", "c"), one_step),
  two_step = spruce_prob(c("a", "b", "c"), two_step),
  binary = spruce_prob(c("yes", "no"), binary)
)
#> # A tibble: 2 × 3
#>   .pred_one_step$.pred_a .pred_two_step$.pred_a .pred_binary$.pred_yes
#>                    <dbl>                  <dbl>                  <dbl>
#> 1                    0.3                    0.2                    0.5
#> 2                    0.1                    0.2                    0.4
#> # ℹ 5 more variables: .pred_one_step$.pred_b <dbl>, $.pred_c <dbl>,
#> #   .pred_two_step$.pred_b <dbl>, $.pred_c <dbl>,
#> #   .pred_binary$.pred_no <dbl>
```
