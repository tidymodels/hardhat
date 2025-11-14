# Forging data for predictions

``` r
library(hardhat)
library(modeldata)
#> 
#> Attaching package: 'modeldata'
#> The following object is masked from 'package:datasets':
#> 
#>     penguins

data(penguins)
penguins <- na.omit(penguins)
```

## Introduction

The counterpart to
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) (which
you can read all about in
[`vignette("mold", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/mold.md)),
is [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).
Where [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
is used to preprocess your training data,
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) is
used to preprocess new data that you are going to use to generate
predictions from your model.

Like [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md),
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) is
not intended to be used interactively. Instead, it should be called from
the [`predict()`](https://rdrr.io/r/stats/predict.html) method for your
model. To learn more about using
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) in a
modeling package, see
[`vignette("package", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/package.md).
The rest of this vignette will be focused on the many features that
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
offers.

## Connection with mold()

When [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
used, one of the returned objects is an `blueprint`. This is the key to
preprocessing new data with
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md). For
instance, assume you’ve called
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) like
so:

``` r
penguin_train <- penguins[1:300,]
penguin_test  <- penguins[-(1:300),]
```

``` r
penguin_form <- mold(
  log(body_mass_g) ~ species + bill_length_mm, 
  penguin_train, 
  blueprint = default_formula_blueprint(indicators = "none")
)

formula_eng <- penguin_form$blueprint

formula_eng
#> Formula blueprint:
#> # Predictors: 2
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> Indicators: none
#> 
```

A formula blueprint is returned here, which knows about the predictors
and outcomes that were used at training time, and knows that you don’t
want to expand `species` into dummy variables by setting
`indicators = "none"`.

When it is time to [`predict()`](https://rdrr.io/r/stats/predict.html)
on new data, that data is passed on to
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) along
with the `blueprint` we just created.

``` r
forge(penguin_test, formula_eng)
#> $predictors
#> # A tibble: 33 × 2
#>    bill_length_mm species  
#>             <dbl> <fct>    
#>  1           47.5 Chinstrap
#>  2           47.6 Chinstrap
#>  3           52   Chinstrap
#>  4           46.9 Chinstrap
#>  5           53.5 Chinstrap
#>  6           49   Chinstrap
#>  7           46.2 Chinstrap
#>  8           50.9 Chinstrap
#>  9           45.5 Chinstrap
#> 10           50.9 Chinstrap
#> # ℹ 23 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$offset
#> NULL
```

Note that in `predictors`, `species` was not expanded because the
`blueprint` knew about the preprocessing options that were set when
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) was
called.

[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
always returns three things, and they should look familiar to you if you
have used
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).

- `predictors` holds a tibble of the predictors.

- `outcomes` is returned as `NULL` by default, because most
  [`predict()`](https://rdrr.io/r/stats/predict.html) methods assume you
  only have access to the new predictors. Alternatively, as you will
  read in a moment, this can contain a tibble of the new outcomes.

- `extras` varies per blueprint, but is a catch-all slot to hold the
  same kind of extra objects that were returned by the blueprint when
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) was
  called.

## Outcomes

Generally when generating predictions you only need to know about the
new predictors. However, when performing resampling you will need to
have the processed outcomes as well so you can compute cross validated
performance statistics and decide between multiple models, or choose
between hyperparameters.

You can easily request the outcomes as well with `outcomes = TRUE`. Just
like with the predictors, these get processed using the same steps as
done to the outcomes at fit time.

``` r
forge(penguin_test, formula_eng, outcomes = TRUE)
#> $predictors
#> # A tibble: 33 × 2
#>    bill_length_mm species  
#>             <dbl> <fct>    
#>  1           47.5 Chinstrap
#>  2           47.6 Chinstrap
#>  3           52   Chinstrap
#>  4           46.9 Chinstrap
#>  5           53.5 Chinstrap
#>  6           49   Chinstrap
#>  7           46.2 Chinstrap
#>  8           50.9 Chinstrap
#>  9           45.5 Chinstrap
#> 10           50.9 Chinstrap
#> # ℹ 23 more rows
#> 
#> $outcomes
#> # A tibble: 33 × 1
#>    `log(body_mass_g)`
#>                 <dbl>
#>  1               8.27
#>  2               8.26
#>  3               8.48
#>  4               7.90
#>  5               8.41
#>  6               8.28
#>  7               8.20
#>  8               8.17
#>  9               8.16
#> 10               8.21
#> # ℹ 23 more rows
#> 
#> $extras
#> $extras$offset
#> NULL
```

## Validation

One of the most useful things about
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) is
its robustness against malformed new data. It isn’t unreasonable to
enforce that the new data a user provides at prediction time should have
the same *type* as the data used at fit time. *Type* is defined in the
[vctrs](https://vctrs.r-lib.org/articles/type-size.html) sense, and for
our uses essentially means that a number of checks on the test data have
to pass, including:

- The column names of the testing data and training data must be the
  same.

- The *type* of each column of the testing data must be the same as the
  columns found in the training data. This means:

  - The classes must be the same (e.g. if it was a factor in training,
    it must be a factor in testing).

  - The attributes must be the same (e.g. the levels of the factors must
    also be the same).

Almost all of this validation is possible through the use of
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html),
and is called for you by
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).

### Column existence

The easiest example to demonstrate is missing columns in the testing
data. [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
won’t let you continue until all of the required predictors used at
training are also present in the new data.

``` r
test_missing_column <- subset(penguin_test, select = -species)

forge(test_missing_column, formula_eng)
#> Error in `forge()`:
#> ! The required column "species" is missing.
```

### Column types

After an initial scan for the column names is done, a deeper scan of
each column is performed, checking the type of that column. For
instance, what happens if the new `species` column was a double, not a
factor?

``` r
test_species_double <- penguin_test
test_species_double$species <- as.double(test_species_double$species)

forge(test_species_double, formula_eng)
#> Error in `scream()`:
#> ! Can't convert `data$species` <double> to match type of `species` <factor<b22a0>>.
```

An error is thrown, indicating that a double can’t be cast to a factor.

### Lossless conversion

The error message above suggests that in some cases you *can*
automatically cast from one type to another, and in fact that is true!
Rather than being a double, what if `species` was just a character?

``` r
test_species_character <- penguin_test
test_species_character$species <- as.character(test_species_character$species)

forged_char <- forge(test_species_character, formula_eng)

forged_char$predictors
#> # A tibble: 33 × 2
#>    bill_length_mm species  
#>             <dbl> <fct>    
#>  1           47.5 Chinstrap
#>  2           47.6 Chinstrap
#>  3           52   Chinstrap
#>  4           46.9 Chinstrap
#>  5           53.5 Chinstrap
#>  6           49   Chinstrap
#>  7           46.2 Chinstrap
#>  8           50.9 Chinstrap
#>  9           45.5 Chinstrap
#> 10           50.9 Chinstrap
#> # ℹ 23 more rows

class(forged_char$predictors$species)
#> [1] "factor"

levels(forged_char$predictors$species)
#> [1] "Adelie"    "Chinstrap" "Gentoo"
```

Interesting, so in this case we can actually convert to a factor, and
the class and even the levels are all restored. The key here is that
this was a *lossless* conversion. We lost no information when converting
the character `species` to a factor because the unique character values
were a subset of the original levels.

An example of a conversion that would be lossy is if the character
`species` column had a value that was *not* a level in the training
data.

``` r
test_species_lossy <- penguin_test
test_species_lossy$species <- as.character(test_species_lossy$species)
test_species_lossy$species[2] <- "im new!"

forged_lossy <- forge(test_species_lossy, formula_eng)
#> Warning: Novel level found in column "species": "im new!".
#> ℹ The level has been removed, and values have been coerced to <NA>.

forged_lossy$predictors
#> # A tibble: 33 × 2
#>    bill_length_mm species  
#>             <dbl> <fct>    
#>  1           47.5 Chinstrap
#>  2           47.6 NA       
#>  3           52   Chinstrap
#>  4           46.9 Chinstrap
#>  5           53.5 Chinstrap
#>  6           49   Chinstrap
#>  7           46.2 Chinstrap
#>  8           50.9 Chinstrap
#>  9           45.5 Chinstrap
#> 10           50.9 Chinstrap
#> # ℹ 23 more rows
```

In this case:

- A lossy warning is thrown

- The `species` column is still converted to a factor with the right
  levels

- The novel level is removed and its value is set to `NA`

## Recipes and forge()

Just like with the formula method, a recipe can be used as the
preprocessor at fit and prediction time. `hardhat` handles calling
[`prep()`](https://recipes.tidymodels.org/reference/prep.html),
[`juice()`](https://recipes.tidymodels.org/reference/juice.html), and
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) for you
at the right times. For instance, say we have a recipe that just creates
dummy variables out of `species`.

``` r
library(recipes)

rec <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) |>
  step_dummy(species)

penguin_recipe <- mold(rec, penguin_train)

penguin_recipe$predictors
#> # A tibble: 300 × 3
#>    body_mass_g species_Chinstrap species_Gentoo
#>          <int>             <dbl>          <dbl>
#>  1        3750                 0              0
#>  2        3800                 0              0
#>  3        3250                 0              0
#>  4        3450                 0              0
#>  5        3650                 0              0
#>  6        3625                 0              0
#>  7        4675                 0              0
#>  8        3200                 0              0
#>  9        3800                 0              0
#> 10        4400                 0              0
#> # ℹ 290 more rows
```

The blueprint is a `recipe` blueprint.

``` r
recipe_eng <- penguin_recipe$blueprint

recipe_eng
#> Recipe blueprint:
#> # Predictors: 2
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
```

When we
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md), we
can request `outcomes` to have the predictors and outcomes separated
like with the formula method.

``` r
forge(penguin_test, recipe_eng, outcomes = TRUE)
#> $predictors
#> # A tibble: 33 × 3
#>    body_mass_g species_Chinstrap species_Gentoo
#>          <int>             <dbl>          <dbl>
#>  1        3900                 1              0
#>  2        3850                 1              0
#>  3        4800                 1              0
#>  4        2700                 1              0
#>  5        4500                 1              0
#>  6        3950                 1              0
#>  7        3650                 1              0
#>  8        3550                 1              0
#>  9        3500                 1              0
#> 10        3675                 1              0
#> # ℹ 23 more rows
#> 
#> $outcomes
#> # A tibble: 33 × 1
#>    bill_length_mm
#>             <dbl>
#>  1           47.5
#>  2           47.6
#>  3           52  
#>  4           46.9
#>  5           53.5
#>  6           49  
#>  7           46.2
#>  8           50.9
#>  9           45.5
#> 10           50.9
#> # ℹ 23 more rows
#> 
#> $extras
#> $extras$roles
#> NULL
```

### A note on recipes

One complication with `recipes` is that, in the
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) step, the
processing happens to the predictors and the outcomes all together. This
means that you might run into the situation where the outcomes seem to
be required to
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md), even
if you aren’t requesting them.

``` r
rec2 <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) |>
  step_dummy(species) |>
  step_center(bill_length_mm) # Here we modify the outcome

penguin_recipe2 <- mold(rec2, penguin_train)

recipe_eng_log_outcome <- penguin_recipe2$blueprint
```

If our `new_data` doesn’t have the outcome, baking this recipe will fail
even if we don’t request that the outcomes are returned by
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).

``` r
penguin_test_no_outcome <- subset(penguin_test, select = -bill_length_mm)

forge(penguin_test_no_outcome, recipe_eng_log_outcome)
#> Error in `step_center()`:
#> ! The following required column is missing from `new_data`:
#>   bill_length_mm.
```

The way around this is to use the built-in recipe argument, `skip`, on
the step containing the outcome. This skips the processing of that step
at [`bake()`](https://recipes.tidymodels.org/reference/bake.html) time.

``` r
rec3 <- recipe(bill_length_mm ~ body_mass_g + species, penguin_train) |>
  step_dummy(species) |>
  step_center(bill_length_mm, skip = TRUE)

penguin_recipe3 <- mold(rec3, penguin_train)

recipe_eng_skip_outcome <- penguin_recipe3$blueprint

forge(penguin_test_no_outcome, recipe_eng_skip_outcome)
#> $predictors
#> # A tibble: 33 × 3
#>    body_mass_g species_Chinstrap species_Gentoo
#>          <int>             <dbl>          <dbl>
#>  1        3900                 1              0
#>  2        3850                 1              0
#>  3        4800                 1              0
#>  4        2700                 1              0
#>  5        4500                 1              0
#>  6        3950                 1              0
#>  7        3650                 1              0
#>  8        3550                 1              0
#>  9        3500                 1              0
#> 10        3675                 1              0
#> # ℹ 23 more rows
#> 
#> $outcomes
#> NULL
#> 
#> $extras
#> $extras$roles
#> NULL
```

There is a tradeoff here that you need to be aware of.

- If you are just interested in generating predictions on completely new
  data, you can safely use `skip = TRUE` because you will almost never
  have access to the corresponding true outcomes to preprocess and
  compare against.

- If you know you need to do resampling, you will likely have access to
  the outcomes during the resampling step so you can cross-validate the
  performance. In this case, you can’t set `skip = TRUE` because then
  the outcomes won’t be processed, but since you have access to them,
  you shouldn’t need to.

For example, if we used `penguin_test` with the above recipe (which has
the outcome), `bill_length_mm` wouldn’t get centered when
[`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) is
called. But we probably would not have skipped that step if we knew that
our test data would have the outcome.

``` r
forge(penguin_test, recipe_eng_skip_outcome, outcomes = TRUE)$outcomes
#> # A tibble: 33 × 1
#>    bill_length_mm
#>             <dbl>
#>  1           47.5
#>  2           47.6
#>  3           52  
#>  4           46.9
#>  5           53.5
#>  6           49  
#>  7           46.2
#>  8           50.9
#>  9           45.5
#> 10           50.9
#> # ℹ 23 more rows

# Notice that the `outcome` values haven't been centered
# and are the same as before
head(penguin_test$bill_length_mm)
#> [1] 47.5 47.6 52.0 46.9 53.5 49.0
```
