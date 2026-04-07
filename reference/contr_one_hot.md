# Contrast function for one-hot encodings

This contrast function produces a model matrix that has indicator
columns for each level of each factor.

## Usage

``` r
contr_one_hot(n, contrasts = TRUE, sparse = FALSE)
```

## Arguments

- n:

  A vector of character factor levels (of length \>=1) or the number of
  unique levels (\>= 1).

- contrasts:

  This argument is for backwards compatibility and only the default of
  `TRUE` is supported.

- sparse:

  This argument is for backwards compatibility and only the default of
  `FALSE` is supported.

## Value

A diagonal matrix that is `n`-by-`n`.

## Details

By default,
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) generates
binary indicator variables for factor predictors. When the formula does
not remove an intercept, an incomplete set of indicators are created; no
indicator is made for the first level of the factor.

For example, `species` and `island` both have three levels but
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) creates
two indicator variables for each:

    library(dplyr)
    library(modeldata)

    ##
    ## Attaching package: 'modeldata'

    ## The following object is masked from 'package:datasets':
    ##
    ##     penguins

    data(penguins)

    levels(penguins$species)

    ## [1] "Adelie"    "Chinstrap" "Gentoo"

    levels(penguins$island)

    ## [1] "Biscoe"    "Dream"     "Torgersen"

    model.matrix(~ species + island, data = penguins) |>
      colnames()

    ## [1] "(Intercept)"      "speciesChinstrap" "speciesGentoo"    "islandDream"
    ## [5] "islandTorgersen"

For a formula with no intercept, the first factor is expanded to
indicators for *all* factor levels but all other factors are expanded to
all but one (as above):

    model.matrix(~ 0 + species + island, data = penguins) |>
      colnames()

    ## [1] "speciesAdelie"    "speciesChinstrap" "speciesGentoo"    "islandDream"
    ## [5] "islandTorgersen"

For inference, this hybrid encoding can be problematic.

To generate all indicators, use this contrast:

    # Switch out the contrast method
    old_contr <- options("contrasts")$contrasts
    new_contr <- old_contr
    new_contr["unordered"] <- "contr_one_hot"
    options(contrasts = new_contr)

    model.matrix(~ species + island, data = penguins) |>
      colnames()

    ## [1] "(Intercept)"      "speciesAdelie"    "speciesChinstrap" "speciesGentoo"
    ## [5] "islandBiscoe"     "islandDream"      "islandTorgersen"

    options(contrasts = old_contr)

Removing the intercept here does not affect the factor encodings.
