
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hardhat

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/DavisVaughan/hardhat.svg?branch=master)](https://travis-ci.org/DavisVaughan/hardhat)
[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/hardhat/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/hardhat?branch=master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/hardhat)](https://cran.r-project.org/package=hardhat)
<!-- badges: end -->

## Introduction

hardhat is designed to ease the creation of new modeling packages, while
simultaneously promoting good R modeling package standards as laid out
by the set of opinionated [Conventions for R Modeling
Packages](https://tidymodels.github.io/model-implementation-principles/).

The idea is to take as much of the burden around creating a good
interface off the developer as possible, and instead let them focus on
writing the core implementation of the model. This benefits not only the
developer, but also the user of the modeling package, as the
standardization allows users to build a set of “expectations” around
what any modeling function should return, and how they should interact
with it.

``` r
library(hardhat)
```

## Installation

You can install the released version of hardhat from
[CRAN](https://CRAN.R-project.org) with:

``` r
# no you cannot
install.packages("hardhat")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("DavisVaughan/hardhat")
```

## Example

hardhat will mainly be useful to developers, and it allows you to
develop new modeling packages that implement formula, data frame,
matrix, and recipes interfaces with ease.

There are a number of useful functions in hardhat, but two of the most
important ones are `mold()` and `forge()`.

### `mold()`

`mold()` readies input data for ingestion into a modeling function. It
is to be called from the interface layer of your modeling package. For
instance, if you have a top level function called `linear_regression()`
that accepts input from the user, you would call `mold()` inside of
`linear_regression()`, before passing on the input to the actual
implementation function, which should be separated from the top level
interface and might be called `linear_regression_impl()`.

If your modeling function has a formula interface, `mold()` will call
`model.frame()` and `model.matrix()` for you, storing important
information such as the predictor factor levels and the class of each
predictor and outcome column.

``` r
iris_train <- iris[1:100,]
iris_test <- iris[101:150,]

processed <- mold(log(Sepal.Length) ~ Species + Petal.Width, iris_train)
```

The return value of `mold()` has three things. The `predictors`, the
`outcomes`, and a `preprocessor` that varies based on the interface you
are using. Notice that the default behavior for `mold()` is to *not* add
an intercept column. This is completely controlled by the `intercept`
argument and not by the formula itself.

``` r
processed$predictors
#> # A tibble: 100 x 4
#>    Speciessetosa Speciesversicolor Speciesvirginica Petal.Width
#>            <dbl>             <dbl>            <dbl>       <dbl>
#>  1             1                 0                0         0.2
#>  2             1                 0                0         0.2
#>  3             1                 0                0         0.2
#>  4             1                 0                0         0.2
#>  5             1                 0                0         0.2
#>  6             1                 0                0         0.4
#>  7             1                 0                0         0.3
#>  8             1                 0                0         0.2
#>  9             1                 0                0         0.2
#> 10             1                 0                0         0.1
#> # … with 90 more rows

processed$outcomes
#> # A tibble: 100 x 1
#>    `log(Sepal.Length)`
#>                  <dbl>
#>  1                1.63
#>  2                1.59
#>  3                1.55
#>  4                1.53
#>  5                1.61
#>  6                1.69
#>  7                1.53
#>  8                1.61
#>  9                1.48
#> 10                1.59
#> # … with 90 more rows
```

The preprocessor stores a number of things that are useful to any model
when it is time to make predictions on new data.

``` r
names(processed$preprocessor)
#> [1] "engine"     "intercept"  "predictors" "outcomes"   "indicators"
```

For instance, the original levels and classes of the predictors are
stored automatically.

``` r
processed$preprocessor$predictors$levels
#> $Species
#> [1] "setosa"     "versicolor" "virginica"

processed$preprocessor$predictors$classes
#> $Species
#> [1] "factor"
#> 
#> $Petal.Width
#> [1] "numeric"
```

There is also an interface for the XY method (providing the predictors
and outcome directly to `x` and `y`). The preprocessing that occurs here
is minimal, but you can add an intercept to `x` with `intercept = TRUE`.

Notice how even though `y` is a vector, the `outcomes` slot of the
return value *always* holds a tibble. Because `y` is not a data frame /
matrix with existing column names, a default name of `".outcome"` is
used.

``` r
x <- iris_train[, c("Species", "Petal.Width")]
y <- iris_train$Sepal.Length

processed_xy <- mold(x, y, intercept = TRUE)

processed_xy$predictors
#> # A tibble: 100 x 3
#>    `(Intercept)` Species Petal.Width
#>            <int> <fct>         <dbl>
#>  1             1 setosa          0.2
#>  2             1 setosa          0.2
#>  3             1 setosa          0.2
#>  4             1 setosa          0.2
#>  5             1 setosa          0.2
#>  6             1 setosa          0.4
#>  7             1 setosa          0.3
#>  8             1 setosa          0.2
#>  9             1 setosa          0.2
#> 10             1 setosa          0.1
#> # … with 90 more rows

processed_xy$outcomes
#> # A tibble: 100 x 1
#>    .outcome
#>       <dbl>
#>  1      5.1
#>  2      4.9
#>  3      4.7
#>  4      4.6
#>  5      5  
#>  6      5.4
#>  7      4.6
#>  8      5  
#>  9      4.4
#> 10      4.9
#> # … with 90 more rows
```

Finally, there is an interface for `recipes`. It calls `recipes::prep()`
on the recipe for you.

``` r
suppressPackageStartupMessages(library(recipes))

rec <- recipe(Sepal.Length ~ Species + Petal.Width, iris_train) %>%
  step_log(Sepal.Length) %>%
  step_dummy(Species)

processed_rec <- mold(rec, iris_train)

processed_rec$predictors
#> # A tibble: 100 x 3
#>    Petal.Width Species_versicolor Species_virginica
#>          <dbl>              <dbl>             <dbl>
#>  1         0.2                  0                 0
#>  2         0.2                  0                 0
#>  3         0.2                  0                 0
#>  4         0.2                  0                 0
#>  5         0.2                  0                 0
#>  6         0.4                  0                 0
#>  7         0.3                  0                 0
#>  8         0.2                  0                 0
#>  9         0.2                  0                 0
#> 10         0.1                  0                 0
#> # … with 90 more rows
```

### `forge()`

`forge()` takes `new_data` and applies the same preprocessing steps that
happened to the data used in training the model. It is to be called from
`predict()`, or potentially from a cross validation function where you
will use `predict()` repeatedly to measure performance of different
folds or different hyperparameters.

Say you fit a model and called `mold()` from your fitting function. When
you return the model object, you should attach the `preprocessor` that
you get with the output of `mold()` onto the model object (using the
model constructor, `new_base_model()`, makes this easier to do). Then
when you call `predict()` on that model object along with `new_data`,
you should call `forge()` inside of the `predict()` method with the
stored `preprocessor` and the `new_data`.

``` r
processed_test <- forge(
  preprocessor = processed$preprocessor, 
  new_data = iris_test,
  outcomes = TRUE
)
```

`forge()` always returns a list with two things. The first is a tibble
containing the preprocessed `predictors`. The second is optionally the
preprocessed `outcomes` if you are performing cross validation and used
a formula or recipes interface. Because we used the formula interface to
generate the `processed` object, we could set `outcomes = TRUE` above to
also return the processed outcome column.

``` r
processed_test$predictors
#> # A tibble: 50 x 4
#>    Speciessetosa Speciesversicolor Speciesvirginica Petal.Width
#>            <dbl>             <dbl>            <dbl>       <dbl>
#>  1             0                 0                1         2.5
#>  2             0                 0                1         1.9
#>  3             0                 0                1         2.1
#>  4             0                 0                1         1.8
#>  5             0                 0                1         2.2
#>  6             0                 0                1         2.1
#>  7             0                 0                1         1.7
#>  8             0                 0                1         1.8
#>  9             0                 0                1         1.8
#> 10             0                 0                1         2.5
#> # … with 40 more rows

processed_test$outcomes
#> # A tibble: 50 x 1
#>    `log(Sepal.Length)`
#>                  <dbl>
#>  1                1.84
#>  2                1.76
#>  3                1.96
#>  4                1.84
#>  5                1.87
#>  6                2.03
#>  7                1.59
#>  8                1.99
#>  9                1.90
#> 10                1.97
#> # … with 40 more rows
```

The nice thing about `forge()` is that the `preprocessor` remembers a
lot of information about what happened at fit time, and keeps the user
from shooting themselves in the foot at prediction time.

For instance, each predictor used at fit time has to have the same class
at prediction time.

``` r
iris_test_bad <- iris_test

# Turning Species into a character column rather than
# a factor
iris_test_bad$Species <- as.character(iris_test_bad$Species)

forge(processed$preprocessor, iris_test_bad)
#> Error: Some columns in `new_data` have an incorrect class:
#> `Species`: `character` should be `factor`.
```

And each predictor column has to exist in `new_data`.

``` r
# Removing species alltogether
iris_test_bad$Species <- NULL

forge(processed$preprocessor, iris_test_bad)
#> Error: `new_data` is missing the following required predictors:
#> Species
```

And new levels in any of the predictors throw a warning and are coerced
to `NA`.

``` r
iris_test_bad <- iris
iris_test_bad$Species <- as.character(iris_test_bad$Species)
iris_test_bad$Species[1] <- "new_level"
iris_test_bad$Species  <- factor(iris_test_bad$Species)

levels(iris_test_bad$Species)
#> [1] "new_level"  "setosa"     "versicolor" "virginica"

processed_bad_test <- forge(processed$preprocessor, iris_test_bad)
#> Warning: The following factor levels are new for column, `Species`, and
#> have been coerced to `NA`: 'new_level'.

processed_bad_test$predictors
#> # A tibble: 150 x 4
#>    Speciessetosa Speciesversicolor Speciesvirginica Petal.Width
#>            <dbl>             <dbl>            <dbl>       <dbl>
#>  1            NA                NA               NA         0.2
#>  2             1                 0                0         0.2
#>  3             1                 0                0         0.2
#>  4             1                 0                0         0.2
#>  5             1                 0                0         0.2
#>  6             1                 0                0         0.4
#>  7             1                 0                0         0.3
#>  8             1                 0                0         0.2
#>  9             1                 0                0         0.2
#> 10             1                 0                0         0.1
#> # … with 140 more rows
```
