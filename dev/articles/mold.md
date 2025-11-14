# Molding data for modeling

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

For most modeling functions, data must be accepted from the user in some
format where the *outcomes* and *predictors* are both specified. The
next step is often to validate and preprocess that input in some way to
prepare it for the actual modeling implementation function. For example,
when a formula method is used, R provides some infrastructure for
preprocessing the user input through the
[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) and
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) functions.

But the formula method is not the only way to specify modeling terms.
There is also an XY method, where `x` and `y` are supplied directly,
and, recently, a `recipe` implementation can be used to preprocess data
using a set of sequential steps.

As a developer, you likely won’t want to care about the details of how
each of these methods work, but (hopefully) still want to provide all
three of these interfaces for your shiny new model.
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) makes
this easy on you, and takes care of the details of preprocessing user
input from any of these methods.

The intended use of
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is to
be called from your user facing modeling function. To see that in
action, have a look at the vignette found here:
[`vignette("package", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/package.md).
The rest of this vignette will be focused on the various different ways
to use [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md),
but keep in mind that generally it is not used as an interactive
function like this.

## A First Example

The most familiar interface for R users is likely the formula interface.
In this case, terms are specified using the formula notation:
`outcomes ~ predictors`. Generally, as a developer, you have to then
call [`model.frame()`](https://rdrr.io/r/stats/model.frame.html) and
[`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html) on this
result to coerce it into the right format for ingestion into your model.
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) handles
all of that for you.

``` r
penguin_form <- mold(body_mass_g ~ log(bill_length_mm), penguins)

names(penguin_form)
#> [1] "predictors" "outcomes"   "blueprint"  "extras"
```

[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) returns
four things. Two of them are immediately useful, and are almost always
applicable to the modeling implementation you have created. The first is
the `predictors`, returned as a tibble. All of the required processing
has been done for you, so you just have to focus on the modeling
implementation.

``` r
penguin_form$predictors
#> # A tibble: 333 × 1
#>    `log(bill_length_mm)`
#>                    <dbl>
#>  1                  3.67
#>  2                  3.68
#>  3                  3.70
#>  4                  3.60
#>  5                  3.67
#>  6                  3.66
#>  7                  3.67
#>  8                  3.72
#>  9                  3.65
#> 10                  3.54
#> # ℹ 323 more rows
```

Second is the `outcomes`, also returned as a tibble. While not used
here, any processing on the outcome that was specified in the formula
would also be done here.

``` r
penguin_form$outcomes
#> # A tibble: 333 × 1
#>    body_mass_g
#>          <int>
#>  1        3750
#>  2        3800
#>  3        3250
#>  4        3450
#>  5        3650
#>  6        3625
#>  7        4675
#>  8        3200
#>  9        3800
#> 10        4400
#> # ℹ 323 more rows
```

Beyond these two elements,
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) also
returns a slot for any `extras` that might have been generated during
preprocessing, but aren’t specifically predictors or outcomes. For
example, an [`offset()`](https://rdrr.io/r/stats/offset.html) can be
specified directly in the formula, but isn’t technically a predictor.

``` r
mold(body_mass_g ~ log(bill_length_mm) + offset(bill_depth_mm), penguins)$extras
#> $offset
#> # A tibble: 333 × 1
#>    .offset
#>      <dbl>
#>  1    18.7
#>  2    17.4
#>  3    18  
#>  4    19.3
#>  5    20.6
#>  6    17.8
#>  7    19.6
#>  8    17.6
#>  9    21.2
#> 10    21.1
#> # ℹ 323 more rows
```

Lastly, [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
returns a very important object, the `blueprint`. This is responsible
for knowing how to preprocess both the training data, and any new data
at prediction time. As a developer, you should attach the `blueprint` to
your model object before returning it to the user. For more information
about this, see the package development vignette,
[`vignette("package", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/package.md).

## blueprints

As mentioned above, one of the objects that
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) returns
is an `blueprint` responsible for controlling the preprocessing. There
are multiple blueprints available in `hardhat`, but when you call
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) one is
selected automatically for you. The following two calls generate the
same result, using the default formula blueprint.

``` r
identical(
  mold(~ body_mass_g, penguins), 
  mold(~ body_mass_g, penguins, blueprint = default_formula_blueprint())
)
#> [1] TRUE
```

Each blueprint can be tweaked to change how the processing for that
interface occurs, and the options vary per blueprint. To understand why
you’d ever want to do this, read on!

## Formulas

Now that you have a basic idea of how
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) works,
we can talk about some of the more interesting functionality.

### Intercepts

One challenge with the standard formula interface is that, by default,
intercepts are always implicitly present and are added to your data set
automatically. This works great for the simple regression case. However,
other models might either always require or never allow an intercept,
but still use the formula interface because of its convenience (for
example, `earth`). This has led to many ad hoc solutions that prevent
the user from removing or adding an intercept.

To get around this,
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) will
never add an intercept by default. Instead, the addition of an intercept
is completely controlled by the formula blueprint argument, `intercept`.

``` r
no_intercept <- mold(~ body_mass_g, penguins)

no_intercept$predictors
#> # A tibble: 333 × 1
#>    body_mass_g
#>          <dbl>
#>  1        3750
#>  2        3800
#>  3        3250
#>  4        3450
#>  5        3650
#>  6        3625
#>  7        4675
#>  8        3200
#>  9        3800
#> 10        4400
#> # ℹ 323 more rows
```

``` r
with_intercept <- mold(
  ~ body_mass_g, penguins, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

with_intercept$predictors
#> # A tibble: 333 × 2
#>    `(Intercept)` body_mass_g
#>            <dbl>       <dbl>
#>  1             1        3750
#>  2             1        3800
#>  3             1        3250
#>  4             1        3450
#>  5             1        3650
#>  6             1        3625
#>  7             1        4675
#>  8             1        3200
#>  9             1        3800
#> 10             1        4400
#> # ℹ 323 more rows
```

An error is thrown if an intercept removal term is specified:

``` r
mold(~ body_mass_g - 1, penguins)
#> Error in `mold()`:
#> ! `formula` must not contain the intercept removal term: `- 1`.

mold(~ body_mass_g + 0, penguins)
#> Error in `mold()`:
#> ! `formula` must not contain the intercept removal term: `+ 0`
#>   or `0 +`.
```

### Dummy variables

One of the nice things about the formula interface is that it expands
factors into dummy variable columns for you. Like intercepts, this is
great…until it isn’t. For example, `ranger` fits a random forest, which
can take factors directly, but still uses the formula notation. In this
case, it would be great if the factor columns specified as predictors
*weren’t* expanded. This is the job of the blueprint argument,
`indicators`.

``` r
expanded_dummies <- mold(~ body_mass_g + species, penguins)

expanded_dummies$predictors
#> # A tibble: 333 × 4
#>    body_mass_g speciesAdelie speciesChinstrap speciesGentoo
#>          <dbl>         <dbl>            <dbl>         <dbl>
#>  1        3750             1                0             0
#>  2        3800             1                0             0
#>  3        3250             1                0             0
#>  4        3450             1                0             0
#>  5        3650             1                0             0
#>  6        3625             1                0             0
#>  7        4675             1                0             0
#>  8        3200             1                0             0
#>  9        3800             1                0             0
#> 10        4400             1                0             0
#> # ℹ 323 more rows
```

``` r
non_expanded_dummies <- mold(
  ~ body_mass_g + species, penguins, 
  blueprint = default_formula_blueprint(indicators = "none")
)

non_expanded_dummies$predictors
#> # A tibble: 333 × 2
#>    body_mass_g species
#>          <dbl> <fct>  
#>  1        3750 Adelie 
#>  2        3800 Adelie 
#>  3        3250 Adelie 
#>  4        3450 Adelie 
#>  5        3650 Adelie 
#>  6        3625 Adelie 
#>  7        4675 Adelie 
#>  8        3200 Adelie 
#>  9        3800 Adelie 
#> 10        4400 Adelie 
#> # ℹ 323 more rows
```

*Note:* It’s worth mentioning that when an intercept is not present,
base R expands the first factor completely into `K` indicator columns
corresponding to the `K` levels present in that factor (also known as
one-hot encoding). Subsequent columns are expanded into the more
traditional `K - 1` columns. When an intercept is present, `K - 1`
columns are generated for all factor predictors.

``` r
k_cols <- mold(~ species, penguins)

k_minus_one_cols <- mold(
  ~ species, penguins, 
  blueprint = default_formula_blueprint(intercept = TRUE)
)

colnames(k_cols$predictors)
#> [1] "speciesAdelie"    "speciesChinstrap" "speciesGentoo"

colnames(k_minus_one_cols$predictors)
#> [1] "(Intercept)"      "speciesChinstrap" "speciesGentoo"
```

### Multivariate outcomes

One of the other frustrating things about working with the formula
method is that multivariate outcomes are a bit clunky to specify.

``` r
.f <- cbind(body_mass_g, bill_length_mm) ~ bill_depth_mm

frame <- model.frame(.f, penguins)

head(frame)
#>   cbind(body_mass_g, bill_length_mm).body_mass_g
#> 1                                         3750.0
#> 2                                         3800.0
#> 3                                         3250.0
#> 4                                         3450.0
#> 5                                         3650.0
#> 6                                         3625.0
#>   cbind(body_mass_g, bill_length_mm).bill_length_mm bill_depth_mm
#> 1                                              39.1          18.7
#> 2                                              39.5          17.4
#> 3                                              40.3          18.0
#> 4                                              36.7          19.3
#> 5                                              39.3          20.6
#> 6                                              38.9          17.8
```

This might look like 3 columns, but it is actually 2, where the first
column is named `cbind(body_mass_g, bill_length_mm)`, and it is actually
a matrix with 2 columns, `body_mass_g` and `bill_length_mm` inside it.

``` r
ncol(frame)
#> [1] 2

class(frame$`cbind(body_mass_g, bill_length_mm)`)
#> [1] "matrix" "array"

head(frame$`cbind(body_mass_g, bill_length_mm)`)
#>      body_mass_g bill_length_mm
#> [1,]        3750           39.1
#> [2,]        3800           39.5
#> [3,]        3250           40.3
#> [4,]        3450           36.7
#> [5,]        3650           39.3
#> [6,]        3625           38.9
```

The default formula blueprint used with
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) allows
you to specify multiple outcomes like you specify multiple predictors.
You can even do inline transformations of each outcome, although if you
are doing very much of that, I’d advise using a recipe instead.
`outcomes` then holds the two outcomes columns.

``` r
multivariate <- mold(body_mass_g + log(bill_length_mm) ~ bill_depth_mm, penguins)

multivariate$outcomes
#> # A tibble: 333 × 2
#>    body_mass_g `log(bill_length_mm)`
#>          <int>                 <dbl>
#>  1        3750                  3.67
#>  2        3800                  3.68
#>  3        3250                  3.70
#>  4        3450                  3.60
#>  5        3650                  3.67
#>  6        3625                  3.66
#>  7        4675                  3.67
#>  8        3200                  3.72
#>  9        3800                  3.65
#> 10        4400                  3.54
#> # ℹ 323 more rows
```

## XY

The second interface is the XY interface, useful when the predictors and
outcomes are specified separately.

``` r
x <- subset(penguins, select = -body_mass_g)
y <- subset(penguins, select =  body_mass_g)

penguin_xy <- mold(x, y)

penguin_xy$predictors
#> # A tibble: 333 × 6
#>    species island  bill_length_mm bill_depth_mm flipper_length_mm sex  
#>    <fct>   <fct>            <dbl>         <dbl>             <int> <fct>
#>  1 Adelie  Torger…           39.1          18.7               181 male 
#>  2 Adelie  Torger…           39.5          17.4               186 fema…
#>  3 Adelie  Torger…           40.3          18                 195 fema…
#>  4 Adelie  Torger…           36.7          19.3               193 fema…
#>  5 Adelie  Torger…           39.3          20.6               190 male 
#>  6 Adelie  Torger…           38.9          17.8               181 fema…
#>  7 Adelie  Torger…           39.2          19.6               195 male 
#>  8 Adelie  Torger…           41.1          17.6               182 fema…
#>  9 Adelie  Torger…           38.6          21.2               191 male 
#> 10 Adelie  Torger…           34.6          21.1               198 male 
#> # ℹ 323 more rows

penguin_xy$outcomes
#> # A tibble: 333 × 1
#>    body_mass_g
#>          <int>
#>  1        3750
#>  2        3800
#>  3        3250
#>  4        3450
#>  5        3650
#>  6        3625
#>  7        4675
#>  8        3200
#>  9        3800
#> 10        4400
#> # ℹ 323 more rows
```

This interface doesn’t do too much in the way of preprocessing, but it
does let you specify an `intercept` in the blueprint specific arguments.
Rather than
[`default_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_formula_blueprint.md),
this uses the
[`default_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md).

``` r
xy_with_intercept <- mold(x, y, blueprint = default_xy_blueprint(intercept = TRUE))

xy_with_intercept$predictors
#> # A tibble: 333 × 7
#>    `(Intercept)` species island    bill_length_mm bill_depth_mm
#>            <int> <fct>   <fct>              <dbl>         <dbl>
#>  1             1 Adelie  Torgersen           39.1          18.7
#>  2             1 Adelie  Torgersen           39.5          17.4
#>  3             1 Adelie  Torgersen           40.3          18  
#>  4             1 Adelie  Torgersen           36.7          19.3
#>  5             1 Adelie  Torgersen           39.3          20.6
#>  6             1 Adelie  Torgersen           38.9          17.8
#>  7             1 Adelie  Torgersen           39.2          19.6
#>  8             1 Adelie  Torgersen           41.1          17.6
#>  9             1 Adelie  Torgersen           38.6          21.2
#> 10             1 Adelie  Torgersen           34.6          21.1
#> # ℹ 323 more rows
#> # ℹ 2 more variables: flipper_length_mm <int>, sex <fct>
```

### Vector outcomes

`y` is a bit special in the XY interface, because in the univariate case
users might expect to be able to pass a vector, a 1 column data frame,
or a matrix.
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) is
prepared for all of those cases, but the vector case requires special
attention. To be consistent with all of the other
[`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
interfaces, the `outcomes` slot of the return value should be a tibble.
To achieve this when `y` is supplied as a vector, a default column name
is created, `".outcome"`.

``` r
mold(x, y$body_mass_g)$outcomes
#> # A tibble: 333 × 1
#>    .outcome
#>       <int>
#>  1     3750
#>  2     3800
#>  3     3250
#>  4     3450
#>  5     3650
#>  6     3625
#>  7     4675
#>  8     3200
#>  9     3800
#> 10     4400
#> # ℹ 323 more rows
```

## Recipe

The last of the three interfaces is the relatively new recipes
interface. The
[`default_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_recipe_blueprint.md)
knows how to
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) your
recipe, and
[`juice()`](https://recipes.tidymodels.org/reference/juice.html) it to
extract the predictors and the outcomes. This is by far the most
flexible way to preprocess your data.

``` r
library(recipes)

rec <- recipe(bill_length_mm ~ species + bill_depth_mm, penguins) |>
  step_log(bill_length_mm) |>
  step_dummy(species)

penguin_recipe <- mold(rec, penguins)

penguin_recipe$predictors
#> # A tibble: 333 × 3
#>    bill_depth_mm species_Chinstrap species_Gentoo
#>            <dbl>             <dbl>          <dbl>
#>  1          18.7                 0              0
#>  2          17.4                 0              0
#>  3          18                   0              0
#>  4          19.3                 0              0
#>  5          20.6                 0              0
#>  6          17.8                 0              0
#>  7          19.6                 0              0
#>  8          17.6                 0              0
#>  9          21.2                 0              0
#> 10          21.1                 0              0
#> # ℹ 323 more rows

penguin_recipe$outcomes
#> # A tibble: 333 × 1
#>    bill_length_mm
#>             <dbl>
#>  1           3.67
#>  2           3.68
#>  3           3.70
#>  4           3.60
#>  5           3.67
#>  6           3.66
#>  7           3.67
#>  8           3.72
#>  9           3.65
#> 10           3.54
#> # ℹ 323 more rows
```

The only special thing you can tweak with the recipe blueprint is
whether or not an intercept is added.

``` r
recipe_with_intercept <- mold(
  rec, penguins, 
  blueprint = default_recipe_blueprint(intercept = TRUE)
)

recipe_with_intercept$predictors
#> # A tibble: 333 × 4
#>    `(Intercept)` bill_depth_mm species_Chinstrap species_Gentoo
#>            <int>         <dbl>             <dbl>          <dbl>
#>  1             1          18.7                 0              0
#>  2             1          17.4                 0              0
#>  3             1          18                   0              0
#>  4             1          19.3                 0              0
#>  5             1          20.6                 0              0
#>  6             1          17.8                 0              0
#>  7             1          19.6                 0              0
#>  8             1          17.6                 0              0
#>  9             1          21.2                 0              0
#> 10             1          21.1                 0              0
#> # ℹ 323 more rows
```
