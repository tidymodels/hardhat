# Create a new preprocessing blueprint

These are the base classes for creating new preprocessing blueprints.
All blueprints inherit from the one created by `new_blueprint()`, and
the default method specific blueprints inherit from the other three
here.

If you want to create your own processing blueprint for a specific
method, generally you will subclass one of the method specific
blueprints here. If you want to create a completely new preprocessing
blueprint for a totally new preprocessing method (i.e. not the formula,
xy, or recipe method) then you should subclass `new_blueprint()`.

In addition to creating a blueprint subclass, you will likely also need
to provide S3 methods for
[`run_mold()`](https://hardhat.tidymodels.org/dev/reference/run-mold.md)
and
[`run_forge()`](https://hardhat.tidymodels.org/dev/reference/run-forge.md)
for your subclass.

## Usage

``` r
new_formula_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  ptypes = NULL,
  formula = NULL,
  indicators = "traditional",
  composition = "tibble",
  ...,
  subclass = character()
)

new_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  strings_as_factors = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  ...,
  subclass = character()
)

new_xy_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "tibble",
  ptypes = NULL,
  ...,
  subclass = character()
)

new_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  composition = "tibble",
  ptypes = NULL,
  ...,
  subclass = character()
)
```

## Arguments

- intercept:

  A logical. Should an intercept be included in the processed data? This
  information is used by the `process` function in the `mold` and
  `forge` function list.

- allow_novel_levels:

  A logical. Should novel factor levels be allowed at prediction time?
  This information is used by the `clean` function in the `forge`
  function list, and is passed on to
  [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md).

- ptypes:

  Either `NULL`, or a named list with 2 elements, `predictors` and
  `outcomes`, both of which are 0-row tibbles. `ptypes` is generated
  automatically at
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) time
  and is used to validate `new_data` at prediction time.

- formula:

  Either `NULL`, or a formula that specifies how the predictors and
  outcomes should be preprocessed. This argument is set automatically at
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) time.

- indicators:

  A single character string. Control how factors are expanded into dummy
  variable indicator columns. One of:

  - `"traditional"` - The default. Create dummy variables using the
    traditional
    [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
    infrastructure. Generally this creates `K - 1` indicator columns for
    each factor, where `K` is the number of levels in that factor.

  - `"none"` - Leave factor variables alone. No expansion is done.

  - `"one_hot"` - Create dummy variables using a one-hot encoding
    approach that expands unordered factors into all `K` indicator
    columns, rather than `K - 1`.

- composition:

  Either "tibble", "matrix", or "dgCMatrix" for the format of the
  processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
  the predictors must be numeric after the preprocessing method has been
  applied; otherwise an error is thrown.

- ...:

  Name-value pairs for additional elements of blueprints that subclass
  this blueprint.

- subclass:

  A character vector. The subclasses of this blueprint.

- fresh:

  Should already trained operations be re-trained when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  called?

- strings_as_factors:

  Should character columns be converted to factors when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  called?

- recipe:

  Either `NULL`, or an unprepped recipe. This argument is set
  automatically at
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) time.

## Value

A preprocessing blueprint, which is a list containing the inputs used as
arguments to the function, along with a class specific to the type of
blueprint being created.
