# Create a new default blueprint

This page contains the constructors for the default blueprints. They can
be extended if you want to add extra behavior on top of what the default
blueprints already do, but generally you will extend the non-default
versions of the constructors found in the documentation for
[`new_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md).

## Usage

``` r
new_default_formula_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  ptypes = NULL,
  formula = NULL,
  indicators = "traditional",
  composition = "tibble",
  terms = list(predictors = NULL, outcomes = NULL),
  levels = NULL,
  ...,
  subclass = character()
)

new_default_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  strings_as_factors = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  extra_role_ptypes = NULL,
  ...,
  subclass = character()
)

new_default_xy_blueprint(
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

- terms:

  A named list of two elements, `predictors` and `outcomes`. Both
  elements are `terms` objects that describe the terms for the outcomes
  and predictors separately. This argument is set automatically at
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) time.

- levels:

  Either `NULL` or a named list of character vectors that correspond to
  the levels observed when converting character predictor columns to
  factors during
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md). This
  argument is set automatically at
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) time.

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

- extra_role_ptypes:

  A named list. The names are the unique non-standard recipe roles (i.e.
  everything except `"predictors"` and `"outcomes"`). The values are
  prototypes of the original columns with that role. These are used for
  validation in
  [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).
