# Generics for object extraction

These generics are used to extract elements from various model objects.
Methods are defined in other packages, such as tune, workflows, and
workflowsets, but the returned object is always the same.

- `extract_fit_engine()` returns the engine specific fit embedded within
  a parsnip model fit. For example, when using `parsnip::linear_reg()`
  with the `"lm"` engine, this returns the underlying `lm` object.

- `extract_fit_parsnip()` returns a parsnip model fit.

- `extract_mold()` returns the preprocessed "mold" object returned from
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md). It
  contains information about the preprocessing, including either the
  prepped recipe, the formula terms object, or variable selectors.

- `extract_spec_parsnip()` returns a parsnip model specification.

- `extract_preprocessor()` returns the formula, recipe, or variable
  expressions used for preprocessing.

- `extract_recipe()` returns a recipe, possibly estimated.

- `extract_postprocessor()` returns the post-processor.

- `extract_tailor()` returns a tailor, possibly fit.

- `extract_workflow()` returns a workflow, possibly fit.

- `extract_parameter_dials()` returns a single dials parameter object.

- `extract_parameter_set_dials()` returns a set of dials parameter
  objects.

- `extract_fit_time()` returns a tibble with fit times.

## Usage

``` r
extract_workflow(x, ...)

extract_recipe(x, ...)

extract_spec_parsnip(x, ...)

extract_fit_parsnip(x, ...)

extract_fit_engine(x, ...)

extract_mold(x, ...)

extract_preprocessor(x, ...)

extract_postprocessor(x, ...)

extract_tailor(x, ...)

extract_parameter_dials(x, ...)

extract_parameter_set_dials(x, ...)

extract_fit_time(x, ...)
```

## Arguments

- x:

  An object.

- ...:

  Extra arguments passed on to methods.

## Examples

``` r
# See packages where methods are defined for examples, such as `parsnip` or
# `workflows`.
```
