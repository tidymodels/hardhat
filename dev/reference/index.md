# Package index

## Preprocessing

- [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) :
  Mold data for modeling
- [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) :
  Forge prediction-ready data

## Prediction

- [`spruce_numeric_multiple()`](https://hardhat.tidymodels.org/dev/reference/spruce-multiple.md)
  [`spruce_class_multiple()`](https://hardhat.tidymodels.org/dev/reference/spruce-multiple.md)
  [`spruce_prob_multiple()`](https://hardhat.tidymodels.org/dev/reference/spruce-multiple.md)
  : Spruce up multi-outcome predictions
- [`spruce_numeric()`](https://hardhat.tidymodels.org/dev/reference/spruce.md)
  [`spruce_class()`](https://hardhat.tidymodels.org/dev/reference/spruce.md)
  [`spruce_prob()`](https://hardhat.tidymodels.org/dev/reference/spruce.md)
  : Spruce up predictions
- [`quantile_pred()`](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
  [`is_quantile_pred()`](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
  [`extract_quantile_levels()`](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
  [`as_tibble(`*`<quantile_pred>`*`)`](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
  [`as.matrix(`*`<quantile_pred>`*`)`](https://hardhat.tidymodels.org/dev/reference/quantile_pred.md)
  : Create a vector containing sets of quantiles

## Utility

- [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md)
  : Construct a model frame

- [`model_matrix()`](https://hardhat.tidymodels.org/dev/reference/model_matrix.md)
  : Construct a design matrix

- [`model_offset()`](https://hardhat.tidymodels.org/dev/reference/model_offset.md)
  : Extract a model offset

- [`delete_response()`](https://hardhat.tidymodels.org/dev/reference/delete_response.md)
  : Delete the response from a terms object

- [`standardize()`](https://hardhat.tidymodels.org/dev/reference/standardize.md)
  : Standardize the outcome

- [`new_model()`](https://hardhat.tidymodels.org/dev/reference/new_model.md)
  : Constructor for a base model

- [`add_intercept_column()`](https://hardhat.tidymodels.org/dev/reference/add_intercept_column.md)
  :

  Add an intercept column to `data`

- [`weighted_table()`](https://hardhat.tidymodels.org/dev/reference/weighted_table.md)
  : Weighted table

- [`fct_encode_one_hot()`](https://hardhat.tidymodels.org/dev/reference/fct_encode_one_hot.md)
  : Encode a factor as a one-hot indicator matrix

- [`impute_quantiles()`](https://hardhat.tidymodels.org/dev/reference/impute_quantiles.md)
  :

  Impute additional quantiles from a `quantile_pred`

## Validation

- [`scream()`](https://hardhat.tidymodels.org/dev/reference/scream.md) :
  Scream

- [`shrink()`](https://hardhat.tidymodels.org/dev/reference/shrink.md) :
  Subset only required columns

- [`validate_column_names()`](https://hardhat.tidymodels.org/dev/reference/validate_column_names.md)
  [`check_column_names()`](https://hardhat.tidymodels.org/dev/reference/validate_column_names.md)
  :

  Ensure that `data` contains required column names

- [`validate_no_formula_duplication()`](https://hardhat.tidymodels.org/dev/reference/validate_no_formula_duplication.md)
  [`check_no_formula_duplication()`](https://hardhat.tidymodels.org/dev/reference/validate_no_formula_duplication.md)
  :

  Ensure no duplicate terms appear in `formula`

- [`validate_outcomes_are_binary()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_binary.md)
  [`check_outcomes_are_binary()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_binary.md)
  : Ensure that the outcome has binary factors

- [`validate_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md)
  [`check_outcomes_are_factors()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_factors.md)
  : Ensure that the outcome has only factor columns

- [`validate_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md)
  [`check_outcomes_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_numeric.md)
  : Ensure outcomes are all numeric

- [`validate_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md)
  [`check_outcomes_are_univariate()`](https://hardhat.tidymodels.org/dev/reference/validate_outcomes_are_univariate.md)
  : Ensure that the outcome is univariate

- [`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md)
  [`check_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md)
  : Ensure that predictions have the correct number of rows

- [`validate_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)
  [`check_predictors_are_numeric()`](https://hardhat.tidymodels.org/dev/reference/validate_predictors_are_numeric.md)
  : Ensure predictors are all numeric

## Blueprint

- [`default_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_formula_blueprint.md)
  [`mold(`*`<formula>`*`)`](https://hardhat.tidymodels.org/dev/reference/default_formula_blueprint.md)
  : Default formula blueprint

- [`default_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_recipe_blueprint.md)
  [`mold(`*`<recipe>`*`)`](https://hardhat.tidymodels.org/dev/reference/default_recipe_blueprint.md)
  : Default recipe blueprint

- [`default_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md)
  [`mold(`*`<data.frame>`*`)`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md)
  [`mold(`*`<matrix>`*`)`](https://hardhat.tidymodels.org/dev/reference/default_xy_blueprint.md)
  : Default XY blueprint

- [`is_blueprint()`](https://hardhat.tidymodels.org/dev/reference/is_blueprint.md)
  :

  Is `x` a preprocessing blueprint?

- [`new_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
  [`new_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
  [`new_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
  [`new_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-blueprint.md)
  : Create a new preprocessing blueprint

- [`new_default_formula_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-default-blueprint.md)
  [`new_default_recipe_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-default-blueprint.md)
  [`new_default_xy_blueprint()`](https://hardhat.tidymodels.org/dev/reference/new-default-blueprint.md)
  : Create a new default blueprint

- [`refresh_blueprint()`](https://hardhat.tidymodels.org/dev/reference/refresh_blueprint.md)
  : Refresh a preprocessing blueprint

- [`run_forge()`](https://hardhat.tidymodels.org/dev/reference/run-forge.md)
  :

  [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md)
  according to a blueprint

- [`run_mold()`](https://hardhat.tidymodels.org/dev/reference/run-mold.md)
  :

  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md)
  according to a blueprint

- [`update_blueprint()`](https://hardhat.tidymodels.org/dev/reference/update_blueprint.md)
  : Update a preprocessing blueprint

## Case Weights

- [`new_case_weights()`](https://hardhat.tidymodels.org/dev/reference/new_case_weights.md)
  **\[experimental\]** : Extend case weights

- [`is_case_weights()`](https://hardhat.tidymodels.org/dev/reference/is_case_weights.md)
  **\[experimental\]** :

  Is `x` a case weights vector?

### Importance Weights

- [`importance_weights()`](https://hardhat.tidymodels.org/dev/reference/importance_weights.md)
  **\[experimental\]** : Importance weights

- [`new_importance_weights()`](https://hardhat.tidymodels.org/dev/reference/new_importance_weights.md)
  **\[experimental\]** : Construct an importance weights vector

- [`is_importance_weights()`](https://hardhat.tidymodels.org/dev/reference/is_importance_weights.md)
  **\[experimental\]** :

  Is `x` an importance weights vector?

### Frequency Weights

- [`frequency_weights()`](https://hardhat.tidymodels.org/dev/reference/frequency_weights.md)
  **\[experimental\]** : Frequency weights

- [`new_frequency_weights()`](https://hardhat.tidymodels.org/dev/reference/new_frequency_weights.md)
  **\[experimental\]** : Construct a frequency weights vector

- [`is_frequency_weights()`](https://hardhat.tidymodels.org/dev/reference/is_frequency_weights.md)
  **\[experimental\]** :

  Is `x` a frequency weights vector?

## Setup

- [`create_modeling_package()`](https://hardhat.tidymodels.org/dev/reference/modeling-usethis.md)
  [`use_modeling_deps()`](https://hardhat.tidymodels.org/dev/reference/modeling-usethis.md)
  [`use_modeling_files()`](https://hardhat.tidymodels.org/dev/reference/modeling-usethis.md)
  : Create a modeling package

## Information

- [`get_data_classes()`](https://hardhat.tidymodels.org/dev/reference/get_data_classes.md)
  : Extract data classes from a data frame or matrix
- [`get_levels()`](https://hardhat.tidymodels.org/dev/reference/get_levels.md)
  [`get_outcome_levels()`](https://hardhat.tidymodels.org/dev/reference/get_levels.md)
  : Extract factor levels from a data frame

## Development

- [`tune()`](https://hardhat.tidymodels.org/dev/reference/tune.md) :
  Mark arguments for tuning
- [`extract_workflow()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_recipe()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_spec_parsnip()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_fit_parsnip()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_fit_engine()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_mold()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_preprocessor()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_postprocessor()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_tailor()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  [`extract_fit_time()`](https://hardhat.tidymodels.org/dev/reference/hardhat-extract.md)
  : Generics for object extraction

## Data

- [`hardhat-example-data`](https://hardhat.tidymodels.org/dev/reference/hardhat-example-data.md)
  [`example_train`](https://hardhat.tidymodels.org/dev/reference/hardhat-example-data.md)
  [`example_test`](https://hardhat.tidymodels.org/dev/reference/hardhat-example-data.md)
  : Example data for hardhat
