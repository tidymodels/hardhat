# Spruce up predictions

The family of `spruce_*()` functions convert predictions into a
standardized format. They are generally called from a prediction
implementation function for the specific `type` of prediction to return.

## Usage

``` r
spruce_numeric(pred)

spruce_class(pred_class)

spruce_prob(pred_levels, prob_matrix)
```

## Arguments

- pred:

  (`type = "numeric"`) A numeric vector of predictions.

- pred_class:

  (`type = "class"`) A factor of "hard" class predictions.

- pred_levels, prob_matrix:

  (`type = "prob"`)

  - `pred_levels` should be a character vector of the original levels of
    the outcome used in training.

  - `prob_matrix` should be a numeric matrix of class probabilities with
    as many columns as levels in `pred_levels`, and in the same order.

## Value

A tibble, ideally with the same number of rows as the `new_data` passed
to [`predict()`](https://rdrr.io/r/stats/predict.html). The column names
and number of columns vary based on the function used, but are
standardized.

## Details

After running a `spruce_*()` function, you should *always* use the
validation function
[`validate_prediction_size()`](https://hardhat.tidymodels.org/dev/reference/validate_prediction_size.md)
to ensure that the number of rows being returned is the same as the
number of rows in the input (`new_data`).
