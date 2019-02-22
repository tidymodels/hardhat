scream2 <- function(engine, new_data, outcomes = FALSE, drop_novel = TRUE) {

  new_data <- tibble::as_tibble(new_data)

  original_predictor_classes <- engine$info$predictors$classes
  original_predictor_levels <- engine$info$predictors$levels

  validate_new_data_classes(new_data, original_predictor_classes)

  new_data <- enforce_new_data_novel_levels(new_data, original_predictor_levels, drop_novel)
  new_data <- enforce_new_data_level_recovery(new_data, original_predictor_levels)

  if (outcomes) {

    original_outcome_classes <- engine$info$outcomes$classes
    original_outcome_levels <- engine$info$outcomes$levels

    validate_new_data_classes(new_data, original_outcome_classes)

    new_data <- enforce_new_data_novel_levels(new_data, original_outcome_levels, drop_novel)
    new_data <- enforce_new_data_level_recovery(new_data, original_outcome_levels)

  }

  new_data
}
