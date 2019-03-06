shrink2 <- function(new_data, engine, outcomes = FALSE) {
  new_data <- check_is_data_like(new_data)

  cols <- colnames(engine$info$predictors)
  predictors <- shrink2_impl(new_data, cols)

  if (outcomes) {
    outcome_cols <- colnames(engine$info$outcomes)
    outcomes <- shrink2_impl(new_data, outcome_cols)
  }
  else {
    outcomes <- NULL
  }

  list(predictors = predictors, outcomes = outcomes)
}

shrink2_impl <- function(data, cols) {
  validate_new_data_column_names(data, cols)
  data[, cols, drop = FALSE]
}
