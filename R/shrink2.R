shrink2 <- function(engine, new_data, outcomes = FALSE) {

  new_data <- check_is_data_like(new_data)

  cols <- engine$info$predictors$names
  validate_new_data_column_names(new_data, cols)

  if (outcomes) {

    outcome_cols <- engine$info$outcomes$names
    validate_new_data_column_names(new_data, outcome_cols)

    cols <- c(outcome_cols, cols)

    # For the rare chance that an outcome
    # is also a predictor
    cols <- unique(cols)

  }

  # Subset new_data
  # (also sorts so columns are in the right order)
  new_data <- new_data[, cols, drop = FALSE]

  new_data
}
