new_default_xy_engine <- function(intercept = FALSE,
                                  info = NULL) {

  mold <- get_mold_xy_default_function_set()
  forge <- get_forge_xy_default_function_set()

  new_xy_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    subclass = "default_xy_engine"
  )

}

refresh_engine.default_xy_engine <- function(engine) {
  new_default_xy_engine(
    intercept = engine$intercept,
    info = engine$info
  )
}

# ------------------------------------------------------------------------------

get_mold_xy_default_function_set <- function() {
  engine_function_set(mold_xy_default_clean, mold_xy_default_process)
}

# mold - xy - clean
mold_xy_default_clean <- function(engine, x, y) {

  c(engine, x) %<-% mold_xy_default_clean_predictors(engine, x)
  c(engine, y) %<-% mold_xy_default_clean_outcomes(engine, y)

  list(
    engine = engine,
    x = x,
    y = y
  )
}

mold_xy_default_clean_predictors <- function(engine, x) {
  x <- tibble::as_tibble(x)
  list(engine = engine, x = x)
}

mold_xy_default_clean_outcomes <- function(engine, y) {
  y <- standardize(y)
  list(engine = engine, y = y)
}

# mold - xy - process
mold_xy_default_process <- function(engine, x, y) {

  c(engine, predictors) %<-% mold_xy_default_process_predictors(engine, x)
  c(engine, outcomes) %<-% mold_xy_default_process_outcomes(engine, y)

  list(
    engine = engine,
    predictors = predictors,
    outcomes = outcomes
  )
}

mold_xy_default_process_predictors <- function(engine, x) {

  # Important! Collect info before adding intercept!
  info <- predictors_info(
    names = colnames(x),
    classes = get_data_classes(x),
    levels = get_levels(x)
  )

  x <- maybe_add_intercept_column(x, engine$intercept)

  list(
    engine = engine,
    predictors = list(
      data = x,
      info = info,
      offset = NULL
    )
  )

}

mold_xy_default_process_outcomes <- function(engine, y) {

  info <- outcomes_info(
    names = colnames(y),
    classes = get_data_classes(y),
    levels = get_levels(y)
  )

  list(
    engine = engine,
    outcomes = list(
      data = y,
      info = info
    )
  )

}

# ------------------------------------------------------------------------------

get_forge_xy_default_function_set <- function() {
  engine_function_set(forge_xy_default_clean, forge_xy_default_process)
}

forge_xy_default_clean <- function(engine, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink(new_data, engine, outcomes)
  new_data <- scream(new_data, engine, outcomes)

  list(
    engine = engine,
    new_data = new_data
  )

}

forge_xy_default_process <- function(engine, new_data, outcomes) {

  c(engine, .predictors) %<-% forge_xy_default_process_predictors(engine, new_data)
  c(engine, .outcomes) %<-% forge_xy_default_process_outcomes(engine, new_data, outcomes)

  list(
    engine = engine,
    predictors = .predictors,
    outcomes = .outcomes
  )
}

forge_xy_default_process_predictors <- function(engine, new_data) {

  original_names <- engine$info$predictors$names

  .predictors <- new_data[, original_names, drop = FALSE]

  .predictors <- maybe_add_intercept_column(.predictors, engine$intercept)

  list(
    engine = engine,
    predictors = list(
      data = .predictors,
      offset = NULL
    )
  )

}

forge_xy_default_process_outcomes <- function(engine, new_data, outcomes) {

  if (!outcomes) {

    out <- list(
      engine = engine,
      outcomes = list(
        data = NULL
      )
    )

    return(out)
  }

  original_names <- engine$info$outcomes$names

  .outcomes <- new_data[, original_names, drop = FALSE]

  list(
    engine = engine,
    outcomes = list(
      data = .outcomes
    )
  )

}
