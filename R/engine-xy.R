new_xy_engine <- function(mold,
                          forge,
                          intercept = FALSE,
                          info = NULL,
                          ...,
                          subclass = character()) {

  if (rlang::is_missing(mold)) {
    abort_no_mold()
  }

  if (rlang::is_missing(forge)) {
    abort_no_forge()
  }

  mold <- check_mold_xy(mold)
  forge <- check_forge(forge)

  validate_mold_args(
    mold = mold,
    required_clean_args = c("engine", "x", "y"),
    required_process_args = c("engine", "x", "y")
  )

  validate_forge_args(forge)

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    ...,
    subclass = c(subclass, "xy_engine")
  )

}

refresh_engine.xy_engine <- function(engine) {
  do.call(new_xy_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

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

  x <- maybe_add_intercept_column(x, engine$intercept)

  info <- predictors_info(
    names = colnames(x),
    classes = get_data_classes(x),
    levels = get_levels(x)
  )

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

# ------------------------------------------------------------------------------

check_mold_xy <- function(mold) {

  validate_has_function_set_structure(mold)

  if (is.null(mold$clean)) {
    mold$clean <- get_default_mold_xy_clean()
  }

  mold
}

get_default_mold_xy_clean <- function() {

  function(engine, x, y) {
    list(
      engine = engine,
      x = x,
      y = y
    )
  }

}

validate_mold_args <- function(mold, required_clean_args, required_process_args) {

  actual_clean_args <- rlang::fn_fmls_names(mold$clean)

  if (!identical(actual_clean_args, required_clean_args)) {
    required_clean_args <- glue_quote_collapse(required_clean_args)

    glubort(
      "`mold$clean()` must have the following arguments: {required_clean_args}."
    )
  }

  actual_process_args <- rlang::fn_fmls_names(mold$process)

  if (!identical(required_process_args, actual_process_args)) {
    required_process_args <- glue_quote_collapse(required_process_args)

    glubort(
      "`mold$process()` must have the following arguments: {required_process_args}."
    )
  }

  invisible(mold)

}


# ------------------------------------------------------------------------------

check_forge <- function(forge) {

  validate_has_function_set_structure(forge)

  if (is.null(forge$clean)) {
    forge$clean <- get_default_forge_clean()
  }

  forge
}

get_default_forge_clean <- function() {

  function(engine, new_data) {
    list(
      engine = engine,
      new_data
    )
  }

}

validate_forge_args <- function(forge) {

  required_args <- c("engine", "new_data", "outcomes")

  actual_clean_args <- rlang::fn_fmls_names(forge$clean)

  if (!identical(actual_clean_args, required_args)) {
    required_args <- glue_quote_collapse(required_args)

    glubort(
      "`forge$clean()` must have the following arguments: {required_args}."
    )
  }

  actual_process_args <- rlang::fn_fmls_names(forge$process)

  if (!identical(required_args, actual_process_args)) {
    required_args <- glue_quote_collapse(required_args)

    glubort(
      "`forge$process()` must have the following arguments: {required_args}."
    )
  }

  invisible(forge)
}

abort_no_mold <- function() {
  glubort(
    "`mold` must be supplied in the form: ",
    "list(clean = <function>, process = <function>)."
  )
}

abort_no_forge <- function() {
  glubort(
    "`forge` must be supplied in the form: ",
    "list(clean = <function>, process = <function>)."
  )
}
