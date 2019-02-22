new_formula_engine <- function(mold,
                               forge,
                               intercept = FALSE,
                               info = NULL,
                               formula = NULL,
                               indicators = TRUE,
                               ...,
                               subclass = character()) {

  if (rlang::is_missing(mold)) {
    abort_no_mold()
  }

  if (rlang::is_missing(forge)) {
    abort_no_forge()
  }

  mold <- check_mold_formula(mold)
  forge <- check_forge(forge)

  validate_mold_args(
    mold,
    required_clean_args = c("engine", "data"),
    required_process_args = c("engine", "data")
  )

  validate_forge_args(forge)
  validate_is_formula_or_null(formula)
  validate_is_bool(indicators)

  new_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    formula = formula,
    indicators = indicators,
    ...,
    subclass = c(subclass, "formula_engine")
  )

}

refresh_engine.formula_engine <- function(engine) {
  do.call(new_formula_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

new_default_formula_engine <- function(intercept = FALSE,
                                       info = NULL,
                                       formula = NULL,
                                       indicators = TRUE,
                                       terms = list(
                                         predictors = NULL,
                                         outcomes = NULL
                                       )) {

  mold <- get_mold_formula_default_function_set()
  forge <- get_forge_formula_default_function_set()

  validate_is_terms_list_or_null(terms)

  new_formula_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
    formula = formula,
    indicators = indicators,
    terms = terms,
    subclass = "default_formula_engine"
  )

}

refresh_engine.default_formula_engine <- function(engine) {
  new_default_formula_engine(
    intercept = engine$intercept,
    info = engine$info,
    formula = engine$formula,
    indicators = engine$indicators,
    terms = engine$terms
  )
}

# ------------------------------------------------------------------------------

get_mold_formula_default_function_set <- function() {
  engine_function_set(mold_formula_default_clean, mold_formula_default_process)
}

# mold - formula - clean
mold_formula_default_clean <- function(engine, data) {

  data <- check_is_data_like(data)

  # validate here, not in the constructor, because we
  # put a non-intercept-containing formula back in
  validate_formula_has_intercept(engine$formula)

  formula <- remove_formula_intercept(engine$formula, engine$intercept)
  formula <- alter_formula_environment(formula)

  engine <- update_engine(engine, formula = formula)

  list(
    engine = engine,
    data = data
  )

}

# mold - formula - process
mold_formula_default_process <- function(engine, data) {

  c(engine, predictors) %<-% mold_formula_default_process_predictors(engine, data)
  c(engine, outcomes) %<-% mold_formula_default_process_outcomes(engine, data)

  # nuke formula environment before returning
  formula_empty_env <- nuke_formula_environment(engine$formula)
  engine <- update_engine(engine, formula = formula_empty_env)

  list(
    engine = engine,
    predictors = predictors,
    outcomes = outcomes
  )

}

mold_formula_default_process_predictors <- function(engine, data) {

  formula <- get_predictors_formula(engine$formula)

  original_names <- get_all_predictors(formula, data)
  original_data <- data[, original_names, drop = FALSE]
  original_data_classes <- get_data_classes(original_data)
  original_levels <- get_levels(original_data)

  if (!engine$indicators) {
    factor_names <- extract_original_factor_names(original_data_classes)
    validate_no_factors_in_functions(formula, factor_names)
    validate_no_factors_in_interactions(formula, factor_names)
    formula <- remove_factors_from_formula(formula, factor_names)
  }

  framed <- model_frame(formula, data)
  offset <- extract_offset(framed$data, framed$terms)

  predictors <- model_matrix(
    terms = framed$terms,
    data = framed$data
  )

  if (!engine$indicators) {
    predictors <- reattach_factor_columns(predictors, data, factor_names)
  }

  terms <- simplify_terms(framed$terms)

  engine_terms <- engine$terms
  engine_terms$predictors <- terms
  engine <- update_engine(engine, terms = engine_terms)

  info <- predictors_info(
    names = original_names,
    classes = original_data_classes,
    levels = original_levels
  )

  list(
    engine = engine,
    predictors = list(
      data = predictors,
      info = info,
      offset = offset
    )
  )

}

mold_formula_default_process_outcomes <- function(engine, data) {

  formula <- engine$formula

  original_names <- get_all_outcomes(formula, data)
  original_data <- data[, original_names, drop = FALSE]
  original_data_classes <- get_data_classes(original_data)
  original_levels <- get_levels(original_data)

  formula <- get_outcomes_formula(formula)

  # used on the `~ LHS` formula
  validate_no_interactions(formula)

  framed <- model_frame(formula, data)

  outcomes <- flatten_embedded_columns(framed$data)

  terms <- simplify_terms(framed$terms)

  engine_terms <- engine$terms
  engine_terms$outcomes <- terms
  engine <- update_engine(engine, terms = engine_terms)

  info <- outcomes_info(
    names = original_names,
    classes = original_data_classes,
    levels = original_levels
  )

  list(
    engine = engine,
    outcomes = list(
      data = outcomes,
      info = info
    )
  )

}

nuke_formula_environment <- function(formula) {
  rlang::new_formula(
    lhs = rlang::f_lhs(formula),
    rhs = rlang::f_rhs(formula),
    env = rlang::empty_env()
  )
}

# ------------------------------------------------------------------------------

get_forge_formula_default_function_set <- function() {
  engine_function_set(forge_formula_default_clean, forge_formula_default_process)
}

forge_formula_default_clean <- function(engine, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  new_data <- shrink2(engine, new_data, outcomes)
  new_data <- scream2(engine, new_data, outcomes)

  list(
    engine = engine,
    new_data = new_data
  )

}

forge_formula_default_process <- function(engine, new_data, outcomes) {

  c(engine, .predictors) %<-% forge_formula_default_process_predictors(engine, new_data)
  c(engine, .outcomes) %<-% forge_formula_default_process_outcomes(engine, new_data, outcomes)

  list(
    engine = engine,
    predictors = .predictors,
    outcomes = .outcomes
  )
}

forge_formula_default_process_predictors <- function(engine, new_data) {

  terms <- engine$terms$predictors
  terms <- alter_terms_environment(terms)
  terms <- delete_response(terms)

  framed <- model_frame(terms, new_data, engine$info$predictors$levels)

  .predictors <- model_matrix(
    terms = framed$terms,
    data = framed$data
  )

  if (!engine$indicators) {
    factor_names <- extract_original_factor_names(engine$info$predictors$classes)
    .predictors <- reattach_factor_columns(.predictors, new_data, factor_names)
  }

  .offset <- extract_offset(framed$data, framed$terms)

  list(
    engine = engine,
    predictors = list(
      data = .predictors,
      offset = .offset
    )
  )

}

forge_formula_default_process_outcomes <- function(engine, new_data, outcomes) {

  if (!outcomes) {

    out <- list(
      engine = engine,
      outcomes = list(
        data = NULL
      )
    )

    return(out)
  }

  terms <- engine$terms$outcomes
  terms <- alter_terms_environment(terms)

  framed <- model_frame(terms, new_data, engine$info$outcomes$levels)

  # Because model.matrix() does this for the RHS and we want
  # to be consistent even though we are only going through
  # model.frame()
  .outcomes <- flatten_embedded_columns(framed$data)

  list(
    engine = engine,
    outcomes = list(
      data = .outcomes
    )
  )

}

# ------------------------------------------------------------------------------

check_mold_formula <- function(mold) {

  validate_has_function_set_structure(mold)

  if (is.null(mold$clean)) {
    mold$clean <- get_default_mold_formula_clean()
  }

  mold
}

get_default_mold_formula_clean <- function() {

  function(engine, data) {
    list(
      engine = engine,
      data = data
    )
  }

}

# ------------------------------------------------------------------------------

validate_is_formula_or_null <- function(formula) {
  validate_is_or_null(formula, rlang::is_formula, "formula")
}

validate_formula_has_intercept_or_null <- function(formula) {

  if (is.null(formula)) {
    return(invisible(formula))
  }

  validate_formula_has_intercept(formula)

  invisible(formula)
}



validate_is_terms_list_or_null <- function(terms) {

  validate_is(terms, rlang::is_list, "list")

  validate_has_name(terms, "terms", "predictors")
  validate_has_name(terms, "terms", "outcomes")

  if (!is.null(terms$predictors)) {
    validate_is_terms(terms$predictors, glue("terms$predictors"))
  }

  if (!is.null(terms$outcomes)) {
    validate_is_terms(terms$outcomes, glue("terms$outcomes"))
  }

  invisible(terms)
}
