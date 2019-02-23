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

# ------------------------------------------------------------------------------

get_forge_formula_default_function_set <- function() {
  engine_function_set(forge_formula_default_clean, forge_formula_default_process)
}

forge_formula_default_clean <- function(engine, new_data, outcomes) {

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

# Is this a bad idea? We need it to forge() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in mold()
alter_terms_environment <- function(terms_engine) {
  env_above_global_env <- rlang::env_parent(rlang::global_env())
  attr(terms_engine, ".Environment") <- env_above_global_env
  terms_engine
}

# ------------------------------------------------------------------------------

nuke_formula_environment <- function(formula) {
  rlang::new_formula(
    lhs = rlang::f_lhs(formula),
    rhs = rlang::f_rhs(formula),
    env = rlang::empty_env()
  )
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

alter_formula_environment <- function(formula) {

  # formula environment is 1 step above global env to avoid
  # global variables but maintain ability to use pkg functions
  # (like stats::poly())
  env_above_global_env <- rlang::env_parent(rlang::global_env())

  rlang::new_formula(
    lhs = rlang::f_lhs(formula),
    rhs = rlang::f_rhs(formula),
    env = env_above_global_env
  )
}

# We do this extra flattening because it happens on the RHS
# automatically because of the model.matrix() call. So this
# makes the column types consistent when doing something
# complex on the LHS like poly(, degree = 2) that returns
# a matrix
flatten_embedded_columns <- function(data) {

  has_embedded_2D <- vapply(
    X = data,
    FUN = function(col) dims(col) > 1,
    FUN.VALUE = logical(1)
  )

  has_any_embedded_2D <- any(has_embedded_2D)

  if (has_any_embedded_2D) {

    # Inspired by
    # https://stackoverflow.com/questions/43281803/embedded-data-frame-in-r-what-is-it-what-is-it-called-why-does-it-behave-th
    # This could probably be better?
    # It doesn't work with tibble(!!!x)
    frame_flattener <- rlang::expr(
      data.frame(
        !!!data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )

    data <- rlang::eval_bare(frame_flattener)
  }

  tibble::as_tibble(data)
}

validate_no_factors_in_functions <- function(.formula, .factor_names) {

  .terms <- terms(.formula)

  bad_original_cols <- detect_factors_in_functions(.terms, .factor_names)

  ok <- length(bad_original_cols) == 0L

  if (!ok) {

    bad_original_cols <- glue_quote_collapse(bad_original_cols)

    glubort(
      "Functions involving factors have been detected on the ",
      "RHS of `formula`. These are not allowed when `indicators = FALSE`. ",
      "Functions involving factors were detected for the following columns: ",
      "{bad_original_cols}."
    )

  }

  invisible(.formula)
}

# Returns original column names of any factor columns that
# are present in an inline function
# The row.names() of the factors matrix contains all of the
# non-interaction expressions that are used in the formula
detect_factors_in_functions <- function(.terms, .factor_names) {

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  all_terms_exprs <- row.names(terms_matrix)

  # Remove bare factor names
  exprs_no_bare_factors <- all_terms_exprs[!(all_terms_exprs %in% .factor_names)]

  if (length(exprs_no_bare_factors) == 0L) {
    return(character(0))
  }

  .factor_name_is_in_a_fn <- vapply(
    .factor_names,
    function(nm) {
      any(grepl(nm, exprs_no_bare_factors))
    },
    logical(1)
  )

  bad_cols <- .factor_names[.factor_name_is_in_a_fn]

  bad_cols
}

validate_no_factors_in_interactions <- function(.formula, .factor_names) {

  # Call terms on a standard formula to generate the terms interaction matrix
  .terms <- terms(.formula)

  bad_original_cols <- detect_factors_in_interactions(.terms, .factor_names)

  ok <- length(bad_original_cols) == 0L

  if (!ok) {

    bad_original_cols <- glue_quote_collapse(bad_original_cols)

    glubort(
      "Interaction terms involving factors have been detected on the ",
      "RHS of `formula`. These are not allowed when `indicators = FALSE`. ",
      "Interactions involving factors were detected for the following columns: ",
      "{bad_original_cols}."
    )

  }

  invisible(.formula)
}

# Returns the _original_ column names
# of any factor columns that are present
# in any interaction terms (from : or * or %in% or ^)
detect_factors_in_interactions <- function(.terms, .factor_names) {

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  other_cols <- setdiff(colnames(terms_matrix), .factor_names)

  no_other_cols <- length(other_cols) == 0L
  if (no_other_cols) {
    return(character(0))
  }

  # Something like Species, rather than paste0(Species)
  bare_factor_names <- .factor_names[.factor_names %in% row.names(terms_matrix)]

  # Something like mold(~ paste0(Species), iris, indicators = FALSE)
  no_bare_factors_used <- length(bare_factor_names) == 0L
  if (no_bare_factors_used) {
    return(character(0))
  }

  factor_rows <- terms_matrix[bare_factor_names, , drop = FALSE]
  factor_rows <- factor_rows[, other_cols, drop = FALSE]

  # In the factor matrix, only `:` is present to represent interactions,
  # even if something like * or ^ or %in% was used to generate it
  where_interactions <- grepl(":", colnames(factor_rows))

  none_have_interactions <- !any(where_interactions)
  if (none_have_interactions) {
    return(character(0))
  }

  interaction_cols <- factor_rows[, where_interactions, drop = FALSE]

  factor_is_bad_if_gt_0 <- rowSums(interaction_cols)
  bad_factor_vals <- factor_is_bad_if_gt_0[factor_is_bad_if_gt_0 > 0]

  bad_cols <- names(bad_factor_vals)

  bad_cols
}

validate_no_interactions <- function(.formula) {

  bad_terms <- detect_interactions(.formula)

  no_interactions <- length(bad_terms) == 0L
  if (no_interactions) {
    return(invisible(.formula))
  }

  bad_terms <- glue_quote_collapse(bad_terms)

  glubort(
    "Interaction terms cannot be specified on the LHS of `formula`. ",
    "The following interaction terms were found: {bad_terms}."
  )
}

# Returns processed names of any interaction terms
# like 'Species:Sepal.Width', or character(0)
detect_interactions <- function(.formula) {

  .terms <- terms(.formula)

  terms_matrix <- attr(.terms, "factors")

  only_intercept_or_offsets <- length(terms_matrix) == 0L
  if (only_intercept_or_offsets) {
    return(character(0))
  }

  terms_nms <- colnames(terms_matrix)

  # All interactions (*, ^, %in%) will be expanded to `:`
  has_interactions <- grepl(":", terms_nms)

  has_any_interactions <- any(has_interactions)

  if (!has_any_interactions) {
    return(character(0))
  }

  bad_terms <- terms_nms[has_interactions]

  bad_terms
}

extract_original_factor_names <- function(.data_classes) {

  no_data_classes <- length(.data_classes) == 0L
  if (no_data_classes) {
    return(character(0))
  }

  where_factor <- vapply(.data_classes, function(cls) any(cls %in% c("factor", "ordered")), logical(1))
  factor_classes <- .data_classes[where_factor]

  no_factor_classes <- length(factor_classes) == 0L
  if (no_factor_classes) {
    return(character(0))
  }

  original_factor_columns <- names(factor_classes)

  original_factor_columns
}

remove_factors_from_formula <- function(.formula, .factor_names) {

  if (length(.factor_names) == 0L) {
    return(.formula)
  }

  .factor_syms <- rlang::syms(.factor_names)

  .f_rhs <- rlang::f_rhs(.formula)

  for (.factor_sym in .factor_syms) {
    .f_rhs <- rlang::expr(!! .f_rhs - !! .factor_sym)
  }

  rlang::new_formula(
    lhs = rlang::f_lhs(.formula),
    rhs = .f_rhs,
    env = rlang::f_env(.formula)
  )
}

reattach_factor_columns <- function(predictors, data, .factor_names) {
  data_factor_cols <- data[, .factor_names, drop = FALSE]
  tibble::add_column(predictors, !!! data_factor_cols)
}

get_predictors_formula <- function(formula) {
  rlang::new_formula(
    lhs = NULL,
    rhs = rlang::f_rhs(formula),
    env = rlang::f_env(formula)
  )
}

get_outcomes_formula <- function(formula) {

  new_formula <- rlang::new_formula(
    lhs = NULL,
    rhs = rlang::f_lhs(formula),
    env = rlang::f_env(formula)
  )

  remove_formula_intercept(new_formula, intercept = FALSE)
}

