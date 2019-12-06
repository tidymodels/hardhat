#' Default formula blueprint
#'
#' This pages holds the details for the formula preprocessing blueprint. This
#' is the blueprint used by default from `mold()` if `x` is a formula.
#'
#' @inheritParams new_formula_blueprint
#'
#' @param formula A formula specifying the predictors and the outcomes.
#'
#' @param data A data frame or matrix containing the outcomes and predictors.
#'
#' @param blueprint A preprocessing `blueprint`. If left as `NULL`, then a
#' [default_formula_blueprint()] is used.
#'
#' @param ... Not used.
#'
#' @return
#'
#' For `default_formula_blueprint()`, a formula blueprint.
#'
#' @details
#'
#' While not different from base R, the behavior of expanding factors into
#' dummy variables when an intercept is _not_ present should be documented.
#'
#' - When an intercept is present, factors are expanded into `K-1` new columns,
#' where `K` is the number of levels in the factor.
#'
#' - When an intercept is _not_ present, factors are expanded into all `K`
#' columns (one-hot encoding).
#'
#' Offsets can be included in the formula method through the use of the inline
#' function [stats::offset()]. These are returned as a tibble with 1 column
#' named `".offset"` in the `$extras$offset` slot of the return value.
#'
#' @section Mold:
#'
#' When `mold()` is used with the default formula blueprint:
#'
#' - Predictors
#'
#'    - The RHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ RHS`.
#'
#'    - Runs [stats::model.frame()] on the RHS formula and uses `data`.
#'
#'    - If `indicators = TRUE`, it then runs [stats::model.matrix()] on the
#'    result.
#'
#'    - If `indicators = FALSE`, factors are removed before `model.matrix()`
#'    is run, and then added back afterwards. No interactions or inline
#'    functions involving factors are allowed.
#'
#'    - If any offsets are present from using `offset()`, then they are
#'    extracted with [model_offset()].
#'
#'    - If `intercept = TRUE`, adds an intercept column.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' - Outcomes
#'
#'    - The LHS of the `formula` is isolated, and converted to its own
#'    1 sided formula: `~ LHS`.
#'
#'    - Runs [stats::model.frame()] on the LHS formula and uses `data`.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#' @section Forge:
#'
#' When `forge()` is used with the default formula blueprint:
#'
#' - It calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - It calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Predictors
#'
#'    - It runs [stats::model.frame()] on `new_data` using the stored terms
#'    object corresponding to the _predictors_.
#'
#'    - If, in the original [mold()] call, `indicators = TRUE` was set, it
#'    then runs [stats::model.matrix()] on the result.
#'
#'    - If, in the original [mold()] call, `indicators = FALSE` was set, it
#'    runs [stats::model.matrix()] on the result without the factor columns,
#'    and then adds them on afterwards.
#'
#'    - If any offsets are present from using `offset()` in the original call
#'    to [mold()], then they are extracted with [model_offset()].
#'
#'    - If `intercept = TRUE` in the original call to [mold()], then an
#'    intercept column is added.
#'
#'    - It coerces the result of the above steps to a tibble.
#'
#'  - Outcomes
#'
#'    - It runs [stats::model.frame()] on `new_data` using the
#'    stored terms object corresponding to the _outcomes_.
#'
#'    - Coerces the result to a tibble.
#'
#' @section Differences From Base R:
#'
#' There are a number of differences from base R regarding how formulas are
#' processed by `mold()` that require some explanation.
#'
#' Multivariate outcomes can be specified on the LHS using syntax that is
#' similar to the RHS (i.e. `outcome_1 + outcome_2 ~ predictors`).
#' If any complex calculations are done on the LHS and they return matrices
#' (like [stats::poly()]), then those matrices are flattened into multiple
#' columns of the tibble after the call to `model.frame()`. While this is
#' possible, it is not recommended, and if a large amount of preprocessing is
#' required on the outcomes, then you are better off
#' using a [recipes::recipe()].
#'
#' Global variables are _not_ allowed in the formula. An error will be thrown
#' if they are included. All terms in the formula should come from `data`.
#'
#' By default, intercepts are _not_ included in the predictor output from the
#' formula. To include an intercept, set
#' `blueprint = default_formula_blueprint(intercept = TRUE)`. The rationale
#' for this is that many packages either always require or never allow an
#' intercept (for example, the `earth` package), and they do a large amount of
#' extra work to keep the user from supplying one or removing it. This
#' interface standardizes all of that flexibility in one place.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Formula Example
#'
#' # Call mold() with the training data
#' processed <- mold(
#'   log(Sepal.Width) ~ Sepal.Length + Species,
#'   train,
#'   blueprint = default_formula_blueprint(intercept = TRUE)
#' )
#'
#' # Then, call forge() with the blueprint and the test data
#' # to have it preprocess the test data in the same way
#' forge(test, processed$blueprint)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome
#' forge(test, processed$blueprint, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Factors without an intercept
#'
#' # No intercept is added by default
#' processed <- mold(Sepal.Width ~ Species, train)
#'
#' # So factor columns are completely expanded
#' # into all `K` columns (the number of levels)
#' processed$predictors
#'
#' # ---------------------------------------------------------------------------
#' # Global variables
#'
#' y <- rep(1, times = nrow(train))
#'
#' # In base R, global variables are allowed in a model formula
#' frame <- model.frame(Species ~ y + Sepal.Length, train)
#' head(frame)
#'
#' # mold() does not allow them, and throws an error
#' tryCatch(
#'   expr = mold(Species ~ y + Sepal.Length, train),
#'   error = function(e) print(e$message)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables and interactions
#'
#' # By default, factor columns are expanded
#' # and interactions are created, both by
#' # calling model.matrix(). Some models (like
#' # tree based models) can take factors directly
#' # but still might want to use the formula method.
#' # In those cases, set `indicators = FALSE` to not
#' # run model.matrix() on factor columns. Interactions
#' # are still allowed and are run on numeric columns.
#'
#' blueprint_no_indicators <- default_formula_blueprint(indicators = FALSE)
#'
#' processed <- mold(
#'   ~ Species + Sepal.Width:Sepal.Length,
#'   train,
#'   blueprint = blueprint_no_indicators
#' )
#'
#' processed$predictors
#'
#' # An informative error is thrown when `indicators = FALSE` and
#' # factors are present in interaction terms or in inline functions
#' try(mold(Sepal.Width ~ Sepal.Length:Species, train, blueprint = blueprint_no_indicators))
#' try(mold(Sepal.Width ~ paste0(Species), train, blueprint = blueprint_no_indicators))
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas can be specified easily
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, train)
#' processed$outcomes
#'
#' # Inline functions on the LHS are run, but any matrix
#' # output is flattened (like what happens in `model.matrix()`)
#' # (essentially this means you don't wind up with columns
#' # in the tibble that are matrices)
#' processed <- mold(poly(Sepal.Length, degree = 2) ~ Species, train)
#' processed$outcomes
#'
#' # TRUE
#' ncol(processed$outcomes) == 2
#'
#' # Multivariate formulas specified in mold()
#' # carry over into forge()
#' forge(test, processed$blueprint, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets are handled specially in base R, so they deserve special
#' # treatment here as well. You can add offsets using the inline function
#' # offset()
#' processed <- mold(Sepal.Width ~ offset(Sepal.Length) + Species, train)
#'
#' processed$extras$offset
#'
#' # Multiple offsets can be included, and they get added together
#' processed <- mold(
#'   Sepal.Width ~ offset(Sepal.Length) + offset(Petal.Width),
#'   train
#' )
#'
#' identical(
#'   processed$extras$offset$.offset,
#'   train$Sepal.Length + train$Petal.Width
#' )
#'
#' # Forging test data will also require
#' # and include the offset
#' forge(test, processed$blueprint)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept only
#'
#' # Because `1` and `0` are intercept modifying terms, they are
#' # not allowed in the formula and are controlled by the
#' # `intercept` argument of the blueprint. To use an intercept
#' # only formula, you should supply `NULL` on the RHS of the formula.
#' mold(~ NULL, train, blueprint = default_formula_blueprint(intercept = TRUE))
#'
#' @export
default_formula_blueprint <- function(intercept = FALSE,
                                      allow_novel_levels = FALSE,
                                      indicators = TRUE) {

  mold <- get_mold_formula_default_function_set()
  forge <- get_forge_formula_default_function_set()

  new_default_formula_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    indicators = indicators
  )

}

#' @param terms A named list of two elements, `predictors` and `outcomes`. Both
#' elements are `terms` objects that describe the terms for the outcomes and
#' predictors separately. This argument is set automatically at [mold()] time.
#'
#' @rdname new-default-blueprint
#' @export
new_default_formula_blueprint <- function(mold,
                                          forge,
                                          intercept = FALSE,
                                          allow_novel_levels = FALSE,
                                          ptypes = NULL,
                                          formula = NULL,
                                          indicators = TRUE,
                                          terms = list(
                                            predictors = NULL,
                                            outcomes = NULL
                                          ),
                                          ...,
                                          subclass = character()) {

  validate_is_terms_list_or_null(terms)

  new_formula_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    ptypes = ptypes,
    formula = formula,
    indicators = indicators,
    terms = terms,
    ...,
    subclass = c(subclass, "default_formula_blueprint")
  )

}

#' @export
refresh_blueprint.default_formula_blueprint <- function(blueprint) {
  do.call(new_default_formula_blueprint, as.list(blueprint))
}

# ------------------------------------------------------------------------------

get_mold_formula_default_function_set <- function() {
  blueprint_function_set(mold_formula_default_clean, mold_formula_default_process)
}

# mold - formula - clean
mold_formula_default_clean <- function(blueprint, data) {

  data <- check_is_data_like(data)

  # validate here, not in the constructor, because we
  # put a non-intercept-containing formula back in
  validate_formula_has_intercept(blueprint$formula)

  formula <- remove_formula_intercept(blueprint$formula, blueprint$intercept)
  formula <- alter_formula_environment(formula)

  blueprint <- update_blueprint(blueprint, formula = formula)

  out$mold$clean(blueprint, data)
}

# mold - formula - process
mold_formula_default_process <- function(blueprint, data) {
  processed <- mold_formula_default_process_predictors(
    blueprint = blueprint,
    data = data
  )

  blueprint <- processed$blueprint
  predictors_lst <- processed$terms_lst

  processed <- mold_formula_default_process_outcomes(
    blueprint = blueprint,
    data = data
  )

  blueprint <- processed$blueprint
  outcomes_lst <- processed$terms_lst

  # nuke formula environment before returning
  formula_empty_env <- nuke_formula_environment(blueprint$formula)
  blueprint <- update_blueprint(blueprint, formula = formula_empty_env)

  ptypes <- out$ptypes$final(predictors_lst$ptype, outcomes_lst$ptype)
  extras <- out$extras$final(predictors_lst$extras, outcomes_lst$extras)

  out$mold$process(blueprint, predictors_lst$data, outcomes_lst$data, ptypes, extras)
}

mold_formula_default_process_predictors <- function(blueprint, data) {

  formula <- expand_formula_dot_notation(blueprint$formula, data)
  formula <- get_predictors_formula(formula)

  original_names <- get_all_predictors(formula, data)
  original_data <- data[, original_names, drop = FALSE]

  ptype <- extract_ptype(original_data)

  if (!blueprint$indicators) {
    factor_names <- extract_original_factor_names(ptype)
    validate_no_factors_in_functions(formula, factor_names)
    validate_no_factors_in_interactions(formula, factor_names)
    formula <- remove_factors_from_formula(formula, factor_names)
  }

  framed <- model_frame(formula, data)
  offset <- extract_offset(framed$terms, framed$data)

  predictors <- model_matrix(
    terms = framed$terms,
    data = framed$data
  )

  if (!blueprint$indicators) {
    predictors <- reattach_factor_columns(predictors, data, factor_names)
  }

  terms <- simplify_terms(framed$terms)

  blueprint_terms <- blueprint$terms
  blueprint_terms$predictors <- terms
  blueprint <- update_blueprint(blueprint, terms = blueprint_terms)

  predictors_lst <- out$mold$process_terms_lst(
    data = predictors,
    ptype = ptype,
    extras = list(offset = offset)
  )

  out$mold$process_terms(blueprint, predictors_lst)
}

mold_formula_default_process_outcomes <- function(blueprint, data) {

  formula <- blueprint$formula

  original_names <- get_all_outcomes(formula, data)
  original_data <- data[, original_names, drop = FALSE]

  ptype <- extract_ptype(original_data)

  formula <- get_outcomes_formula(formula)

  # used on the `~ LHS` formula
  validate_no_interactions(formula)

  framed <- model_frame(formula, data)

  outcomes <- flatten_embedded_columns(framed$data)

  terms <- simplify_terms(framed$terms)

  blueprint_terms <- blueprint$terms
  blueprint_terms$outcomes <- terms
  blueprint <- update_blueprint(blueprint, terms = blueprint_terms)

  outcomes_lst <- out$mold$process_terms_lst(data = outcomes, ptype)

  out$mold$process_terms(blueprint, outcomes_lst)
}

# ------------------------------------------------------------------------------

get_forge_formula_default_function_set <- function() {
  blueprint_function_set(forge_formula_default_clean, forge_formula_default_process)
}

forge_formula_default_clean <- function(blueprint, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  predictors <- shrink(new_data, blueprint$ptypes$predictors)

  predictors <- scream(
    predictors,
    blueprint$ptypes$predictors,
    allow_novel_levels = blueprint$allow_novel_levels
  )

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes)
    # Never allow novel levels for outcomes
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes)
  }
  else {
    outcomes <- NULL
  }

  out$forge$clean(blueprint, predictors, outcomes)
}

forge_formula_default_process <- function(blueprint, predictors, outcomes, extras) {
  processed <- forge_formula_default_process_predictors(
    blueprint = blueprint,
    predictors = predictors
  )

  blueprint <- processed$blueprint
  predictors_lst <- processed$terms_lst

  processed <- forge_formula_default_process_outcomes(
    blueprint = blueprint,
    outcomes = outcomes
  )

  blueprint <- processed$blueprint
  outcomes_lst <- processed$terms_lst

  extras <- c(
    extras,
    out$extras$final(predictors_lst$extras, outcomes_lst$extras)
  )

  out$forge$process(predictors_lst$data, outcomes_lst$data, extras)
}

forge_formula_default_process_predictors <- function(blueprint, predictors) {

  terms <- blueprint$terms$predictors
  terms <- alter_terms_environment(terms)

  framed <- model_frame(terms, predictors)

  data <- model_matrix(
    terms = framed$terms,
    data = framed$data
  )

  if (!blueprint$indicators) {
    factor_names <- extract_original_factor_names(blueprint$ptypes$predictors)
    data <- reattach_factor_columns(data, predictors, factor_names)
  }

  .offset <- extract_offset(framed$terms, framed$data)

  predictors_lst <- out$forge$process_terms_lst(
    data = data,
    extras = list(offset = .offset)
  )

  out$forge$process_terms(blueprint, predictors_lst)
}

forge_formula_default_process_outcomes <- function(blueprint, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    outcomes_lst <- out$forge$process_terms_lst()
    result <- out$forge$process_terms(blueprint, outcomes_lst)
    return(result)
  }

  terms <- blueprint$terms$outcomes
  terms <- alter_terms_environment(terms)

  framed <- model_frame(terms, outcomes)

  # Because model.matrix() does this for the RHS and we want
  # to be consistent even though we are only going through
  # model.frame()
  data <- flatten_embedded_columns(framed$data)

  outcomes_lst <- out$forge$process_terms_lst(data = data)

  out$forge$process_terms(blueprint, outcomes_lst)
}

# ------------------------------------------------------------------------------

# Is this a bad idea? We need it to forge() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in mold()
alter_terms_environment <- function(terms_blueprint) {
  env_above_global_env <- rlang::env_parent(rlang::global_env())
  attr(terms_blueprint, ".Environment") <- env_above_global_env
  terms_blueprint
}

# ------------------------------------------------------------------------------

expand_formula_dot_notation <- function(formula, data) {

  # Calling terms() on the formula, and providing
  # data will go ahead and expand the formula
  # if any `.` was present
  .terms <- terms(formula, data = data)

  rlang::new_formula(
    lhs = rlang::f_lhs(.terms),
    rhs = rlang::f_rhs(.terms),
    env = rlang::f_env(.terms)
  )
}

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

extract_original_factor_names <- function(ptype) {

  where_factor <- vapply(ptype, is.factor, logical(1))

  original_factor_columns <- colnames(ptype)[where_factor]

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

