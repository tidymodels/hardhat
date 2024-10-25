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
#' dummy variables when `indicators = "traditional"` and an intercept is _not_
#' present is not always intuitive and should be documented.
#'
#' - When an intercept is present, factors are expanded into `K-1` new columns,
#' where `K` is the number of levels in the factor.
#'
#' - When an intercept is _not_ present, the first factor is expanded into
#' all `K` columns (one-hot encoding), and the remaining factors are expanded
#' into `K-1` columns. This behavior ensures that meaningful predictions can
#' be made for the reference level of the first factor, but is not the exact
#' "no intercept" model that was requested. Without this behavior, predictions
#' for the reference level of the first factor would always be forced to `0`
#' when there is no intercept.
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
#'    - If `indicators = "traditional"`, it then runs [stats::model.matrix()]
#'    on the result.
#'
#'    - If `indicators = "none"`, factors are removed before `model.matrix()`
#'    is run, and then added back afterwards. No interactions or inline
#'    functions involving factors are allowed.
#'
#'    - If `indicators = "one_hot"`, it then runs [stats::model.matrix()] on the
#'    result using a contrast function that creates indicator columns for all
#'    levels of all factors.
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
#'    - If, in the original [mold()] call, `indicators = "traditional"` was
#'    set, it then runs [stats::model.matrix()] on the result.
#'
#'    - If, in the original [mold()] call, `indicators = "none"` was set, it
#'    runs [stats::model.matrix()] on the result without the factor columns,
#'    and then adds them on afterwards.
#'
#'    - If, in the original [mold()] call, `indicators = "one_hot"` was set, it
#'    runs [stats::model.matrix()] on the result with a contrast function that
#'    includes indicators for all levels of all factor columns.
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
#' if they are included. All terms in the formula should come from `data`. If
#' you need to use inline functions in the formula, the safest way to do so is
#' to prefix them with their package name, like `pkg::fn()`. This ensures that
#' the function will always be available at `mold()` (fit) and `forge()`
#' (prediction) time. That said, if the package is _attached_
#' (i.e. with `library()`), then you should be able to use the inline function
#' without the prefix.
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
#'
#' data("hardhat-example-data")
#'
#' # ---------------------------------------------------------------------------
#' # Formula Example
#'
#' # Call mold() with the training data
#' processed <- mold(
#'   log(num_1) ~ num_2 + fac_1,
#'   example_train,
#'   blueprint = default_formula_blueprint(intercept = TRUE)
#' )
#'
#' # Then, call forge() with the blueprint and the test data
#' # to have it preprocess the test data in the same way
#' forge(example_test, processed$blueprint)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome
#' forge(example_test, processed$blueprint, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Factors without an intercept
#'
#' # No intercept is added by default
#' processed <- mold(num_1 ~ fac_1 + fac_2, example_train)
#'
#' # So, for factor columns, the first factor is completely expanded into all
#' # `K` columns (the number of levels), and the subsequent factors are expanded
#' # into `K - 1` columns.
#' processed$predictors
#'
#' # In the above example, `fac_1` is expanded into all three columns,
#' # `fac_2` is not. This behavior comes from `model.matrix()`, and is somewhat
#' # known in the R community, but can lead to a model that is difficult to
#' # interpret since the corresponding p-values are testing wildly different
#' # hypotheses.
#'
#' # To get all indicators for all columns (irrespective of the intercept),
#' # use the `indicators = "one_hot"` option
#' processed <- mold(
#'   num_1 ~ fac_1 + fac_2,
#'   example_train,
#'   blueprint = default_formula_blueprint(indicators = "one_hot")
#' )
#'
#' processed$predictors
#'
#' # It is not possible to construct a no-intercept model that expands all
#' # factors into `K - 1` columns using the formula method. If required, a
#' # recipe could be used to construct this model.
#'
#' # ---------------------------------------------------------------------------
#' # Global variables
#'
#' y <- rep(1, times = nrow(example_train))
#'
#' # In base R, global variables are allowed in a model formula
#' frame <- model.frame(fac_1 ~ y + num_2, example_train)
#' head(frame)
#'
#' # mold() does not allow them, and throws an error
#' try(mold(fac_1 ~ y + num_2, example_train))
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables and interactions
#'
#' # By default, factor columns are expanded
#' # and interactions are created, both by
#' # calling `model.matrix()`. Some models (like
#' # tree based models) can take factors directly
#' # but still might want to use the formula method.
#' # In those cases, set `indicators = "none"` to not
#' # run `model.matrix()` on factor columns. Interactions
#' # are still allowed and are run on numeric columns.
#'
#' bp_no_indicators <- default_formula_blueprint(indicators = "none")
#'
#' processed <- mold(
#'   ~ fac_1 + num_1:num_2,
#'   example_train,
#'   blueprint = bp_no_indicators
#' )
#'
#' processed$predictors
#'
#' # An informative error is thrown when `indicators = "none"` and
#' # factors are present in interaction terms or in inline functions
#' try(mold(num_1 ~ num_2:fac_1, example_train, blueprint = bp_no_indicators))
#' try(mold(num_1 ~ paste0(fac_1), example_train, blueprint = bp_no_indicators))
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas can be specified easily
#' processed <- mold(num_1 + log(num_2) ~ fac_1, example_train)
#' processed$outcomes
#'
#' # Inline functions on the LHS are run, but any matrix
#' # output is flattened (like what happens in `model.matrix()`)
#' # (essentially this means you don't wind up with columns
#' # in the tibble that are matrices)
#' processed <- mold(poly(num_2, degree = 2) ~ fac_1, example_train)
#' processed$outcomes
#'
#' # TRUE
#' ncol(processed$outcomes) == 2
#'
#' # Multivariate formulas specified in mold()
#' # carry over into forge()
#' forge(example_test, processed$blueprint, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets are handled specially in base R, so they deserve special
#' # treatment here as well. You can add offsets using the inline function
#' # `offset()`
#' processed <- mold(num_1 ~ offset(num_2) + fac_1, example_train)
#'
#' processed$extras$offset
#'
#' # Multiple offsets can be included, and they get added together
#' processed <- mold(
#'   num_1 ~ offset(num_2) + offset(num_3),
#'   example_train
#' )
#'
#' identical(
#'   processed$extras$offset$.offset,
#'   example_train$num_2 + example_train$num_3
#' )
#'
#' # Forging test data will also require
#' # and include the offset
#' forge(example_test, processed$blueprint)
#'
#' # ---------------------------------------------------------------------------
#' # Intercept only
#'
#' # Because `1` and `0` are intercept modifying terms, they are
#' # not allowed in the formula and are instead controlled by the
#' # `intercept` argument of the blueprint. To use an intercept
#' # only formula, you should supply `NULL` on the RHS of the formula.
#' mold(
#'   ~NULL,
#'   example_train,
#'   blueprint = default_formula_blueprint(intercept = TRUE)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Matrix output for predictors
#'
#' # You can change the `composition` of the predictor data set
#' bp <- default_formula_blueprint(composition = "dgCMatrix")
#' processed <- mold(log(num_1) ~ num_2 + fac_1, example_train, blueprint = bp)
#' class(processed$predictors)
#' @export
default_formula_blueprint <- function(intercept = FALSE,
                                      allow_novel_levels = FALSE,
                                      indicators = "traditional",
                                      composition = "tibble") {
  new_default_formula_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    indicators = indicators,
    composition = composition
  )
}

#' @param terms A named list of two elements, `predictors` and `outcomes`. Both
#' elements are `terms` objects that describe the terms for the outcomes and
#' predictors separately. This argument is set automatically at [mold()] time.
#'
#' @param levels Either `NULL` or a named list of character vectors that
#' correspond to the levels observed when converting character predictor columns
#' to factors during [mold()]. This argument is set automatically at [mold()]
#' time.
#'
#' @rdname new-default-blueprint
#' @export
new_default_formula_blueprint <- function(intercept = FALSE,
                                          allow_novel_levels = FALSE,
                                          ptypes = NULL,
                                          formula = NULL,
                                          indicators = "traditional",
                                          composition = "tibble",
                                          terms = list(
                                            predictors = NULL,
                                            outcomes = NULL
                                          ),
                                          levels = NULL,
                                          ...,
                                          subclass = character()) {
  check_terms_list(terms)
  check_levels(levels, allow_null = TRUE)

  new_formula_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    ptypes = ptypes,
    formula = formula,
    indicators = indicators,
    composition = composition,
    terms = terms,
    levels = levels,
    ...,
    subclass = c(subclass, "default_formula_blueprint")
  )
}

#' @export
refresh_blueprint.default_formula_blueprint <- function(blueprint) {
  do.call(new_default_formula_blueprint, as.list(blueprint))
}

# ------------------------------------------------------------------------------

#' @param data A data frame or matrix containing the outcomes and predictors.
#'
#' @rdname run-mold
#' @export
run_mold.default_formula_blueprint <- function(blueprint, ..., data) {
  check_dots_empty0(...)

  cleaned <- mold_formula_default_clean(blueprint = blueprint, data = data)

  blueprint <- cleaned$blueprint
  data <- cleaned$data

  mold_formula_default_process(blueprint = blueprint, data = data)
}

# ------------------------------------------------------------------------------
# mold - formula - clean

mold_formula_default_clean <- function(blueprint, data) {
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  # Check here, not in the constructor, because we
  # put a non-intercept-containing formula back in
  check_implicit_intercept(blueprint$formula, arg = "formula")

  formula <- remove_formula_intercept(blueprint$formula, blueprint$intercept)
  formula <- alter_formula_environment(formula)

  blueprint <- update_blueprint0(blueprint, formula = formula)

  new_mold_clean(blueprint, data)
}

check_implicit_intercept <- function(x,
                                     ...,
                                     arg = caller_arg(x),
                                     call = caller_env()) {
  # The formula must have an implicit intercept to remove.
  # Don't let the user do `0+` or `+0` or `-1`.
  x <- f_rhs(x)
  check_not_1_or_0(x, arg = arg, call = call)
  recurse_intercept_search(x, arg = arg, call = call)
}

check_not_1_or_0 <- function(x,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()) {
  if (!is_scalar_integerish(x)) {
    return(invisible(NULL))
  }

  if (x == 1) {
    cli::cli_abort(
      "{.arg {arg}} must not contain the intercept term, `1`.",
      call = call
    )
  }

  if (x == 0) {
    cli::cli_abort(
      "{.arg {arg}} must not contain the intercept removal term, `0`.",
      call = call
    )
  }

  invisible(NULL)
}

recurse_intercept_search <- function(x,
                                     ...,
                                     arg = caller_arg(x),
                                     call = caller_env()) {
  if (!is_call(x)) {
    return(invisible(NULL))
  }

  call_name <- call_name(x)
  call_args <- call_args(x)

  # Check for `+ 0` or `0 +`
  if (is_string(call_name, string = "+")) {
    for (call_arg in call_args) {
      if (call_arg == 0L) {
        cli::cli_abort(
          "{.arg {arg}} must not contain the intercept removal term: `+ 0` or `0 +`.",
          call = call
        )
      }
    }
  }

  # Check for `- 1`
  if (is_string(call_name, string = "-")) {
    if (length(call_args) == 2L) {
      call_arg <- call_args[[2]]
    }

    if (length(call_args) == 1L) {
      call_arg <- call_args[[1]]
    }

    if (call_arg == 1L) {
      cli::cli_abort(
        "{.arg {arg}} must not contain the intercept removal term: `- 1`.",
        call = call
      )
    }
  }

  # Recurse
  for (call_arg in call_args) {
    recurse_intercept_search(call_arg, arg = arg, call = call)
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------
# mold - formula - process

mold_formula_default_process <- function(blueprint, data) {
  processed <- mold_formula_default_process_predictors(
    blueprint = blueprint,
    data = data
  )

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_ptype <- processed$ptype
  predictors_extras <- processed$extras

  processed <- mold_formula_default_process_outcomes(
    blueprint = blueprint,
    data = data
  )

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_ptype <- processed$ptype
  outcomes_extras <- processed$extras

  # nuke formula environment before returning
  formula_empty_env <- nuke_formula_environment(blueprint$formula)
  blueprint <- update_blueprint0(blueprint, formula = formula_empty_env)

  ptypes <- new_ptypes(predictors_ptype, outcomes_ptype)
  extras <- new_extras(predictors_extras, outcomes_extras)

  blueprint <- update_blueprint0(blueprint, ptypes = ptypes)

  new_mold_process(predictors, outcomes, blueprint, extras)
}

mold_formula_default_process_predictors <- function(blueprint, data) {
  formula <- expand_formula_dot_notation(blueprint$formula, data)
  formula <- get_predictors_formula(formula)

  original_names <- get_all_predictors(formula, data)
  data <- data[original_names]

  ptype <- extract_ptype(data)

  if (identical(blueprint$indicators, "traditional") ||
      identical(blueprint$indicators, "one_hot")) {
    # Early convert character columns to factors and capture the factor levels
    # for use in `forge()`. Do this after collecting the `ptype` on the original
    # data to ensure it is untouched. Converting with just `factor()` and
    # letting levels be sorted (rather than by appearance) to match base R and
    # recipes `prep(strings_as_factors = TRUE)`.
    characters <- map_lgl(ptype, is.character)
    characters <- names(ptype)[characters]
    data[characters] <- map(data[characters], factor)
    levels <- map(data[characters], levels)
  } else if (identical(blueprint$indicators, "none")) {
    # We don't convert character columns to factors with `indicators = "none"`
    levels <- list()
  }

  blueprint <- update_blueprint0(blueprint, levels = levels)

  if (identical(blueprint$indicators, "none")) {
    factorish_names <- extract_original_factorish_names(ptype)
    factorish_data <- data[factorish_names]
    check_no_factorish_in_interactions(formula, factorish_names)
    check_no_factorish_in_functions(formula, factorish_names)
    formula <- remove_factorish_from_formula(formula, factorish_names)
    data <- mask_factorish_in_data(data, factorish_names)
  }

  framed <- model_frame(formula, data)
  offset <- extract_offset(framed$terms, framed$data)

  if (identical(blueprint$indicators, "one_hot")) {
    predictors <- model_matrix_one_hot(
      terms = framed$terms,
      data = framed$data
    )
  } else {
    predictors <- model_matrix(
      terms = framed$terms,
      data = framed$data
    )
  }

  if (identical(blueprint$indicators, "none")) {
    predictors <- unmask_factorish_in_data(predictors, factorish_data)
  }

  terms <- simplify_terms(framed$terms)

  predictors <- recompose(predictors, composition = blueprint$composition)

  blueprint_terms <- blueprint$terms
  blueprint_terms$predictors <- terms
  blueprint <- update_blueprint0(blueprint, terms = blueprint_terms)

  new_mold_process_terms(
    blueprint = blueprint,
    data = predictors,
    ptype = ptype,
    extras = list(offset = offset)
  )
}

mold_formula_default_process_outcomes <- function(blueprint, data) {
  formula <- blueprint$formula

  original_names <- get_all_outcomes(formula, data)
  data <- data[original_names]

  ptype <- extract_ptype(data)

  formula <- get_outcomes_formula(formula)

  # used on the `~ LHS` formula
  check_no_interactions(formula)

  framed <- model_frame(formula, data)

  outcomes <- flatten_embedded_columns(framed$data)

  terms <- simplify_terms(framed$terms)

  blueprint_terms <- blueprint$terms
  blueprint_terms$outcomes <- terms
  blueprint <- update_blueprint0(blueprint, terms = blueprint_terms)

  new_mold_process_terms(
    blueprint = blueprint,
    data = outcomes,
    ptype = ptype
  )
}

# ------------------------------------------------------------------------------

#' @rdname run-forge
#' @export
run_forge.default_formula_blueprint <- function(blueprint,
                                                new_data,
                                                ...,
                                                outcomes = FALSE) {
  check_dots_empty0(...)

  cleaned <- forge_formula_default_clean(
    blueprint = blueprint,
    new_data = new_data,
    outcomes = outcomes
  )

  blueprint <- cleaned$blueprint
  predictors <- cleaned$predictors
  outcomes <- cleaned$outcomes
  extras <- cleaned$extras

  forge_formula_default_process(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

# ------------------------------------------------------------------------------

forge_formula_default_clean <- function(blueprint, new_data, outcomes) {
  check_data_frame_or_matrix(new_data)
  new_data <- coerce_to_tibble(new_data)
  check_unique_column_names(new_data)
  check_bool(outcomes)

  # If character columns were converted to factor at `mold()` time,
  # reapply that to the predictors `ptype` here (#213)
  predictors_levels <- blueprint_levels(blueprint)
  predictors_characters <- names2(predictors_levels)
  predictors_ptype <- blueprint$ptypes$predictors
  predictors_ptype[predictors_characters] <- map2(
    predictors_ptype[predictors_characters],
    predictors_levels,
    function(col, levels) factor(col, levels = levels)
  )

  predictors <- shrink(new_data, predictors_ptype)

  predictors <- scream(
    predictors,
    predictors_ptype,
    allow_novel_levels = blueprint$allow_novel_levels
  )

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes)
    # Never allow novel levels for outcomes
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes)
  } else {
    outcomes <- NULL
  }

  new_forge_clean(blueprint, predictors, outcomes)
}

# ------------------------------------------------------------------------------

forge_formula_default_process <- function(blueprint, predictors, outcomes, extras) {
  processed <- forge_formula_default_process_predictors(
    blueprint = blueprint,
    predictors = predictors
  )

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_extras <- processed$extras

  processed <- forge_formula_default_process_outcomes(
    blueprint = blueprint,
    outcomes = outcomes
  )

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_extras <- processed$extras

  extras <- c(
    extras,
    new_extras(predictors_extras, outcomes_extras)
  )

  new_forge_process(predictors, outcomes, extras)
}

forge_formula_default_process_predictors <- function(blueprint, predictors) {
  terms <- blueprint$terms$predictors
  terms <- alter_terms_environment(terms)

  if (identical(blueprint$indicators, "none")) {
    factorish_names <- extract_original_factorish_names(blueprint$ptypes$predictors)
    factorish_predictors <- predictors[factorish_names]
    predictors <- mask_factorish_in_data(predictors, factorish_names)
  }

  framed <- model_frame(terms, predictors)

  if (identical(blueprint$indicators, "one_hot")) {
    data <- model_matrix_one_hot(
      terms = framed$terms,
      data = framed$data
    )
  } else {
    data <- model_matrix(
      terms = framed$terms,
      data = framed$data
    )
  }

  if (identical(blueprint$indicators, "none")) {
    data <- unmask_factorish_in_data(data, factorish_predictors)
  }

  data <- recompose(data, composition = blueprint$composition)

  offset <- extract_offset(framed$terms, framed$data)

  extras <- list(offset = offset)

  new_forge_process_terms(
    blueprint = blueprint,
    data = data,
    extras = extras
  )
}

forge_formula_default_process_outcomes <- function(blueprint, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    result <- new_forge_process_terms(
      blueprint = blueprint,
      data = outcomes
    )
    return(result)
  }

  terms <- blueprint$terms$outcomes
  terms <- alter_terms_environment(terms)

  framed <- model_frame(terms, outcomes)

  # Because model.matrix() does this for the RHS and we want
  # to be consistent even though we are only going through
  # model.frame()
  data <- flatten_embedded_columns(framed$data)

  new_forge_process_terms(
    blueprint = blueprint,
    data = data
  )
}

# ------------------------------------------------------------------------------

# Is this a bad idea? We need it to forge() terms where
# an inline function may have been used like poly(), but there
# is no gurantee that the env above the global env is the same
# as the one that was used in mold()
alter_terms_environment <- function(terms_blueprint) {
  env_above_global_env <- env_parent(global_env())
  attr(terms_blueprint, ".Environment") <- env_above_global_env
  terms_blueprint
}

# ------------------------------------------------------------------------------

blueprint_levels <- function(x) {
  # See #213
  if (has_name(x, "levels")) {
    # Blueprint is new enough to have this field
    x[["levels"]]
  } else {
    # Backwards compatible support if the blueprint is old
    list()
  }
}

# ------------------------------------------------------------------------------

expand_formula_dot_notation <- function(formula, data) {

  # Calling terms() on the formula, and providing
  # data will go ahead and expand the formula
  # if any `.` was present
  .terms <- terms(formula, data = data)

  new_formula(
    lhs = f_lhs(.terms),
    rhs = f_rhs(.terms),
    env = f_env(.terms)
  )
}

nuke_formula_environment <- function(formula) {
  new_formula(
    lhs = f_lhs(formula),
    rhs = f_rhs(formula),
    env = empty_env()
  )
}

check_terms_list <- function(x,
                             ...,
                             arg = caller_arg(x),
                             call = caller_env()) {
  check_list(x, arg = arg, call = call)

  check_has_name(x = x, name = "predictors", arg = arg, call = call)
  check_has_name(x = x, name = "outcomes", arg = arg, call = call)

  check_terms(
    x = x$predictors,
    allow_null = TRUE,
    arg = cli::format_inline("{arg}$predictors"),
    call = call
  )
  check_terms(
    x = x$outcomes,
    allow_null = TRUE,
    arg = cli::format_inline("{arg}$outcomes"),
    call = call
  )

  invisible(NULL)
}

check_levels <- function(x,
                         ...,
                         allow_null = FALSE,
                         arg = caller_arg(x),
                         call = caller_env()) {
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  check_list(x, arg = arg, call = call)

  if (!is_named2(x)) {
    cli::cli_abort("{.arg {arg}} must be fully named.", call = call)
  }

  if (!all(map_lgl(x, is.character))) {
    cli::cli_abort("{.arg {arg}} must only contain character vectors.", call = call)
  }

  invisible(NULL)
}

alter_formula_environment <- function(formula) {

  # formula environment is 1 step above global env to avoid
  # global variables but maintain ability to use pkg functions
  # (like stats::poly())
  env_above_global_env <- env_parent(global_env())

  new_formula(
    lhs = f_lhs(formula),
    rhs = f_rhs(formula),
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
    frame_flattener <- expr(
      data.frame(
        !!!data,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )

    data <- eval_bare(frame_flattener)
    data <- hardhat_new_tibble(data, size = vec_size(data))
  }

  data
}

check_no_factorish_in_functions <- function(f, names, error_call = caller_env()) {
  expr <- f_rhs(f)

  expr_check_no_factorish_in_functions(
    expr = expr,
    names = names,
    error_call = error_call,
    in_allowed_subset = TRUE
  )
}
expr_check_no_factorish_in_functions <- function(expr,
                                                 names,
                                                 error_call,
                                                 in_allowed_subset = FALSE) {
  if (!is_call(expr)) {
    return(invisible(NULL))
  }

  original_expr <- expr
  expr <- expr[-1L]

  # Are we continuing an existing chain of allowed functions?
  in_allowed_subset <- in_allowed_subset &&
    is_call(original_expr, name = c("+", "-", "("))

  for (i in seq_along(expr)) {
    elt <- expr[[i]]

    if (!in_allowed_subset && is_symbol(elt, name = names)) {
      name <- as_string(elt)
      expr <- as_label(original_expr)

      message <- c(
        "Functions involving factors or characters have been detected on the
        RHS of {.arg formula}. These are not allowed when {.code indicators = \"none\"}.",
        i = "Functions involving factors were detected for {.val {name}} in {.arg {expr}}."
      )

      cli::cli_abort(message, call = error_call)
    }

    expr_check_no_factorish_in_functions(
      expr = elt,
      names = names,
      in_allowed_subset = in_allowed_subset,
      error_call = error_call
    )
  }

  invisible(NULL)
}

check_no_factorish_in_interactions <- function(f, names, error_call = caller_env()) {
  expr <- f_rhs(f)
  expr_check_no_factorish_in_interactions(expr, names, error_call = error_call)
}
expr_check_no_factorish_in_interactions <- function(expr, names, error_call) {
  if (!is_call(expr)) {
    return(invisible(NULL))
  }

  if (is_call(expr, name = fns_interactions())) {
    expr_original <- expr
    expr <- expr[-1L]

    for (i in seq_along(expr)) {
      expr_check_no_factorish_in_interaction_term(
        expr = expr[[i]],
        names = names,
        expr_original = expr_original,
        error_call = error_call
      )
    }
  } else {
    expr <- expr[-1L]

    for (i in seq_along(expr)) {
      expr_check_no_factorish_in_interactions(
        expr = expr[[i]],
        names = names,
        error_call = error_call
      )
    }
  }

  invisible(NULL)
}
expr_check_no_factorish_in_interaction_term <- function(expr,
                                                        names,
                                                        expr_original,
                                                        error_call) {
  if (is_symbol(expr, name = names)) {
    name <- as_string(expr)
    expr <- as_label(expr_original)

    message <- c(
      "Interaction terms involving factors or characters have been detected on the
      RHS of {.arg formula}. These are not allowed when {.code indicators = \"none\"}.",
      i = "Interactions terms involving factors were detected for {.val {name}} in {.arg {expr}}."
    )

    cli::cli_abort(message, call = error_call)
  }

  if (!is_call(expr)) {
    return(invisible(NULL))
  }

  expr <- expr[-1L]

  for (i in seq_along(expr)) {
    expr_check_no_factorish_in_interaction_term(
      expr = expr[[i]],
      names = names,
      expr_original = expr_original,
      error_call = error_call
    )
  }

  invisible(NULL)
}

check_no_interactions <- function(f, error_call = caller_env()) {
  expr <- f_rhs(f)
  expr_check_no_interactions(expr, error_call = error_call)
}
expr_check_no_interactions <- function(expr, error_call) {
  if (!is_call(expr)) {
    return(invisible(NULL))
  }

  if (is_call(expr, name = fns_interactions())) {
    expr <- as_label(expr)

    message <- c(
      "Interaction terms can't be specified on the LHS of `formula`.",
      i = "The following interaction term was found: {.arg {expr}}."
    )

    cli::cli_abort(message, call = error_call)
  }

  expr <- expr[-1L]

  for (i in seq_along(expr)) {
    expr_check_no_interactions(expr[[i]], error_call = error_call)
  }

  invisible(NULL)
}

fns_interactions <- function() {
  c(":", "*", "^", "/", "%in%")
}

extract_original_factorish_names <- function(ptype) {
  where_factorish <- vapply(ptype, is_factorish, logical(1))

  original_factorish_columns <- colnames(ptype)[where_factorish]

  original_factorish_columns
}

is_factorish <- function(x) {
  is.factor(x) || is.character(x)
}

remove_factorish_from_formula <- function(.formula, .factorish_names) {
  if (length(.factorish_names) == 0L) {
    return(.formula)
  }

  .factorish_syms <- syms(.factorish_names)

  .f_rhs <- f_rhs(.formula)

  for (.factorish_sym in .factorish_syms) {
    .f_rhs <- expr(!!.f_rhs - !!.factorish_sym)
  }

  new_formula(
    lhs = f_lhs(.formula),
    rhs = .f_rhs,
    env = f_env(.formula)
  )
}

mask_factorish_in_data <- function(data, factorish_names) {
  # Replace factorish columns with columns of repeated `1L`s (#213).
  # Hopefully this is as close to a no-op as we can get in `model.matrix()`.
  if (length(factorish_names) > 0L) {
    ones <- vec_rep(1L, times = nrow(data))
    ones <- list(ones)
    data[factorish_names] <- ones
  }

  data
}

unmask_factorish_in_data <- function(data, factorish_data) {
  factorish_names <- names(factorish_data)

  if (length(factorish_names) > 0L) {
    data[factorish_names] <- factorish_data
  }

  data
}

get_predictors_formula <- function(formula) {
  new_formula(
    lhs = NULL,
    rhs = f_rhs(formula),
    env = f_env(formula)
  )
}

get_outcomes_formula <- function(formula) {
  new_formula <- new_formula(
    lhs = NULL,
    rhs = f_lhs(formula),
    env = f_env(formula)
  )

  remove_formula_intercept(new_formula, intercept = FALSE)
}
