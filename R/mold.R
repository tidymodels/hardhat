#' Mold data for modeling
#'
#' @description
#'
#' `mold()` applies the appropriate processing steps required to get training
#' data ready to be fed into a model.
#'
#' The return values of each method are all consistent with one another, but the
#' nuances of exactly what is being done for each method vary enough to warrant
#' separate help files for each. Click through to each one below:
#'
#' * XY Method - [mold.data.frame()] / [mold.matrix()]
#'
#' * Formula Method - [mold.formula()]
#'
#' * Recipes Method - [mold.recipe()]
#'
#' @param x A data frame, matrix, formula, or [recipes::recipe()]. If this is a
#' data.frame or matrix, it should contain the predictors.
#'
#' @param ... Currently unused.
#'
#' @return
#'
#' A named list containing 4 elements:
#'
#'  - `predictors`: A tibble containing the molded predictors to be used in the
#'  model.
#'
#'  - `outcome`: A tibble containing the molded outcomes to be used in the
#'  model.
#'
#'  - `preprocessor`: A `"preprocessor"` object for use when making predictions.
#'
#'  - `offset`: A tibble with a single column named `".offset"` if an offset
#'  was specified in the formula method. Otherwise, `NULL`.
#'
#' @export
mold <- function(x, ...) {
  UseMethod("mold")
}

#' @export
mold.default <- function(x, ...) {
  abort_unknown_mold_class(x)
}

#' Mold - XY Method
#'
#' @description
#'
#' For a data frame / matrix, `mold()` does the following:
#'
#' - Converts `x` to a tibble.
#'
#' - Adds an intercept column to `x` if `intercept = TRUE`.
#'
#' - Runs [standardize()] on `y`.
#'
#' @details
#'
#' As documented in [standardize()], if `y` is a _vector_, then the returned
#' outcomes tibble has 1 column with a standardized name of `".outcome"`.
#'
#' @inheritParams mold
#'
#' @param x A data frame or matrix containing the predictors.
#'
#' @param y A data frame, matrix, or vector containing the outcome(s).
#'
#' @param intercept A single logical specifying whether or not to
#' include an intercept in the molded predictors.
#'
#' @inherit mold return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # XY Example
#'
#' x <- iris[, c("Sepal.Width", "Species"), drop = FALSE]
#' y <- iris[, "Sepal.Length", drop = FALSE]
#'
#' processed <- mold(x, y)
#'
#' # The predictors are returned as a tibble
#' processed$predictors
#'
#' # So are the outcomes
#' processed$outcomes
#'
#' # A default preprocessor is also returned
#' # This contains all of the information required
#' # to preprocess new data at prediction time.
#' processed$preprocessor
#'
#' # The preprocessor should be stored in the model object
#' # and is later used by forge()
#' forge(processed$preprocessor, iris)
#'
#' # ---------------------------------------------------------------------------
#' # Y as a vector
#'
#' # Often, `y` will be supplied as a vector to a model. `mold()` handles
#' # this by letting `standardize()` convert `y` to a tibble, adding the
#' # default column name, `".outcome"`.
#' y_vec <- y$Sepal.Length
#'
#' mold(x, y_vec)$outcomes
#'
#' @rdname mold-xy
#'
#' @export
mold.data.frame <- function(x, y, intercept = FALSE, ...) {

  engine <- new_default_preprocessor_engine()

  x <- engine$process(x, intercept)
  y <- standardize(y)

  preprocessor <- new_default_preprocessor(
    engine = engine,
    intercept = intercept,
    predictors = predictors_lst(
      names = colnames(x),
      classes = get_data_classes(x),
      levels = get_levels(x)
    ),
    outcomes = outcomes_lst(
      names = colnames(y),
      classes = get_data_classes(y),
      levels = get_levels(y)
    )
  )

  mold_list(x, y, preprocessor)
}

#' @rdname mold-xy
#' @export
mold.matrix <- function(x, y, intercept = FALSE, ...) {

  engine <- new_default_preprocessor_engine()

  x <- engine$process(x, intercept)
  y <- standardize(y)

  preprocessor <- new_default_preprocessor(
    engine = engine,
    intercept = intercept,
    predictors = predictors_lst(
      names = colnames(x),
      classes = get_data_classes(x),
      levels = NULL
    ),
    outcomes = outcomes_lst(
      names = colnames(y),
      classes = get_data_classes(y),
      levels = get_levels(y)
    )
  )

  mold_list(x, y, preprocessor)
}

#' Mold - Formula Method
#'
#' @description
#'
#' For a formula, `mold()` does the following:
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
#'    - If `indicators = FALSE`, it does not expand factors with
#'    `model.matrix()`, which also means that interactions are not expanded.
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
#' @inheritParams mold.data.frame
#'
#' @param formula A formula specifying the terms of the model, with the outcomes
#' on the left-hand side of the formula, and the predictors on the right-hand
#' side.
#'
#' @param data A data frame containing the predictors and the outcomes.
#'
#' @param indicators Should factors and interactions be expanded
#' (In other words, should [stats::model.matrix()] be run)? If
#' `FALSE`, factor columns are returned without being expanded into dummy
#' variables and a warning is thrown if any interactions are detected.
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
#' required on the outcomes you are better off using a [recipes::recipe()].
#'
#' Global variables are _not_ allowed in the formula. An error will be thrown
#' if they are included. All terms in the formula should come from `data`.
#'
#' By default, intercepts are _not_ included in the predictor output from the
#' formula. To include an intercept, set `intercept = TRUE`. Having an intercept
#' argument is consistent with the other `mold()` methods. More importantly,
#' there are often modeling packages where an intercept is either always or
#' never allowed (for example, the `earth` package), and they do some fancy
#' footwork to keep the user from providing or removing an intercept.
#' This interface standardizes all of that flexibility in one place.
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
#' named `".offset"` in the `$outcome` slot of the return value.
#'
#' @inherit mold return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Formula example
#'
#' processed <- mold(Sepal.Width ~ Species, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' processed$outcomes
#'
#' # ---------------------------------------------------------------------------
#' # Factors without an intercept
#'
#' # No intercept is added
#' processed <- mold(Sepal.Width ~ Species, iris)
#'
#' # So factor columns are completely expanded
#' # into all `K` columns (the number of levels)
#' processed$predictors
#'
#' # ---------------------------------------------------------------------------
#' # Global variables
#'
#' y <- rep(1, times = nrow(iris))
#'
#' # In base R, global variables are allowed in a model formula
#' frame <- model.frame(Species ~ y + Sepal.Length, iris)
#' head(frame)
#'
#' # mold() does not allow them, and throws an error
#' tryCatch(
#'   expr = mold(Species ~ y + Sepal.Length, iris),
#'   error = function(e) print(e$message)
#' )
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables and interactions
#'
#' # By default, factor columns are expanded
#' # And interactions are created, both by
#' # calling model.matrix(). Some models (like
#' # tree based models) can take factors directly
#' # but still might want to use the formula method.
#' # In those cases, set `indicators = FALSE` to not
#' # run model.matrix()
#'
#' processed <- mold(Sepal.Width ~ Species, iris, indicators = FALSE)
#'
#' processed$predictors
#'
#' # No interactions are expanded when `indicators = FALSE`, but
#' # the terms specified in the interaction show up in the predictors
#' # tibble, with a warning
#' \dontrun{
#' mold(Sepal.Width ~ Sepal.Length:Species, iris, indicators = FALSE)
#' }
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas can be specified easily
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, iris)
#' processed$outcomes
#'
#' # Inline functions on the LHS are run, but any matrix
#' # output is flattened (like what happens in `model.matrix()`)
#' # (essentially this means you don't wind up with columns
#' # in the tibble that are matrices)
#' processed <- mold(poly(Sepal.Length, degree = 2) ~ Species, iris)
#' processed$outcomes
#'
#' # TRUE
#' ncol(processed$outcomes) == 2
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets are handled specially in base R, so they deserve special
#' # treatment here as well. You can add offsets using the inline function
#' # offset()
#' processed <- mold(Sepal.Width ~ offset(Sepal.Length) + Species, iris)
#'
#' processed$offset
#'
#' # Multiple offsets can be included, and they get added together
#' processed <- mold(Sepal.Width ~ offset(Sepal.Length) + offset(Petal.Width), iris)
#'
#' identical(
#'    processed$offset$.offset,
#'    iris$Sepal.Length + iris$Petal.Width
#' )
#'
#' @rdname mold-formula
#'
#' @export
mold.formula <- function(formula, data, intercept = FALSE,
                         indicators = TRUE, ...) {

  validate_formula_has_intercept(formula)

  formula <- remove_formula_intercept(formula, intercept)
  formula <- alter_formula_environment(formula)

  outcomes_formula <- get_outcomes_formula(formula)
  outcomes_frame <- model_frame(outcomes_formula, data)
  outcomes_terms <- extract_terms(outcomes_frame)

  predictors_formula <- get_predictors_formula(formula)
  predictors_frame <- model_frame(predictors_formula, data)
  predictors_terms <- extract_terms(predictors_frame)

  predictors <- extract_predictors(
    formula = predictors_formula,
    frame = predictors_frame,
    indicators = indicators,
    intercept = intercept
  )

  outcomes <- extract_outcomes(outcomes_frame)

  offset <- extract_offset(predictors_frame)

  original_predictor_nms <- get_all_predictors(formula, data)
  original_outcome_nms <- get_all_outcomes(formula, data)

  original_predictors <- data[, original_predictor_nms, drop = FALSE]
  original_outcomes <- data[, original_outcome_nms, drop = FALSE]

  preprocessor <- new_terms_preprocessor(
    engine = new_terms_preprocessor_engine(predictors_terms, outcomes_terms),
    intercept = intercept,
    predictors = predictors_lst(
      names = original_predictor_nms,
      classes = get_data_classes(original_predictors),
      levels = get_levels(original_predictors)
    ),
    outcomes = outcomes_lst(
      names = original_outcome_nms,
      classes = get_data_classes(original_outcomes),
      levels = get_levels(original_outcomes)
    ),
    indicators = indicators
  )

  mold_list(predictors, outcomes, preprocessor, offset)
}

#' Mold - Recipes Method
#'
#' @description
#'
#' For a recipe, `mold()` does the following:
#'
#' - Calls [recipes::prep()] to prep the recipe.
#'
#' - Calls [recipes::juice()] to extract the outcomes and predictors. These
#' are returned as tibbles.
#'
#' - If `intercept = TRUE`, adds an intercept column to the predictors.
#'
#' @inheritParams mold
#' @inheritParams mold.formula
#'
#' @param x An unprepped recipe created from [recipes::recipe()].
#'
#' @inherit mold return
#'
#' @examples
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Recipes example
#'
#' # Create a recipe that logs a predictor
#' rec <- recipe(Species ~ Sepal.Length + Sepal.Width, iris) %>%
#'    step_log(Sepal.Length)
#'
#' processed <- mold(rec, iris)
#'
#' # Sepal.Length has been logged
#' processed$predictors
#'
#' processed$outcomes
#'
#' # The underlying engine is a prepped recipe
#' processed$preprocessor$engine
#'
#' # ---------------------------------------------------------------------------
#' # With an intercept
#'
#' # You can add a predictor with `intercept = TRUE`
#' processed <- mold(rec, iris, intercept = TRUE)
#'
#' processed$predictors
#'
#' # But you also could have used a recipe step
#' rec2 <- step_intercept(rec)
#'
#' mold(rec2, iris)$predictors
#'
#' @rdname mold-recipe
#' @export
mold.recipe <- function(x, data, intercept = FALSE, ...) {

  validate_recipes_available()

  prepped_recipe <- recipes::prep(x, training = data)

  # recipes bug?
  all_predictors <- recipes::all_predictors
  all_outcomes <- recipes::all_outcomes

  # "composition" output is always tibble
  predictors <- recipes::juice(prepped_recipe, all_predictors())
  outcomes <- recipes::juice(prepped_recipe, all_outcomes())

  # un-retain training data
  prepped_recipe <- compost(prepped_recipe)

  all_levels <- get_original_recipe_levels(data, prepped_recipe)
  all_data_classes <- get_original_recipe_data_classes(data, prepped_recipe)

  preprocessor <- new_recipes_preprocessor(
    engine = prepped_recipe,
    intercept = intercept,
    predictors = predictors_lst(
      names = colnames(predictors),
      classes = all_data_classes$predictors,
      levels = all_levels$predictors
    ),
    outcomes = outcomes_lst(
      names = colnames(outcomes),
      classes = all_data_classes$outcomes,
      levels = all_levels$outcomes
    )
  )

  predictors <- maybe_add_intercept_column(predictors, intercept)

  mold_list(predictors, outcomes, preprocessor)
}

# ------------------------------------------------------------------------------
# Preparation helpers

mold_list <- function(predictors, outcomes, preprocessor, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    preprocessor = preprocessor,
    offset = offset
  )
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

get_original_recipe_levels <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictors = get_levels(x[, original_predictors, drop = FALSE]),
    outcomes = get_levels(x[, original_outcomes, drop = FALSE])
  )

}

get_original_recipe_data_classes <- function(x, rec) {

  roles <- rec$var_info$role
  original_predictors <- rec$var_info$variable[roles == "predictor"]
  original_outcomes <- rec$var_info$variable[roles == "outcome"]

  list(
    predictors = get_data_classes(x[, original_predictors, drop = FALSE]),
    outcomes = get_data_classes(x[, original_outcomes, drop = FALSE])
  )

}

extract_predictors <- function(formula, frame, indicators, intercept) {

  if (indicators) {
    # intercept is automatically added by model.matrix() if required
    predictors <- extract_predictors_with_model_matrix(formula, frame)
  }
  else {
    check_for_interactions(formula)
    predictors <- extract_predictors_from_frame(formula, frame, intercept)
  }

  tibble::as_tibble(predictors)
}

extract_outcomes <- function(frame) {
  attr(frame, "terms") <- NULL

  frame <- flatten_embedded_columns(frame)

  tibble::as_tibble(frame)
}

# We do this extra flattening because it happens on the RHS
# automatically because of the model.matrix() call. So this
# makes the column types consistent when doing something
# complex on the LHS like poly(, degree = 2) that returns
# a matrix
flatten_embedded_columns <- function(frame) {

  has_embedded_2D <- vapply(
    X = frame,
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
        !!!frame,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    )

    frame <- rlang::eval_bare(frame_flattener)
  }

  frame
}

extract_predictors_with_model_matrix <- function(formula, frame) {

  predictors <- rlang::with_options(
    model.matrix(formula, frame),
    na.action = "na.pass"
  )

  predictors <- strip_model_matrix(predictors)

  predictors
}

extract_predictors_from_frame <- function(terms, frame, intercept) {

  frame <- remove_offsets(frame)

  processed_outcome_nm <- response_name(terms)
  frame[[processed_outcome_nm]] <- NULL

  frame <- maybe_add_intercept_column(frame, intercept)

  frame
}

strip_model_matrix <- function(x) {
  attr(x, "assign") <- NULL
  attr(x, "dimnames") <- list(NULL, dimnames(x)[[2]])
  x
}

check_for_interactions <- function(formula) {

  formula_chr <- rlang::as_label(formula)

  has_interactions <- grepl(":", formula_chr)

  if (has_interactions) {
    rlang::warn(glue::glue(
      "Interaction terms have been detected in `formula`. ",
      "These are not expanded when `indicators = FALSE`, but the individual ",
      "terms will be included in the output."
    ))
  }

  invisible(formula)
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
