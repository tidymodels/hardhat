#' Default recipe engine
#'
#' This pages holds the details for the recipe preprocessing engine. This
#' is the engine used by default from `mold()` if `x` is a recipe.
#'
#' @inheritParams new_recipe_engine
#'
#' @param x An unprepped recipe created from [recipes::recipe()].
#'
#' @param data A data frame or matrix containing the outcomes and predictors.
#'
#' @param engine A preprocessing `engine`. If left as `NULL`, then a
#' [default_recipe_engine()] is used.
#'
#' @param ... Not used.
#'
#' @section Mold:
#'
#' When `mold()` is used with the default recipe engine:
#'
#' - It calls [recipes::prep()] to prep the recipe.
#'
#' - It calls [recipes::juice()] to extract the outcomes and predictors. These
#' are returned as tibbles.
#'
#' - If `intercept = TRUE`, adds an intercept column to the predictors.
#'
#' @section Forge:
#'
#' When `forge()` is used with the default recipe engine:
#'
#' - It calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - It calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - It calls [recipes::bake()] on the `new_data` using the prepped recipe
#' used during training.
#'
#' - It adds an intercept column onto `new_data` if `intercept = TRUE`.
#'
#' @examples
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Recipes example
#'
#' # Create a recipe that logs a predictor
#' rec <- recipe(Species ~ Sepal.Length + Sepal.Width, train) %>%
#'    step_log(Sepal.Length)
#'
#' processed <- mold(rec, train)
#'
#' # Sepal.Length has been logged
#' processed$predictors
#'
#' processed$outcomes
#'
#' # The underlying engine is a prepped recipe
#' processed$engine$recipe
#'
#' # Call forge() with the engine and the test data
#' # to have it preprocess the test data in the same way
#' forge(test, processed$engine)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome!
#' # This logged the Sepal.Length column of `new_data`
#' forge(test, processed$engine, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # With an intercept
#'
#' # You can add an intercept with `intercept = TRUE`
#' processed <- mold(rec, train, engine = default_recipe_engine(intercept = TRUE))
#'
#' processed$predictors
#'
#' # But you also could have used a recipe step
#' rec2 <- step_intercept(rec)
#'
#' mold(rec2, iris)$predictors
#'
#' @export
default_recipe_engine <- function(intercept = FALSE) {

  mold <- get_mold_recipe_default_function_set()
  forge <- get_forge_recipe_default_function_set()

  new_default_recipe_engine(
    mold = mold,
    forge = forge,
    intercept = intercept
  )

}

#' @rdname new-default-engine
#' @export
new_default_recipe_engine <- function(mold,
                                      forge,
                                      intercept = FALSE,
                                      ptypes = NULL,
                                      recipe = NULL,
                                      ...,
                                      subclass = character()) {

  new_recipe_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    ptypes = ptypes,
    recipe = recipe,
    ...,
    subclass = c(subclass, "default_recipe_engine")
  )

}

#' @export
refresh_engine.default_recipe_engine <- function(engine) {
  do.call(new_default_recipe_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

get_mold_recipe_default_function_set <- function() {
  engine_function_set(mold_recipe_default_clean, mold_recipe_default_process)
}

# mold - recipe - clean
mold_recipe_default_clean <- function(engine, data) {

  data <- check_is_data_like(data)

  out$mold$clean(engine, data)
}

# mold - recipe - process
mold_recipe_default_process <- function(engine, data) {

  # Prep for predictors and outcomes
  recipe <- recipes::prep(engine$recipe, training = data)
  engine <- update_engine(engine, recipe = recipe)

  c(engine, predictors_lst) %<-% mold_recipe_default_process_predictors(
    engine = engine,
    data = data
  )

  c(engine, outcomes_lst) %<-% mold_recipe_default_process_outcomes(
    engine = engine,
    data = data
  )

  # un-retain training data
  recipe <- compost(engine$recipe)
  engine <- update_engine(engine, recipe = recipe)

  ptypes <- out$ptypes$final(predictors_lst$ptype, outcomes_lst$ptype)
  extras <- out$extras$final(predictors_lst$extras, outcomes_lst$extras)

  out$mold$process(engine, predictors_lst$data, outcomes_lst$data, ptypes, extras)
}

mold_recipe_default_process_predictors <- function(engine, data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::juice(engine$recipe, all_predictors())

  predictors <- maybe_add_intercept_column(predictors, engine$intercept)

  ptype <- get_original_predictor_ptype(engine$recipe, data)

  predictors_lst <- out$mold$process_terms_lst(data = predictors, ptype)

  out$mold$process_terms(engine, predictors_lst)
}

mold_recipe_default_process_outcomes <- function(engine, data) {

  all_outcomes <- recipes::all_outcomes

  outcomes <- recipes::juice(engine$recipe, all_outcomes())

  ptype <- get_original_outcome_ptype(engine$recipe, data)

  outcomes_lst <- out$mold$process_terms_lst(data = outcomes, ptype)

  out$mold$process_terms(engine, outcomes_lst)
}

# ------------------------------------------------------------------------------

get_forge_recipe_default_function_set <- function() {
  engine_function_set(forge_recipe_default_clean, forge_recipe_default_process)
}

forge_recipe_default_clean <- function(engine, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  predictors <- shrink(new_data, engine$ptypes$predictors)
  predictors <- scream(predictors, engine$ptypes$predictors)

  if (outcomes) {
    outcomes <- shrink(new_data, engine$ptypes$outcomes)
    outcomes <- scream(outcomes, engine$ptypes$outcomes)
  }
  else {
    outcomes <- NULL
  }

  out$forge$clean(engine, predictors, outcomes)
}

forge_recipe_default_process <- function(engine, predictors, outcomes) {

  rec <- engine$recipe
  roles <- rec$term_info$role
  vars <- rec$term_info$variable

  # Can't move this inside core functions
  # predictors and outcomes both must be present
  baked_data <- recipes::bake(
    object = rec,
    new_data = vctrs::vec_cbind(predictors, outcomes)
  )

  processed_predictor_names <- vars[roles == "predictor"]
  predictors <- baked_data[, processed_predictor_names, drop = FALSE]

  if (!is.null(outcomes)) {
    processed_outcome_names <- vars[roles == "outcome"]
    outcomes <- baked_data[, processed_outcome_names, drop = FALSE]
  }

  c(engine, predictors_lst) %<-% forge_recipe_default_process_predictors(
    engine = engine,
    predictors = predictors
  )

  c(engine, outcomes_lst) %<-% forge_recipe_default_process_outcomes(
    engine = engine,
    outcomes = outcomes
  )

  extras <- c(predictors_lst$extras, outcomes_lst$extras)

  out$forge$process(engine, predictors_lst$data, outcomes_lst$data, extras)
}

forge_recipe_default_process_predictors <- function(engine, predictors) {

  predictors <- maybe_add_intercept_column(predictors, engine$intercept)

  predictors_lst <- out$forge$process_terms_lst(data = predictors)

  out$forge$process_terms(engine, predictors_lst)
}

forge_recipe_default_process_outcomes <- function(engine, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    outcomes_lst <- out$forge$process_terms_lst()
    result <- out$forge$process_terms(engine, outcomes_lst)
    return(result)
  }

  outcomes_lst <- out$forge$process_terms_lst(data = outcomes)

  out$forge$process_terms(engine, outcomes_lst)
}

# ------------------------------------------------------------------------------

get_original_predictor_ptype <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "predictor"]

  original_data <- data[, original_names, drop = FALSE]

  extract_ptype(original_data)
}

get_original_outcome_ptype <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "outcome"]

  original_data <- data[, original_names, drop = FALSE]

  extract_ptype(original_data)
}
