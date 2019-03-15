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
#' # ---------------------------------------------------------------------------
#' # Non standard roles
#'
#' # If you have custom recipe roles, they are processed and returned in
#' # the `$extras$roles` slot of the return value of `mold()` and `forge()`.
#'
#' rec_roles <- recipe(train) %>%
#'   update_role(Sepal.Width, new_role = "predictor") %>%
#'   update_role(Species, new_role = "outcome") %>%
#'   update_role(Sepal.Length, new_role = "custom_role") %>%
#'   update_role(Petal.Length, new_role = "custom_role2")
#'
#' processed_roles <- mold(rec_roles, train)
#'
#' processed_roles$extras
#'
#' forge(test, processed_roles$engine)
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

#' @param extra_role_ptypes A named list. The names are the unique non-standard
#' recipe roles (i.e. everything except `"predictors"` and `"outcomes"`). The
#' values are prototypes of the original columns with that role. These are
#' used for validation in `forge()`.
#'
#' @rdname new-default-engine
#' @export
new_default_recipe_engine <- function(mold,
                                      forge,
                                      intercept = FALSE,
                                      ptypes = NULL,
                                      recipe = NULL,
                                      extra_role_ptypes = NULL,
                                      ...,
                                      subclass = character()) {

  new_recipe_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    ptypes = ptypes,
    recipe = recipe,
    extra_role_ptypes = extra_role_ptypes,
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

  c(engine, extras) %<-% mold_recipe_default_process_extras(engine, data)

  extras <- c(
    extras,
    out$extras$final(predictors_lst$extras, outcomes_lst$extras)
  )

  # un-retain training data
  engine <- update_engine(engine, recipe = compost(engine$recipe))

  ptypes <- out$ptypes$final(predictors_lst$ptype, outcomes_lst$ptype)

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

mold_recipe_default_process_extras <- function(engine, data) {

  # Capture original non standard role columns (anything but "predictor"
  # or "outcome"). These are required in `new_data`
  original_extra_role_cols <- get_extra_role_columns(
    engine$recipe,
    data
  )

  if (!is.null(original_extra_role_cols)) {
    engine <- update_engine(
      engine,
      extra_role_ptypes = lapply(original_extra_role_cols, extract_ptype)
    )
  }

  # Return the processed non standard role columns
  # (could be generated by prep(), not required in `new_data`)
  processed_extra_role_cols <- get_extra_role_columns(
    engine$recipe,
    recipes::juice(engine$recipe),
    original = FALSE
  )

  list(
    engine = engine,
    extras = list(roles = processed_extra_role_cols)
  )
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

  extras <- forge_recipe_default_clean_extras(engine, new_data)

  out$forge$clean(engine, predictors, outcomes, extras)
}

forge_recipe_default_clean_extras <- function(engine, new_data) {

  if (!is.null(engine$extra_role_ptypes)) {
    extra_role_cols <- map(engine$extra_role_ptypes, shrink, data = new_data)
    extra_role_cols <- map2(extra_role_cols, engine$extra_role_ptypes, scream)
  }
  else {
    extra_role_cols <- NULL
  }

  extras <- list(roles = extra_role_cols)

  extras
}

forge_recipe_default_process <- function(engine, predictors, outcomes, extras) {

  rec <- engine$recipe
  roles <- rec$term_info$role
  vars <- rec$term_info$variable

  # Can't move this inside core functions
  # predictors and outcomes both must be present
  baked_data <- recipes::bake(
    object = rec,
    new_data = vctrs::vec_cbind(predictors, outcomes, !!!unname(extras$roles))
  )

  processed_predictor_names <- vars[strict_equal(roles, "predictor")]
  predictors <- baked_data[, processed_predictor_names, drop = FALSE]

  if (!is.null(outcomes)) {
    processed_outcome_names <- vars[strict_equal(roles, "outcome")]
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

  extras <- forge_recipe_default_process_extras(
    extras,
    rec,
    baked_data,
    predictors_lst$extras,
    outcomes_lst$extras
  )

  out$forge$process(predictors_lst$data, outcomes_lst$data, extras)
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

forge_recipe_default_process_extras <- function(extras, rec, baked_data,
                                                predictors_extras, outcomes_extras) {

  # Remove old roles slot
  extras$roles <- NULL

  extras <- c(
    extras,
    list(roles = get_extra_role_columns(rec, baked_data, original = FALSE)),
    out$extras$final(predictors_extras, outcomes_extras)
  )

  extras
}

# ------------------------------------------------------------------------------

get_original_predictor_ptype <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[strict_equal(roles, "predictor")]
  original_names <- original_names[!is.na(original_names)]

  original_data <- data[, original_names, drop = FALSE]

  extract_ptype(original_data)
}

get_original_outcome_ptype <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[strict_equal(roles, "outcome")]

  original_data <- data[, original_names, drop = FALSE]

  extract_ptype(original_data)
}

get_extra_role_columns <- function(rec, data, original = TRUE) {

  if (original) {
    info_type <- "var_info"
  }
  else {
    info_type <- "term_info"
  }

  roles <- rec[[info_type]][["role"]]
  vars <- rec[[info_type]][["variable"]]

  extra_roles <- setdiff(roles, c("predictor", "outcome", NA_character_))

  has_any_extra_roles <- length(extra_roles) > 0

  if (!has_any_extra_roles) {
    return(NULL)
  }

  extra_role_cols_list <- lapply(extra_roles, function(.role) {
    .role_cols <- vars[strict_equal(roles, .role)]
    data[, .role_cols, drop = FALSE]
  })

  names(extra_role_cols_list) <- extra_roles

  extra_role_cols_list
}

validate_is_0_row_tibble_or_null <- function(.x, .x_nm) {

  if (is.null(.x)) {
    return(invisible(.x))
  }

  validate_is_0_row_tibble(.x, .x_nm)
}
