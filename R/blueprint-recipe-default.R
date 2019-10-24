#' Default recipe blueprint
#'
#' This pages holds the details for the recipe preprocessing blueprint. This
#' is the blueprint used by default from `mold()` if `x` is a recipe.
#'
#' @inheritParams new_recipe_blueprint
#'
#' @param x An unprepped recipe created from [recipes::recipe()].
#'
#' @param data A data frame or matrix containing the outcomes and predictors.
#'
#' @param blueprint A preprocessing `blueprint`. If left as `NULL`, then a
#' [default_recipe_blueprint()] is used.
#'
#' @param ... Not used.
#'
#' @section Mold:
#'
#' When `mold()` is used with the default recipe blueprint:
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
#' When `forge()` is used with the default recipe blueprint:
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
#' # The underlying blueprint is a prepped recipe
#' processed$blueprint$recipe
#'
#' # Call forge() with the blueprint and the test data
#' # to have it preprocess the test data in the same way
#' forge(test, processed$blueprint)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome!
#' # This logged the Sepal.Length column of `new_data`
#' forge(test, processed$blueprint, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # With an intercept
#'
#' # You can add an intercept with `intercept = TRUE`
#' processed <- mold(rec, train, blueprint = default_recipe_blueprint(intercept = TRUE))
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
#' forge(test, processed_roles$blueprint)
#'
#' @export
default_recipe_blueprint <- function(intercept = FALSE, fresh = FALSE) {

  mold <- get_mold_recipe_default_function_set()
  forge <- get_forge_recipe_default_function_set()

  new_default_recipe_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    fresh = fresh
  )

}

#' @param extra_role_ptypes A named list. The names are the unique non-standard
#' recipe roles (i.e. everything except `"predictors"` and `"outcomes"`). The
#' values are prototypes of the original columns with that role. These are
#' used for validation in `forge()`.
#'
#' @rdname new-default-blueprint
#' @export
new_default_recipe_blueprint <- function(mold,
                                         forge,
                                         intercept = FALSE,
                                         fresh = FALSE,
                                         ptypes = NULL,
                                         recipe = NULL,
                                         extra_role_ptypes = NULL,
                                         ...,
                                         subclass = character()) {

  new_recipe_blueprint(
    mold = mold,
    forge = forge,
    intercept = intercept,
    fresh = fresh,
    ptypes = ptypes,
    recipe = recipe,
    extra_role_ptypes = extra_role_ptypes,
    ...,
    subclass = c(subclass, "default_recipe_blueprint")
  )

}

#' @export
refresh_blueprint.default_recipe_blueprint <- function(blueprint) {
  do.call(new_default_recipe_blueprint, as.list(blueprint))
}

# ------------------------------------------------------------------------------

get_mold_recipe_default_function_set <- function() {
  blueprint_function_set(mold_recipe_default_clean, mold_recipe_default_process)
}

# mold - recipe - clean
mold_recipe_default_clean <- function(blueprint, data) {

  data <- check_is_data_like(data)

  out$mold$clean(blueprint, data)
}

# mold - recipe - process
mold_recipe_default_process <- function(blueprint, data) {

  # Prep for predictors and outcomes
  recipe <- recipes::prep(blueprint$recipe, training = data, fresh = blueprint$fresh)
  blueprint <- update_blueprint(blueprint, recipe = recipe)

  c(blueprint, predictors_lst) %<-% mold_recipe_default_process_predictors(
    blueprint = blueprint,
    data = data
  )

  c(blueprint, outcomes_lst) %<-% mold_recipe_default_process_outcomes(
    blueprint = blueprint,
    data = data
  )

  c(blueprint, extras) %<-% mold_recipe_default_process_extras(blueprint, data)

  extras <- c(
    extras,
    out$extras$final(predictors_lst$extras, outcomes_lst$extras)
  )

  # un-retain training data
  blueprint <- update_blueprint(blueprint, recipe = compost(blueprint$recipe))

  ptypes <- out$ptypes$final(predictors_lst$ptype, outcomes_lst$ptype)

  out$mold$process(blueprint, predictors_lst$data, outcomes_lst$data, ptypes, extras)
}

mold_recipe_default_process_predictors <- function(blueprint, data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::juice(blueprint$recipe, all_predictors())

  predictors <- maybe_add_intercept_column(predictors, blueprint$intercept)

  ptype <- get_original_predictor_ptype(blueprint$recipe, data)

  predictors_lst <- out$mold$process_terms_lst(data = predictors, ptype)

  out$mold$process_terms(blueprint, predictors_lst)
}

mold_recipe_default_process_outcomes <- function(blueprint, data) {

  all_outcomes <- recipes::all_outcomes

  outcomes <- recipes::juice(blueprint$recipe, all_outcomes())

  ptype <- get_original_outcome_ptype(blueprint$recipe, data)

  outcomes_lst <- out$mold$process_terms_lst(data = outcomes, ptype)

  out$mold$process_terms(blueprint, outcomes_lst)
}

mold_recipe_default_process_extras <- function(blueprint, data) {

  # Capture original non standard role columns (anything but "predictor"
  # or "outcome"). These are required in `new_data`
  original_extra_role_cols <- get_extra_role_columns(
    blueprint$recipe,
    data
  )

  if (!is.null(original_extra_role_cols)) {
    blueprint <- update_blueprint(
      blueprint,
      extra_role_ptypes = lapply(original_extra_role_cols, extract_ptype)
    )
  }

  # Return the processed non standard role columns
  # (could be generated by prep(), not required in `new_data`)
  processed_extra_role_cols <- get_extra_role_columns(
    blueprint$recipe,
    recipes::juice(blueprint$recipe),
    original = FALSE
  )

  list(
    blueprint = blueprint,
    extras = list(roles = processed_extra_role_cols)
  )
}

# ------------------------------------------------------------------------------

get_forge_recipe_default_function_set <- function() {
  blueprint_function_set(forge_recipe_default_clean, forge_recipe_default_process)
}

forge_recipe_default_clean <- function(blueprint, new_data, outcomes) {

  validate_is_new_data_like(new_data)
  validate_has_unique_column_names(new_data, "new_data")
  validate_is_bool(outcomes)

  predictors <- shrink(new_data, blueprint$ptypes$predictors)
  predictors <- scream(predictors, blueprint$ptypes$predictors)

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes)
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes)
  }
  else {
    outcomes <- NULL
  }

  extras <- forge_recipe_default_clean_extras(blueprint, new_data)

  out$forge$clean(blueprint, predictors, outcomes, extras)
}

forge_recipe_default_clean_extras <- function(blueprint, new_data) {

  if (!is.null(blueprint$extra_role_ptypes)) {
    extra_role_cols <- map(blueprint$extra_role_ptypes, shrink, data = new_data)
    extra_role_cols <- map2(extra_role_cols, blueprint$extra_role_ptypes, scream)
  }
  else {
    extra_role_cols <- NULL
  }

  extras <- list(roles = extra_role_cols)

  extras
}

forge_recipe_default_process <- function(blueprint, predictors, outcomes, extras) {

  rec <- blueprint$recipe
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

  c(blueprint, predictors_lst) %<-% forge_recipe_default_process_predictors(
    blueprint = blueprint,
    predictors = predictors
  )

  c(blueprint, outcomes_lst) %<-% forge_recipe_default_process_outcomes(
    blueprint = blueprint,
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

forge_recipe_default_process_predictors <- function(blueprint, predictors) {

  predictors <- maybe_add_intercept_column(predictors, blueprint$intercept)

  predictors_lst <- out$forge$process_terms_lst(data = predictors)

  out$forge$process_terms(blueprint, predictors_lst)
}

forge_recipe_default_process_outcomes <- function(blueprint, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    outcomes_lst <- out$forge$process_terms_lst()
    result <- out$forge$process_terms(blueprint, outcomes_lst)
    return(result)
  }

  outcomes_lst <- out$forge$process_terms_lst(data = outcomes)

  out$forge$process_terms(blueprint, outcomes_lst)
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
