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
#' @return
#'
#' For `default_recipe_blueprint()`, a recipe blueprint.
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
#' @export
#' @examplesIf rlang::is_installed(c("recipes"))
#' # example code
#'
#' library(recipes)
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100, ]
#' test <- iris[101:150, ]
#'
#' # ---------------------------------------------------------------------------
#' # Recipes example
#'
#' # Create a recipe that logs a predictor
#' rec <- recipe(Species ~ Sepal.Length + Sepal.Width, train) %>%
#'   step_log(Sepal.Length)
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
#' # Matrix output for predictors
#'
#' # You can change the `composition` of the predictor data set
#' bp <- default_recipe_blueprint(composition = "dgCMatrix")
#' processed <- mold(rec, train, blueprint = bp)
#' class(processed$predictors)
#'
#' # ---------------------------------------------------------------------------
#' # Non standard roles
#'
#' # If you have custom recipes roles, they are assumed to be required at
#' # `bake()` time when passing in `new_data`. This is an assumption that both
#' # recipes and hardhat makes, meaning that those roles are required at
#' # `forge()` time as well.
#' rec_roles <- recipe(train) %>%
#'   update_role(Sepal.Width, new_role = "predictor") %>%
#'   update_role(Species, new_role = "outcome") %>%
#'   update_role(Sepal.Length, new_role = "id") %>%
#'   update_role(Petal.Length, new_role = "important")
#'
#' processed_roles <- mold(rec_roles, train)
#'
#' # The custom roles will be in the `mold()` result in case you need
#' # them for modeling.
#' processed_roles$extras
#'
#' # And they are in the `forge()` result
#' forge(test, processed_roles$blueprint)$extras
#'
#' # If you remove a column with a custom role from the test data, then you
#' # won't be able to `forge()` even though this recipe technically didn't
#' # use that column in any steps
#' test2 <- test
#' test2$Petal.Length <- NULL
#' try(forge(test2, processed_roles$blueprint))
#'
#' # Most of the time, if you find yourself in the above scenario, then we
#' # suggest that you remove `Petal.Length` from the data that is supplied to
#' # the recipe. If that isn't an option, you can declare that that column
#' # isn't required at `bake()` time by using `update_role_requirements()`
#' rec_roles <- update_role_requirements(rec_roles, "important", bake = FALSE)
#' processed_roles <- mold(rec_roles, train)
#' forge(test2, processed_roles$blueprint)
default_recipe_blueprint <- function(intercept = FALSE,
                                     allow_novel_levels = FALSE,
                                     fresh = TRUE,
                                     strings_as_factors = TRUE,
                                     composition = "tibble") {
  new_default_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    strings_as_factors = strings_as_factors,
    composition = composition
  )
}

#' @param extra_role_ptypes A named list. The names are the unique non-standard
#' recipe roles (i.e. everything except `"predictors"` and `"outcomes"`). The
#' values are prototypes of the original columns with that role. These are
#' used for validation in `forge()`.
#'
#' @rdname new-default-blueprint
#' @export
new_default_recipe_blueprint <- function(intercept = FALSE,
                                         allow_novel_levels = FALSE,
                                         fresh = TRUE,
                                         strings_as_factors = TRUE,
                                         composition = "tibble",
                                         ptypes = NULL,
                                         recipe = NULL,
                                         extra_role_ptypes = NULL,
                                         ...,
                                         subclass = character()) {
  new_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    strings_as_factors = strings_as_factors,
    composition = composition,
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

#' @rdname run-mold
#' @export
run_mold.default_recipe_blueprint <- function(blueprint, ..., data) {
  check_dots_empty0(...)

  cleaned <- mold_recipe_default_clean(blueprint = blueprint, data = data)

  blueprint <- cleaned$blueprint
  data <- cleaned$data

  mold_recipe_default_process(blueprint = blueprint, data = data)
}

# ------------------------------------------------------------------------------
# mold - recipe - clean

mold_recipe_default_clean <- function(blueprint, data) {
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  new_mold_clean(blueprint, data)
}

# ------------------------------------------------------------------------------
# mold - recipe - process

mold_recipe_default_process <- function(blueprint, data) {

  # `prep()` will warn if you pass `training` data and `fresh = FALSE`
  if (is_true(blueprint$fresh)) {
    training <- data
  } else {
    training <- NULL
  }

  # Prep for predictors and outcomes
  recipe <- recipes::prep(
    blueprint$recipe,
    training = training,
    fresh = blueprint$fresh,
    strings_as_factors = blueprint_strings_as_factors(blueprint)
  )
  blueprint <- update_blueprint0(blueprint, recipe = recipe)

  processed <- mold_recipe_default_process_predictors(blueprint = blueprint, data = data)

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_ptype <- processed$ptype
  predictors_extras <- processed$extras

  processed <- mold_recipe_default_process_outcomes(blueprint = blueprint, data = data)

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_ptype <- processed$ptype
  outcomes_extras <- processed$extras

  processed <- mold_recipe_default_process_extras(blueprint, data)

  blueprint <- processed$blueprint
  extras <- processed$extras

  extras <- c(
    extras,
    new_extras(predictors_extras, outcomes_extras)
  )

  # un-retain training data
  blueprint <- update_blueprint0(blueprint, recipe = compost(blueprint$recipe))

  ptypes <- new_ptypes(predictors_ptype, outcomes_ptype)

  blueprint <- update_blueprint0(blueprint, ptypes = ptypes)

  new_mold_process(predictors, outcomes, blueprint, extras)
}

mold_recipe_default_process_predictors <- function(blueprint, data) {
  all_predictors <- recipes::all_predictors

  predictors <- recipes::juice(blueprint$recipe, all_predictors())

  predictors <- maybe_add_intercept_column(predictors, blueprint$intercept)

  predictors <- recompose(predictors, composition = blueprint$composition)

  ptype <- get_original_predictor_ptype(blueprint$recipe, data)

  new_mold_process_terms(
    blueprint = blueprint,
    data = predictors,
    ptype = ptype
  )
}

mold_recipe_default_process_outcomes <- function(blueprint, data) {
  all_outcomes <- recipes::all_outcomes

  outcomes <- recipes::juice(blueprint$recipe, all_outcomes())

  ptype <- get_original_outcome_ptype(blueprint$recipe, data)

  new_mold_process_terms(
    blueprint = blueprint,
    data = outcomes,
    ptype = ptype
  )
}

mold_recipe_default_process_extras <- function(blueprint, data) {

  # Capture original non standard role columns that exist in `data` and are also
  # required by the `recipe$requirements$bake` requirement. These columns are
  # also required in `new_data` at `bake()` time.
  original_extra_role_cols <- get_extra_role_columns_original(
    blueprint$recipe,
    data
  )

  if (!is.null(original_extra_role_cols)) {
    original_extra_role_ptypes <- lapply(original_extra_role_cols, extract_ptype)

    blueprint <- update_blueprint0(
      blueprint,
      extra_role_ptypes = original_extra_role_ptypes
    )
  }

  # Return all of the processed non standard role columns.
  # These might be generated by `prep()` and could differ from the ones in the
  # original data.
  # These are not required in `new_data`, but we return them assuming the
  # developer may need them for model fitting purposes.
  processed_extra_role_cols <- get_extra_role_columns_processed(
    blueprint$recipe,
    recipes::juice(blueprint$recipe)
  )

  list(
    blueprint = blueprint,
    extras = list(roles = processed_extra_role_cols)
  )
}

# ------------------------------------------------------------------------------

#' @rdname run-forge
#' @export
run_forge.default_recipe_blueprint <- function(blueprint,
                                               new_data,
                                               ...,
                                               outcomes = FALSE,
                                               call = caller_env()
                                              ) {
  check_dots_empty0(...)

  cleaned <- forge_recipe_default_clean(
    blueprint = blueprint,
    new_data = new_data,
    outcomes = outcomes, 
    call = call
  )

  blueprint <- cleaned$blueprint
  predictors <- cleaned$predictors
  outcomes <- cleaned$outcomes
  extras <- cleaned$extras

  forge_recipe_default_process(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

# ------------------------------------------------------------------------------

forge_recipe_default_clean <- function(blueprint, new_data, outcomes, ..., call = caller_env()) {
  check_dots_empty0(...)
  check_data_frame_or_matrix(new_data)
  new_data <- coerce_to_tibble(new_data)
  check_unique_column_names(new_data)
  check_bool(outcomes)

  predictors <- shrink(new_data, blueprint$ptypes$predictors, call = call)

  predictors <- scream(
    predictors,
    blueprint$ptypes$predictors,
    allow_novel_levels = blueprint$allow_novel_levels
  )

  if (outcomes) {
    outcomes <- shrink(new_data, blueprint$ptypes$outcomes, call = call)
    # Never allow novel levels for outcomes
    outcomes <- scream(outcomes, blueprint$ptypes$outcomes)
  } else {
    outcomes <- NULL
  }

  extras <- forge_recipe_default_clean_extras(blueprint, new_data, call = call)

  new_forge_clean(blueprint, predictors, outcomes, extras)
}

forge_recipe_default_clean_extras <- function(blueprint, new_data, ..., call = caller_env()) {
  check_dots_empty0(...)
  if (is.null(blueprint$extra_role_ptypes)) {
    extras <- list(roles = NULL)
    return(extras)
  }

  extra_role_cols <- map(
    blueprint$extra_role_ptypes,
    shrink,
    data = new_data,
    call = call
  )

  extra_role_cols <- map2(
    extra_role_cols,
    blueprint$extra_role_ptypes,
    scream,
    allow_novel_levels = blueprint$allow_novel_levels
  )

  extras <- list(roles = extra_role_cols)

  extras
}

# ------------------------------------------------------------------------------

forge_recipe_default_process <- function(blueprint, predictors, outcomes, extras) {
  rec <- blueprint$recipe
  vars <- rec$term_info$variable
  roles <- rec$term_info$role
  roles <- chr_explicit_na(roles)

  # Minimal name repair in case a predictor has multiple roles
  # We just want to include it once, but without any name repair
  new_data <- vec_cbind(
    predictors,
    outcomes,
    !!!unname(extras$roles),
    .name_repair = "minimal"
  )

  new_data_names <- names(new_data)
  unique_names <- unique(new_data_names)

  new_data <- new_data[unique_names]

  # Can't move this inside core functions
  # predictors and outcomes both must be present
  baked_data <- recipes::bake(
    object = rec,
    new_data = new_data
  )

  processed_predictor_names <- vars[roles == "predictor"]
  predictors <- baked_data[processed_predictor_names]

  if (!is.null(outcomes)) {
    processed_outcome_names <- vars[roles == "outcome"]
    outcomes <- baked_data[processed_outcome_names]
  }

  processed <- forge_recipe_default_process_predictors(
    blueprint = blueprint,
    predictors = predictors
  )

  blueprint <- processed$blueprint
  predictors <- processed$data
  predictors_extras <- processed$extras

  processed <- forge_recipe_default_process_outcomes(
    blueprint = blueprint,
    outcomes = outcomes
  )

  blueprint <- processed$blueprint
  outcomes <- processed$data
  outcomes_extras <- processed$extras

  extras <- forge_recipe_default_process_extras(
    extras,
    rec,
    baked_data,
    predictors_extras,
    outcomes_extras
  )

  new_forge_process(predictors, outcomes, extras)
}

forge_recipe_default_process_predictors <- function(blueprint, predictors) {
  predictors <- maybe_add_intercept_column(predictors, blueprint$intercept)

  predictors <- recompose(predictors, composition = blueprint$composition)

  new_forge_process_terms(
    blueprint = blueprint,
    data = predictors
  )
}

forge_recipe_default_process_outcomes <- function(blueprint, outcomes) {

  # no outcomes to process
  if (is.null(outcomes)) {
    result <- new_forge_process_terms(
      blueprint = blueprint,
      data = outcomes
    )
    return(result)
  }

  new_forge_process_terms(
    blueprint = blueprint,
    data = outcomes
  )
}

forge_recipe_default_process_extras <- function(extras,
                                                rec,
                                                baked_data,
                                                predictors_extras,
                                                outcomes_extras) {

  # Remove old roles slot
  extras$roles <- NULL

  # Get the processed extra role columns after `bake()` has been called.
  processed_extra_role_cols <- get_extra_role_columns_processed(
    rec,
    baked_data
  )

  extras <- c(
    extras,
    list(roles = processed_extra_role_cols),
    new_extras(predictors_extras, outcomes_extras)
  )

  extras
}

# ------------------------------------------------------------------------------

get_original_predictor_ptype <- function(rec, data) {
  roles <- rec$var_info$role
  roles <- chr_explicit_na(roles)

  original_names <- rec$var_info$variable[roles == "predictor"]
  original_names <- original_names[!is.na(original_names)]

  data <- data[original_names]

  extract_ptype(data)
}

get_original_outcome_ptype <- function(rec, data) {
  roles <- rec$var_info$role
  roles <- chr_explicit_na(roles)

  original_names <- rec$var_info$variable[roles == "outcome"]

  data <- data[original_names]

  extract_ptype(data)
}

get_extra_role_columns_original <- function(rec, data) {
  # Extra roles that existed before `prep()` has been called.
  # To get "extra" roles that are required at bake time:
  # - Compute the bake role requirements named logical vector.
  #   It has information about every role in the original data.
  # - Subset that vector to only `TRUE` locations, where the role is required
  # - Remove the `"predictor"` role (it is always required, but isn't "extra")
  info_type <- "var_info"

  requirements <- compute_bake_role_requirements(rec)

  # Filter down to the roles that are actually required
  requirements <- requirements[requirements]
  requirement_roles <- names(requirements)

  extra_roles <- setdiff(requirement_roles, "predictor")

  get_extra_role_columns(rec, data, extra_roles, info_type)
}
get_extra_role_columns_processed <- function(rec, data) {
  # Extra roles that exist after baking either the training data or testing
  # data (i.e. through `prep()` or `bake()`). This might include more or less
  # roles than in the original data, because steps may have created or removed
  # them along the way.
  info_type <- "term_info"

  data_roles <- rec[[info_type]][["role"]]
  data_roles <- chr_explicit_na(data_roles)

  extra_roles <- setdiff(data_roles, c("outcome", "predictor"))

  get_extra_role_columns(rec, data, extra_roles, info_type)
}

get_extra_role_columns <- function(rec, data, extra_roles, info_type) {
  has_any_extra_roles <- length(extra_roles) > 0

  if (!has_any_extra_roles) {
    return(NULL)
  }

  data_names <- colnames(data)

  recipe_names <- rec[[info_type]][["variable"]]
  recipe_roles <- rec[[info_type]][["role"]]
  recipe_roles <- chr_explicit_na(recipe_roles)

  out <- lapply(extra_roles, function(role) {
    role_names <- recipe_names[recipe_roles == role]

    # Must restrict to names that are actually in the `data` in case some
    # roles were declared as "not required" and columns with those roles weren't
    # passed to `forge()` through `new_data`.
    role_names <- intersect(role_names, data_names)

    data[role_names]
  })

  names(out) <- extra_roles

  out
}

# ------------------------------------------------------------------------------

new_role_requirements <- function() {
  # recipes:::new_role_requirements()
  list(
    bake = new_bake_role_requirements()
  )
}
get_role_requirements <- function(recipe) {
  # recipes:::get_role_requirements()
  recipe$requirements %||% new_role_requirements()
}

new_bake_role_requirements <- function() {
  # recipes:::new_bake_role_requirements()
  set_names(logical(), nms = character())
}
get_bake_role_requirements <- function(recipe) {
  # recipes:::get_bake_role_requirements()
  requirements <- get_role_requirements(recipe)
  requirements$bake
}
default_bake_role_requirements <- function() {
  # recipes:::default_bake_role_requirements()
  c(
    "outcome" = FALSE,
    "predictor" = TRUE,
    "case_weights" = FALSE,
    "NA" = TRUE
  )
}
compute_bake_role_requirements <- function(recipe) {
  # recipes:::compute_bake_role_requirements()
  var_info <- recipe$var_info
  var_roles <- var_info$role
  var_roles <- chr_explicit_na(var_roles)
  var_roles <- unique(var_roles)

  # Start with default requirements
  requirements <- default_bake_role_requirements()

  # Drop unused default requirements
  requirements <- requirements[names(requirements) %in% var_roles]

  # Update with nonstandard roles in the recipe, which are required by default
  nonstandard_roles <- var_roles[!var_roles %in% names(requirements)]
  requirements[nonstandard_roles] <- TRUE

  # Override with `update_role_requirements()` changes
  user_requirements <- get_bake_role_requirements(recipe)
  requirements[names(user_requirements)] <- user_requirements

  requirements
}

chr_explicit_na <- function(x) {
  # recipes:::chr_explicit_na()
  # To turn `NA_character_` into `"NA"` because you can't match
  # against `NA_character_` when assigning with `[<-`
  x[is.na(x)] <- "NA"
  x
}
