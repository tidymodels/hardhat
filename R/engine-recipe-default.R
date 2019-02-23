#' Create a new default recipe engine
#'
#' This is the constructor for a default recipe preprocessing engine. This
#' is the engine used by default from `mold()` if `x` is a recipe. To learn
#' about what the default mold and forge functionality are, see the
#' Mold and Forge sections below.
#'
#' @section Mold:
#'
#' The recipe engine's mold function does the following:
#'
#' - Calls [recipes::prep()] to prep the recipe.
#'
#' - Calls [recipes::juice()] to extract the outcomes and predictors. These
#' are returned as tibbles.
#'
#' - If `intercept = TRUE`, adds an intercept column to the predictors.
#'
#' @section Forge:
#'
#' The recipe engine's forge function does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Calls [recipes::bake()] on the `new_data` using the prepped recipe
#' used during training.
#'
#' - Potentially adds an intercept column onto `new_data`,
#' if `intercept = TRUE`.
#'
#' @return
#'
#' A preprocessing engine with the class, `"default_recipe_engine"` that can
#' be used with [mold()] and [forge()].
#'
#' @inheritParams new_recipe_engine
#'
#' @examples
#'
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
#' processed <- mold(rec, train, intercept = TRUE)
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

#' @rdname default_recipe_engine
#' @export
new_default_recipe_engine <- function(mold,
                                      forge,
                                      intercept = FALSE,
                                      info = NULL,
                                      recipe = NULL,
                                      ...,
                                      subclass = character()) {

  new_recipe_engine(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info,
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

  list(
    engine = engine,
    data = data
  )
}

# mold - recipe - process
mold_recipe_default_process <- function(engine, data) {

  # Prep for predictors and outcomes
  recipe <- recipes::prep(engine$recipe, training = data)
  engine <- update_engine(engine, recipe = recipe)

  c(engine, predictors) %<-% mold_recipe_default_process_predictors(engine, data)
  c(engine, outcomes) %<-% mold_recipe_default_process_outcomes(engine, data)

  # un-retain training data
  recipe <- compost(engine$recipe)
  engine <- update_engine(engine, recipe = recipe)

  list(
    engine = engine,
    predictors = predictors,
    outcomes = outcomes
  )

}

mold_recipe_default_process_predictors <- function(engine, data) {

  all_predictors <- recipes::all_predictors

  predictors <- recipes::juice(engine$recipe, all_predictors())

  predictors <- maybe_add_intercept_column(predictors, engine$intercept)

  info <- get_original_predictor_info(engine$recipe, data)

  list(
    data = predictors,
    info = info
  )

  list(
    engine = engine,
    predictors = list(
      data = predictors,
      info = info,
      offset = NULL
    )
  )

}

mold_recipe_default_process_outcomes <- function(engine, data) {

  all_outcomes <- recipes::all_outcomes

  outcomes <- recipes::juice(engine$recipe, all_outcomes())

  info <- get_original_outcome_info(engine$recipe, data)

  list(
    engine = engine,
    outcomes = list(
      data = outcomes,
      info = info
    )
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

  new_data <- shrink(new_data, engine, outcomes)
  new_data <- scream(new_data, engine, outcomes)

  list(
    engine = engine,
    new_data = new_data
  )

}

forge_recipe_default_process <- function(engine, new_data, outcomes) {

  # Can't move this inside core functions, would be double baking
  new_data <- recipes::bake(
    object = engine$recipe,
    new_data = new_data
  )

  c(engine, .predictors) %<-% forge_recipe_default_process_predictors(engine, new_data)
  c(engine, .outcomes) %<-% forge_recipe_default_process_outcomes(engine, new_data, outcomes)

  list(
    engine = engine,
    predictors = .predictors,
    outcomes = .outcomes
  )
}

forge_recipe_default_process_predictors <- function(engine, new_data) {

  recipe <- engine$recipe
  roles <- recipe$term_info$role
  processed_names <- recipe$term_info$variable[roles == "predictor"]

  .predictors <- new_data[, processed_names, drop = FALSE]

  .predictors <- maybe_add_intercept_column(.predictors, engine$intercept)

  list(
    engine = engine,
    predictors = list(
      data = .predictors,
      offset = NULL
    )
  )

}

forge_recipe_default_process_outcomes <- function(engine, new_data, outcomes) {

  if (!outcomes) {

    out <- list(
      engine = engine,
      outcomes = list(
        data = NULL
      )
    )

    return(out)
  }

  recipe <- engine$recipe
  roles <- recipe$term_info$role
  processed_names <- recipe$term_info$variable[roles == "outcome"]

  .outcomes <- new_data[, processed_names, drop = FALSE]

  list(
    engine = engine,
    outcomes = list(
      data = .outcomes
    )
  )

}

# ------------------------------------------------------------------------------

get_original_predictor_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "predictor"]

  original_data <- data[, original_names, drop = FALSE]

  predictors_info(
    names = original_names,
    classes = get_data_classes(original_data),
    levels = get_levels(original_data)
  )

}

get_original_outcome_info <- function(rec, data) {

  roles <- rec$var_info$role
  original_names <- rec$var_info$variable[roles == "outcome"]

  original_data <- data[, original_names, drop = FALSE]

  outcomes_info(
    names = original_names,
    classes = get_data_classes(original_data),
    levels = get_levels(original_data)
  )

}
