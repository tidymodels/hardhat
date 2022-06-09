# These are standardized constructors for internal objects returned from
# different blueprint handlers

# ------------------------------------------------------------------------------
# Mold

new_mold_clean <- function(blueprint, data) {
  list(
    blueprint = blueprint,
    data = data
  )
}

new_mold_clean_xy <- function(blueprint, x, y) {
  list(
    blueprint = blueprint,
    x = x,
    y = y
  )
}

new_mold_process <- function(predictors, outcomes, blueprint, extras) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    blueprint = blueprint,
    extras = extras
  )
}

new_mold_process_terms <- function(blueprint,
                                   data,
                                   ptype,
                                   extras = NULL) {
  list(
    blueprint = blueprint,
    data = data,
    ptype = ptype,
    extras = extras
  )
}

# ------------------------------------------------------------------------------
# Forge

new_forge_clean <- function(blueprint, predictors, outcomes, extras = NULL) {
  list(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

new_forge_process <- function(predictors, outcomes, extras) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

new_forge_process_terms <- function(blueprint,
                                    data,
                                    extras = NULL) {
  list(
    blueprint = blueprint,
    data = data,
    extras = extras
  )
}

# ------------------------------------------------------------------------------
# ptypes

new_ptypes <- function(predictors, outcomes) {
  list(
    predictors = predictors,
    outcomes = outcomes
  )
}

# ------------------------------------------------------------------------------
# Extras

# Just c() them together
# Extras aren't predictor or outcome specific
new_extras <- function(predictors_extras, outcomes_extras) {
  c(predictors_extras, outcomes_extras)
}
