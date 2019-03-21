# Standardized helpers for output lists

# ------------------------------------------------------------------------------
# Mold

out_mold <- function(predictors, outcomes, blueprint, extras) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    blueprint = blueprint,
    extras = extras
  )
}

out_mold_clean <- function(blueprint, data) {
  list(
    blueprint = blueprint,
    data = data
  )
}

out_mold_clean_xy <- function(blueprint, x, y) {
  list(
    blueprint = blueprint,
    x = x,
    y = y
  )
}

out_mold_process <- function(blueprint, predictors, outcomes, ptypes, extras) {
  list(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    ptypes = ptypes,
    extras = extras
  )
}

out_mold_process_terms <- function(blueprint, terms_lst) {
  list(
    blueprint = blueprint,
    terms_lst = terms_lst
  )
}

out_mold_process_terms_lst <- function(data, ptype, extras = NULL) {
  list(
    data = data,
    ptype = ptype,
    extras = extras
  )
}

# ------------------------------------------------------------------------------
# Forge

out_forge <- function(predictors, outcomes, extras) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

out_forge_process <- function(predictors, outcomes, extras) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

out_forge_process_terms <- function(blueprint, terms_lst) {
  list(
    blueprint = blueprint,
    terms_lst = terms_lst
  )
}

# data can be NULL here for outcomes = FALSE
out_forge_process_terms_lst <- function(data = NULL, extras = NULL) {
  list(
    data = data,
    extras = extras
  )
}

out_forge_clean <- function(blueprint, predictors, outcomes, extras = NULL) {
  list(
    blueprint = blueprint,
    predictors = predictors,
    outcomes = outcomes,
    extras = extras
  )
}

# ------------------------------------------------------------------------------
# ptypes

out_ptype_lst <- function(predictors, outcomes) {
  list(
    predictors = predictors,
    outcomes = outcomes
  )
}

# ------------------------------------------------------------------------------
# Extras

# Just c() them together
# Extras aren't predictor or outcome specific
out_extras_lst <- function(predictors_extras, outcomes_extras) {
  c(predictors_extras, outcomes_extras)
}

# ------------------------------------------------------------------------------
# Output list helper

# This must be at the end (after all functions have been defined)
# since it calls the functions from inside a
# list and not inside another function

out <- list(
  mold = list(
    final = out_mold,
    clean = out_mold_clean,
    clean_xy = out_mold_clean_xy,
    process = out_mold_process,
    process_terms = out_mold_process_terms,
    process_terms_lst = out_mold_process_terms_lst
  ),
  forge = list(
    final = out_forge,
    clean = out_forge_clean,
    process = out_forge_process,
    process_terms = out_forge_process_terms,
    process_terms_lst = out_forge_process_terms_lst
  ),
  ptypes = list(
    final = out_ptype_lst
  ),
  extras = list(
    final = out_extras_lst
  )
)
