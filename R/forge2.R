forge2 <- function(new_data, engine, outcomes = FALSE, ...) {
  UseMethod("forge2")
}

forge2.default <- function(new_data, engine, outcomes = FALSE, ...) {
  glubort("unknown!")
}

forge_impl <- function(engine, ...) {
  UseMethod("forge_impl")
}

forge_list2 <- function(predictors, outcomes = NULL, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    offset = offset
  )
}
