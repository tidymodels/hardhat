mold2 <- function(x, ...) {
  UseMethod("mold2")
}

mold_impl <- function(engine, ...) {
  UseMethod("mold_impl")
}

mold_list2 <- function(predictors, outcomes, engine, offset = NULL) {
  list(
    predictors = predictors,
    outcomes = outcomes,
    engine = engine,
    offset = offset
  )
}
