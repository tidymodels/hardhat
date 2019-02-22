forge2.data.frame <- function(new_data, engine, outcomes = FALSE, ...) {

  engine <- update_engine(engine, ...)

  forge_impl(engine, new_data, outcomes)
}

forge2.matrix <- forge2.data.frame

forge_impl.xy_engine <- function(engine, new_data, outcomes) {

  c(engine, new_data) %<-% engine$forge$clean(engine, new_data, outcomes)
  c(engine, predictors, outcomes) %<-% engine$forge$process(engine, new_data, outcomes)

  forge_list2(
    predictors = predictors$data,
    outcomes = outcomes$data,
    offset = predictors$offset
    # extras = extras # maybe this would be useful?
  )
}
