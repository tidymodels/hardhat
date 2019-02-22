forge_impl.recipe_engine <- function(engine, new_data, outcomes) {

  validate_recipes_available()

  c(engine, new_data) %<-% engine$forge$clean(engine, new_data, outcomes)
  c(engine, predictors, outcomes) %<-% engine$forge$process(engine, new_data, outcomes)

  forge_list2(
    predictors = predictors$data,
    outcomes = outcomes$data,
    offset = predictors$offset
    # extras = extras # maybe this would be useful?
  )
}
