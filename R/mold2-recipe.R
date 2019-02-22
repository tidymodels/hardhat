mold2.recipe <- function(x, data, intercept = FALSE, engine = NULL, ...) {

  validate_recipes_available()

  if (is.null(engine)) {
    engine <- new_default_recipe_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(
    engine = engine,
    recipe = x,
    intercept = intercept,
    ...
  )

  mold_impl(engine, data)
}

mold_impl.recipe_engine <- function(engine, data, ...) {

  c(engine, data) %<-% engine$mold$clean(engine, data)
  c(engine, predictors, outcomes) %<-% engine$mold$process(engine, data)

  info <- info_lst(predictors = predictors$info, outcomes = outcomes$info)

  engine <- update_engine(engine, info = info)

  mold_list2(
    predictors = predictors$data,
    outcomes = outcomes$data,
    engine = engine,
    offset = predictors$offset
    # extras = extras
  )

}
