mold2.data.frame <- function(x, y, intercept = FALSE, engine = NULL, ...) {

  if (is.null(engine)) {
    engine <- new_default_xy_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(engine, intercept = intercept, ...)

  mold_impl(engine, x, y)
}

mold2.matrix <- mold2.data.frame

# not exposed
# all xy engines call the same things?
mold_impl.xy_engine <- function(engine, x, y, ...) {

  c(engine, x, y) %<-% engine$mold$clean(engine, x, y)
  c(engine, predictors, outcomes) %<-% engine$mold$process(engine, x, y)

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
