mold2.formula <- function(formula, data, intercept = FALSE,
                          indicators = TRUE, engine = NULL, ...) {

  if (is.null(engine)) {
    engine <- new_default_formula_engine()
  }

  # validate_engine(engine)
  engine <- update_engine(
    engine = engine,
    formula = formula,
    intercept = intercept,
    indicators = indicators,
    ...
  )

  mold_impl(engine, data)
}

mold_impl.formula_engine <- function(engine, data, ...) {

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
