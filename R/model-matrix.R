model_matrix <- function(terms, data) {

  if (!inherits(terms, "terms")) {
    glubort("`terms` must be a 'terms' object.")
  }

  if (!is.data.frame(data)) {
    glubort("`data` must be a data frame.")
  }

  # otherwise model.matrix() will try and run model.frame() for us on data
  # but we definitely don't want this, as we have already done it and it can
  # actually error out
  attr(data, "terms") <- terms

  predictors <- rlang::with_options(
    model.matrix(object = terms, data = data),
    na.action = "na.pass"
  )

  predictors <- strip_model_matrix(predictors)

  tibble::as_tibble(predictors)
}
