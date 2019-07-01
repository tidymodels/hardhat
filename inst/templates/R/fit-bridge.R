{{model}}_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  fit <- {{model}}_impl(predictors, outcome)

  new_{{model}}(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}
