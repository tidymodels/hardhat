{{model}}_bridge <- function(processed, ...) {
  predictors <- processed$predictors
  outcomes <- processed$outcomes[[1]]

  fit <- {{model}}_impl(predictors, outcomes)

  new_{{model}}_model(
    coefs = fit$coefs,
    blueprint = processed$blueprint
  )
}
