predict_{{model}}_response <- function(model, predictors) {
  predictions <- rep(1L, times = nrow(predictors))
  hardhat::spruce_response(predictions)
}
