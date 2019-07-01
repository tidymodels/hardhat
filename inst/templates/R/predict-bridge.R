predict_{{model}}_bridge <- function(type, model, predictors) {
  predictors <- as.matrix(predictors)

  predict_function <- get_predict_function(type)
  predictions <- predict_function(model, predictors)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_{{model}}_numeric
  )
}

