# print - formula

    Code
      mold(Species ~ Sepal.Length, iris)$blueprint
    Message
      Formula blueprint:
      # Predictors: 1
      # Outcomes: 1
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: tibble
      Indicators: traditional
      
    Code
      mold(~Sepal.Length, iris)$blueprint
    Message
      Formula blueprint:
      # Predictors: 1
      # Outcomes: 0
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: tibble
      Indicators: traditional
      

# print - default

    Code
      mold(iris[, c("Sepal.Length"), drop = FALSE], iris$Species)$blueprint
    Message
      XY blueprint:
      # Predictors: 1
      # Outcomes: 1
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: tibble
      

# print - recipe

    Code
      mold(recipes::recipe(Species ~ Sepal.Length, iris), iris)$blueprint
    Message
      Recipe blueprint:
      # Predictors: 1
      # Outcomes: 1
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: tibble
      

