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
    Condition
      Warning:
      The `strings_as_factors` argument of `prep.recipe()` is deprecated as of recipes 1.3.0.
      i Please use the `strings_as_factors` argument of `recipe()` instead.
    Message
      Recipe blueprint:
      # Predictors: 1
      # Outcomes: 1
      Intercept: FALSE
      Novel Levels: FALSE
      Composition: tibble
      

