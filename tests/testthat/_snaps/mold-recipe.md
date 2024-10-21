# `data` is validated

    Code
      mold(recipes::recipe(Species ~ Sepal.Length, data = iris), 1)
    Condition
      Error in `mold_recipe_default_clean()`:
      ! `data` must be a data frame or a matrix, not the number 1.

