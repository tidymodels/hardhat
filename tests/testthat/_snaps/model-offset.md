# Only numeric columns can be offsets

    Code
      mold(~ Sepal.Width + offset(Species), iris)
    Condition
      Error in `model_offset()`:
      ! Column, 'offset(Species)', is tagged as an offset, but is not numeric. All offsets must be numeric.

# offset columns are stored as predictors

    Code
      forge(iris2, x$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The required column `Sepal.Length` is missing.

