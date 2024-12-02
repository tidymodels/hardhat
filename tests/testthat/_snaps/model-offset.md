# Only numeric columns can be offsets

    Code
      mold(~ Sepal.Width + offset(Species), iris)
    Condition
      Error in `model_offset()`:
      ! Column "offset(Species)" is tagged as an offset and thus must be numeric, not a <factor> object.

# offset columns are stored as predictors

    Code
      forge(iris2, x$blueprint)
    Condition
      Error in `forge()`:
      ! The required column "Sepal.Length" is missing.

