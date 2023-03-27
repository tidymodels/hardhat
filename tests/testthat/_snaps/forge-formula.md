# can be both missing levels and have new levels

    Code
      mold(y ~ f, dat, blueprint = bp2)
    Condition
      Error in `recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "f".

# `allow_novel_levels` works right with character predictors

    Code
      out <- forge(df2, x$blueprint)
    Condition
      Warning:
      Novel levels found in column 'x': 'd'. The levels have been removed, and values have been coerced to 'NA'.

