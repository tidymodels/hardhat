# errors on missing values

    Code
      fct_encode_one_hot(x)
    Condition
      Error in `fct_encode_one_hot()`:
      ! `x` can't contain missing values.

# errors on non-factors

    Code
      fct_encode_one_hot(1)
    Condition
      Error in `fct_encode_one_hot()`:
      ! `x` must be a factor.

