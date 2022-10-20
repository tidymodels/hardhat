# factor_to_indicators works as expected

    Code
      factor_to_indicators(letters)
    Condition
      Error in `factor_to_indicators()`:
      ! `x` must be a factor.

# factor_to_indicators errors for missing values

    Code
      factor_to_indicators(fact)
    Condition
      Error in `factor_to_indicators()`:
      ! `x` must not have any missing values.

