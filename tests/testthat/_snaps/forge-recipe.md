# `forge()` will error if required non standard roles are missing

    Code
      forge(iris, x$blueprint)
    Condition
      Error in `glubort()`:
      ! The following required columns are missing: 'Sepal.Width'.

# `NA` roles are treated as extra roles that are required at `forge()` time

    Code
      forge(iris, x$blueprint)
    Condition
      Error in `glubort()`:
      ! The following required columns are missing: 'Petal.Length'.

# `forge()` is compatible with hardhat 0.2.0 molded blueprints with a basic recipe

    Code
      forge(new_data, blueprint)
    Condition
      Error in `glubort()`:
      ! The following required columns are missing: 'x'.

# `forge()` is compatible with hardhat 0.2.0 molded blueprints with a recipe with a nonstandard role

    Code
      forge(new_data, blueprint)
    Condition
      Error in `glubort()`:
      ! The following required columns are missing: 'id'.

