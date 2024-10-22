# asking for the outcome when it isn't there fails

    Code
      forge(iris2, x1$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Species'.

---

    Code
      forge(iris2, x2$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Species'.

# missing predictor columns fail appropriately

    Code
      forge(iris[, 1, drop = FALSE], x$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Width'.

---

    Code
      forge(iris[, 3, drop = FALSE], x$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Length', 'Sepal.Width'.

# novel predictor levels are caught

    Code
      xx1 <- forge(new, x1$blueprint)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

---

    Code
      xx2 <- forge(new, x2$blueprint)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.
      Warning:
      ! There are new levels in `f`: NA.
      i Consider using step_unknown() (`?recipes::step_unknown()`) before `step_dummy()` to handle missing values.

# novel predictor levels can be ignored and handled by recipes

    Code
      xx1 <- forge(new, x1$blueprint)

---

    Code
      xx2 <- forge(new, x2$blueprint)

---

    Code
      xx3 <- forge(new, x3$blueprint)

# novel outcome levels are caught

    Code
      xx1 <- forge(new, x1$blueprint, outcomes = TRUE)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

---

    Code
      xx2 <- forge(new, x2$blueprint, outcomes = TRUE)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

# `forge()` will error if required non standard roles are missing

    Code
      forge(iris, x$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Width'.

# `NA` roles are treated as extra roles that are required at `forge()` time

    Code
      forge(iris, x$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Petal.Length'.

# `forge()` is compatible with hardhat 0.2.0 molded blueprints with a basic recipe

    Code
      forge(new_data, blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'x'.

# `forge()` is compatible with hardhat 0.2.0 molded blueprints with a recipe with a nonstandard role

    Code
      forge(new_data, blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'id'.

