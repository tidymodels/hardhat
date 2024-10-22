# asking for the outcome is special cased for vector `y` values

    Code
      forge(iris, x1$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_missing_name_isnt_.outcome()`:
      ! The following required columns are missing: '.outcome'.
      
      (This indicates that `mold()` was called with a vector for `y`. When this is the case, and the outcome columns are requested in `forge()`, `new_data` must include a column with the automatically generated name, '.outcome', containing the outcome.)

---

    Code
      forge(iris, x2$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_missing_name_isnt_.outcome()`:
      ! The following required columns are missing: '.outcome'.
      
      (This indicates that `mold()` was called with a vector for `y`. When this is the case, and the outcome columns are requested in `forge()`, `new_data` must include a column with the automatically generated name, '.outcome', containing the outcome.)

# new_data can only be a data frame / matrix

    Code
      forge("hi", x1$blueprint)
    Condition
      Error in `forge()`:
      ! The class of `new_data`, 'character', is not recognized.

---

    Code
      forge("hi", x2$blueprint)
    Condition
      Error in `forge()`:
      ! The class of `new_data`, 'character', is not recognized.

---

    Code
      forge("hi", x3$blueprint)
    Condition
      Error in `forge()`:
      ! The class of `new_data`, 'character', is not recognized.

# missing predictor columns fail appropriately

    Code
      forge(iris[, 1, drop = FALSE], x1$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Width'.

---

    Code
      forge(iris[, 1, drop = FALSE], x2$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Width'.

---

    Code
      forge(iris[, 3, drop = FALSE], x1$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Length', 'Sepal.Width'.

---

    Code
      forge(iris[, 3, drop = FALSE], x2$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'Sepal.Length', 'Sepal.Width'.

# novel predictor levels are caught

    Code
      xx <- forge(new, x$blueprint)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

# novel predictor levels can be ignored

    Code
      xx <- forge(new, x$blueprint)

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

