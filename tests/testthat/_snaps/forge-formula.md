# asking for the outcome when it isn't there fails

    Code
      forge(example_train2, x1$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'fac_1'.

---

    Code
      forge(example_train2, x2$blueprint, outcomes = TRUE)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'fac_1'.

# new_data can only be a data frame / matrix

    Code
      forge("hi", x1$blueprint)
    Condition
      Error in `forge()`:
      ! No `forge()` method provided for a string object.

---

    Code
      forge("hi", x2$blueprint)
    Condition
      Error in `forge()`:
      ! No `forge()` method provided for a string object.

# missing predictor columns fail appropriately

    Code
      forge(example_train[, 1, drop = FALSE], x1$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'num_2'.

---

    Code
      forge(example_train[, 1, drop = FALSE], x2$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'num_2'.

---

    Code
      forge(example_train[, 3, drop = FALSE], x1$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'num_1', 'num_2'.

---

    Code
      forge(example_train[, 3, drop = FALSE], x2$blueprint)
    Condition
      Error in `validate_column_names()`:
      ! The following required columns are missing: 'num_1', 'num_2'.

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

# novel predictor levels can be ignored

    Code
      xx1 <- forge(new, x1$blueprint)

---

    Code
      xx2 <- forge(new, x2$blueprint)

# novel levels are handled correctly when the new column is a character

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

# novel levels are ignored correctly when the new column is a character

    Code
      xx1 <- forge(new, x1$blueprint)

---

    Code
      xx2 <- forge(new, x2$blueprint)

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

# novel outcome levels are always caught, even if `allow_novel_levels = TRUE`

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

# missing predictor levels are restored silently

    Code
      xx1 <- forge(new, x1$blueprint)

---

    Code
      xx2 <- forge(new, x2$blueprint)

---

    Code
      yy1 <- forge(new2, x1$blueprint)

---

    Code
      yy2 <- forge(new2, x2$blueprint)

# can be both missing levels and have new levels

    Code
      mold(y ~ f, dat, blueprint = bp2)
    Condition
      Error in `recompose()`:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "f".

---

    Code
      xx <- forge(new, x1$blueprint)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

# can be both missing levels and have new levels that get ignored

    Code
      xx <- forge(new, x$blueprint)

# `NA` factor data never triggers a novel level warning (#131)

    Code
      xx <- forge(new, x$blueprint)

# `allow_novel_levels` works right with character predictors

    Code
      out <- forge(df2, x$blueprint)
    Condition
      Warning:
      Novel levels found in column 'x': 'd'. The levels have been removed, and values have been coerced to 'NA'.

