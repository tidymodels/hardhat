# standardize - matrix

    Code
      standardize(mat_bad)
    Condition
      Error in `standardize()`:
      ! All columns of `y` must have unique names.

---

    Code
      standardize(mat_bad2)
    Condition
      Error in `standardize()`:
      ! `y` must be a numeric matrix, not a character matrix.

# standardize - array

    Code
      standardize(bad)
    Condition
      Error in `standardize()`:
      ! All columns of `y` must have unique names.

---

    Code
      standardize(bad2)
    Condition
      Error in `standardize()`:
      ! `y` must be a numeric matrix, not a character matrix.

# standardize - data.frame

    Code
      standardize(bad)
    Condition
      Error in `standardize()`:
      ! All columns of `y` must have unique names.

---

    Code
      standardize(bad2)
    Condition
      Error in `validate_has_known_outcome_types()`:
      ! Not all columns of `y` are known outcome types.
      i This column has an unknown type: "x".

---

    Code
      standardize(bad3)
    Condition
      Error in `validate_has_known_outcome_types()`:
      ! Not all columns of `y` are known outcome types.
      i These columns have unknown types: "x" and "y".

# standardize - unknown

    Code
      standardize("hi")
    Condition
      Error in `standardize()`:
      ! No `standardize()` method provided for a string.

---

    Code
      standardize(Sys.time())
    Condition
      Error in `standardize()`:
      ! No `standardize()` method provided for a <POSIXct> object.

