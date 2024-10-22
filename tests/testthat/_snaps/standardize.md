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
      ! Not all columns of `y` are known outcome types. These columns have unknown types: 'x'.

# standardize - unknown

    Code
      standardize("hi")
    Condition
      Error in `standardize()`:
      ! `y` is of unknown type 'character'.

---

    Code
      standardize(Sys.time())
    Condition
      Error in `standardize()`:
      ! `y` is of unknown type 'POSIXct'.

