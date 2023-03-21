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
      Error in `validate_numeric_elements()`:
      ! `y` should have numeric elements, not character.

# standardize - array

    Code
      standardize(bad)
    Condition
      Error in `standardize()`:
      ! All columns of `y` must have unique names.

# standardize - data.frame

    Code
      standardize(bad)
    Condition
      Error in `standardize()`:
      ! All columns of `y` must have unique names.

