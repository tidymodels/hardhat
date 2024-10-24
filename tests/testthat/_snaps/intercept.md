# existing intercepts are skipped with a warning

    Code
      xx <- add_intercept_column(x)
    Condition
      Warning:
      `data` already has a column named "(Intercept)".
      i Returning `data` unchanged.

# name can only be a single character

    Code
      add_intercept_column(mtcars, name = c("x", "y"))
    Condition
      Error in `add_intercept_column()`:
      ! `name` must be a valid name, not a character vector.

---

    Code
      add_intercept_column(mtcars, name = 1)
    Condition
      Error in `add_intercept_column()`:
      ! `name` must be a valid name, not the number 1.

# data has to be a data frame or matrix

    Code
      add_intercept_column(1)
    Condition
      Error in `add_intercept_column()`:
      ! `data` must be a data frame or matrix to add an intercept column, not a number.

