# columns must be numeric when coercing to matrix

    Code
      recompose(df, composition = "matrix")
    Condition
      Error:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "y" and "z".

# columns must be numeric when coercing to sparse matrix

    Code
      recompose(df, composition = "dgCMatrix")
    Condition
      Error:
      ! `data` must only contain numeric columns.
      i These columns aren't numeric: "y" and "z".

# checks for data frame input

    Code
      recompose(1)
    Condition
      Error:
      ! `data` must be a data frame, not the number 1.

# dots must be empty

    Code
      recompose(data.frame(), 1)
    Condition
      Error in `recompose()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# validates `composition`

    Code
      recompose(data.frame(), composition = "foo")
    Condition
      Error:
      ! `composition` must be one of "tibble", "data.frame", "matrix", or "dgCMatrix", not "foo".

---

    Code
      recompose(data.frame(), composition = 1)
    Condition
      Error:
      ! `composition` must be a string or character vector.

