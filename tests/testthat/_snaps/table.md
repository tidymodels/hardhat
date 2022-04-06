# `na_remove` is validated

    Code
      weighted_table(x, y, weights = w, na_remove = c(TRUE, FALSE))
    Condition
      Error in `weighted_table()`:
      ! `na_remove` must be a single `TRUE` or `FALSE`.

---

    Code
      weighted_table(x, y, weights = w, na_remove = 1)
    Condition
      Error in `weighted_table()`:
      ! `na_remove` must be a single `TRUE` or `FALSE`.

# requires at least one `...`

    Code
      weighted_table(weights = w)
    Condition
      Error in `weighted_table()`:
      ! At least one vector must be supplied to `...`.

# requires all `...` to be factors

    Code
      weighted_table(1, weights = w)
    Condition
      Error in `weighted_table()`:
      ! All elements of `...` must be factors.

# requires all `...` to be the same size

    Code
      weighted_table(x, y, weights = w)
    Condition
      Error in `weighted_table()`:
      ! All elements of `...` must be the same size.

# requires all `weights` to be the same size as `...` elements

    Code
      weighted_table(x, y, weights = w)
    Condition
      Error in `weighted_table()`:
      ! `weights` must have size 3, not size 4.

# requires `weights` to be castable to double

    Code
      weighted_table(x, weights = "a")
    Condition
      Error in `weighted_table()`:
      ! Can't convert `weights` <character> to <double>.

