# `na_remove` is validated

    Code
      weighted_table(x, y, weights = w, na_remove = c(TRUE, FALSE))
    Error <rlang_error>
      `na_remove` must be a single `TRUE` or `FALSE`.

---

    Code
      weighted_table(x, y, weights = w, na_remove = 1)
    Error <rlang_error>
      `na_remove` must be a single `TRUE` or `FALSE`.

# requires at least one `...`

    Code
      weighted_table(weights = w)
    Error <rlang_error>
      At least one vector must be supplied to `...`.

# requires all `...` to be factors

    Code
      weighted_table(1, weights = w)
    Error <rlang_error>
      All elements of `...` must be factors.

# requires all `...` to be the same size

    Code
      weighted_table(x, y, weights = w)
    Error <rlang_error>
      All elements of `...` must be the same size.

# requires all `weights` to be the same size as `...` elements

    Code
      weighted_table(x, y, weights = w)
    Error <vctrs_error_assert_size>
      `weights` must have size 3, not size 4.

# requires `weights` to be castable to double

    Code
      weighted_table(x, weights = "a")
    Error <vctrs_error_incompatible_type>
      Can't convert <character> to <double>.

