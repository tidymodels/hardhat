# `id` is validated

    Code
      tune(1)
    Condition
      Error in `tune()`:
      ! The `id` should be a single character string.

---

    Code
      tune(c("x", "y"))
    Condition
      Error in `tune()`:
      ! The `id` should be a single character string.

---

    Code
      tune(NA_character_)
    Condition
      Error in `tune()`:
      ! The `id` can't be missing.

