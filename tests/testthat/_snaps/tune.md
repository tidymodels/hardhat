# `id` is validated

    Code
      tune(1)
    Condition
      Error in `tune()`:
      ! `id` must be a single string, not the number 1.

---

    Code
      tune(c("x", "y"))
    Condition
      Error in `tune()`:
      ! `id` must be a single string, not a character vector.

---

    Code
      tune(NA_character_)
    Condition
      Error in `tune()`:
      ! `id` must be a single string, not a character `NA`.

