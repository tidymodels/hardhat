# spruce multiple helpers check input type

    Code
      spruce_numeric_multiple(1, "x")
    Condition
      Error in `spruce_numeric_multiple()`:
      ! Each element of `...` must be a numeric vector, not a string.

---

    Code
      spruce_class_multiple(1)
    Condition
      Error in `spruce_class_multiple()`:
      ! Each element of `...` must be a factor, not a number.

---

    Code
      spruce_prob_multiple(1)
    Condition
      Error in `spruce_prob_multiple()`:
      ! Each element of `...` must be a tibble, not a number.

# spruce multiple helpers check input sizes (and disallow recycling)

    Code
      spruce_numeric_multiple(1, 1:2)
    Condition
      Error in `spruce_numeric_multiple()`:
      ! `..1` must have size 2, not size 1.

---

    Code
      spruce_class_multiple(factor("x"), factor(c("a", "b")))
    Condition
      Error in `spruce_class_multiple()`:
      ! `..1` must have size 2, not size 1.

---

    Code
      spruce_prob_multiple(tibble(x = 1), tibble(x = 1:2))
    Condition
      Error in `spruce_prob_multiple()`:
      ! `..1` must have size 2, not size 1.

