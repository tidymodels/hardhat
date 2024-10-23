# spruce - numeric

    Code
      spruce_numeric("hi")
    Condition
      Error in `spruce_numeric()`:
      ! `pred` must be a numeric vector, not the string "hi".

---

    Code
      spruce_numeric(matrix(1))
    Condition
      Error in `spruce_numeric()`:
      ! `pred` must be a numeric vector, not a double matrix.

# spruce - class

    Code
      spruce_class(1)
    Condition
      Error in `spruce_class()`:
      ! `pred_class` must be a factor, not the number 1.

---

    Code
      spruce_class("hi")
    Condition
      Error in `spruce_class()`:
      ! `pred_class` must be a factor, not the string "hi".

# spruce - prob

    Code
      spruce_prob(1, prob_matrix)
    Condition
      Error in `spruce_prob()`:
      ! `pred_levels` must be a character vector, not the number 1.

---

    Code
      spruce_prob(pred_levels, 1)
    Condition
      Error in `spruce_prob()`:
      ! `prob_matrix` must be a numeric matrix, not the number 1.

---

    Code
      spruce_prob("a", matrix("a"))
    Condition
      Error in `spruce_prob()`:
      ! `prob_matrix` must be a numeric matrix, not a character matrix.

---

    Code
      spruce_prob(c("a", "b"), matrix(1, ncol = 3))
    Condition
      Error in `spruce_prob()`:
      ! The number of levels (2) must be
      equal to the number of class probability columns (3).

---

    Code
      spruce_prob(c("a"), matrix(1, ncol = 2))
    Condition
      Error in `spruce_prob()`:
      ! The number of levels (1) must be
      equal to the number of class probability columns (2).

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

