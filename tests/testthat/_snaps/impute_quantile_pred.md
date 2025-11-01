# impute_quantiles failure modes

    Code
      impute_quantiles(x)
    Condition
      Error in `impute_quantiles()`:
      ! `x` must be a <quantile_pred> object, not a double vector.

---

    Code
      impute_quantiles(x, c(0.1, 0.5, 0.9))
    Condition
      Error in `impute_quantiles()`:
      ! Quantile interpolation is not possible when fewer than 2 quantiles are avaliable.

---

    Code
      impute_quantiles(x, probs = c(-1, 0.2, 2))
    Condition
      Error:
      ! `probs` must be a number between 0 and 1, not the number -1.

---

    Code
      impute_quantiles(x)
    Condition
      Error in `impute_quantiles()`:
      ! argument "probs" is missing, with no default

---

    Code
      impute_quantiles(x, probs, lower = "a")
    Condition
      Error in `impute_quantiles()`:
      ! `lower` must be a number, not the string "a".

---

    Code
      impute_quantiles(x, probs, upper = "b")
    Condition
      Error in `impute_quantiles()`:
      ! `upper` must be a number, not the string "b".

---

    Code
      impute_quantiles(x, probs, lower = NULL)
    Condition
      Error in `impute_quantiles()`:
      ! `lower` must be a number, not `NULL`.

---

    Code
      impute_quantiles(x, probs, lower = 2, upper = -1)
    Condition
      Error in `impute_quantiles()`:
      ! `lower` (2) must be less than `upper` (-1).

---

    Code
      impute_quantiles(x, middle = "middle")
    Condition
      Error in `impute_quantiles()`:
      ! argument "probs" is missing, with no default

