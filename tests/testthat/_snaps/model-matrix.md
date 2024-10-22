# `contr_one_hot()` input checks

    Code
      contr_one_hot(n = 1, sparse = TRUE)
    Condition
      Warning:
      `sparse = TRUE` not implemented for `contr_one_hot()`.
    Output
        1
      1 1

---

    Code
      contr_one_hot(n = 1, contrasts = FALSE)
    Condition
      Warning:
      `contrasts = FALSE` not implemented for `contr_one_hot()`.
    Output
        1
      1 1

---

    Code
      contr_one_hot(n = 1:2)
    Condition
      Error in `contr_one_hot()`:
      ! `n` must have length 1 when an integer is provided.

---

    Code
      contr_one_hot(n = list(1:2))
    Condition
      Error in `contr_one_hot()`:
      ! `n` must be a character vector or an integer of size 1.

