# factor_key validates inputs correctly

    Code
      factor_key("not_terms", framed$data)
    Condition
      Error in `factor_key()`:
      ! `terms` must be a <terms>, not the string "not_terms".

---

    Code
      factor_key(framed$terms, "not_data")
    Condition
      Error in `factor_key()`:
      ! `data` must be a data frame or a matrix, not the string "not_data".

---

    Code
      factor_key(framed$terms, framed$data, extra = "arg")
    Condition
      Error in `factor_key()`:
      ! `...` must be empty.
      x Problematic argument:
      * extra = "arg"

