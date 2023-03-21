# RHS with _only_ intercept related terms are caught

    Code
      mold(~0, example_train)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept removal term, `0`.

---

    Code
      mold(~0, example_train, blueprint = bp)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept removal term, `0`.

---

    Code
      mold(~1, example_train)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept term, `1`.

---

    Code
      mold(~ -1, example_train)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept removal term: `- 1`.

# `NULL` can be used to represent empty RHS formulas

    Code
      mold(~0, example_train)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept removal term, `0`.

---

    Code
      mold(~0, example_train, blueprint = bp)
    Condition
      Error in `mold_formula_default_clean()`:
      ! `formula` must not contain the intercept removal term, `0`.

# `blueprint` is validated

    Code
      mold(~x, df, blueprint = 1)
    Condition
      Error in `mold()`:
      ! `blueprint` must be a <formula_blueprint>, not the number 1.

