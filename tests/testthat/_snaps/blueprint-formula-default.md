# `levels` argument is validated

    Code
      new_default_formula_blueprint(levels = 1)
    Condition
      Error in `new_default_formula_blueprint()`:
      ! `levels` must be a list, not the number 1.

---

    Code
      new_default_formula_blueprint(levels = list(1))
    Condition
      Error in `new_default_formula_blueprint()`:
      ! `levels` must be fully named.

---

    Code
      new_default_formula_blueprint(levels = list(a = 1))
    Condition
      Error in `new_default_formula_blueprint()`:
      ! `levels` must only contain character vectors.

