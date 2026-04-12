# factor_key validates inputs correctly

    Code
      factor_key("not_terms", framed$data)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'factor_key' applied to an object of class "character"

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

# factor_key errors appropriately for recipe_blueprint

    Code
      factor_key(blueprint)
    Condition
      Error in `factor_key()`:
      ! factor_key() is not yet implemented for recipe blueprints.
      i Recipes handle factor encoding during the prep step.
      i Consider using a formula blueprint if you need factor mappings.

