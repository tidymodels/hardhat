# check on input to `new_blueprint()`

    Code
      new_blueprint(same_new_arg = 1, same_new_arg = 2)
    Condition
      Error in `new_blueprint()`:
      ! All elements of `...` must have unique names.

# checks for updating a blueprint

    Code
      update_blueprint(blueprint, intercept = TRUE, intercept = FALSE)
    Condition
      Error in `update_blueprint()`:
      ! `...` must have unique names.

---

    Code
      update_blueprint(blueprint, intercpt = TRUE)
    Condition
      Error in `update_blueprint()`:
      ! All elements of `...` must already exist.
      i The following fields are new: "intercpt".

# checks the ptype

    Code
      new_blueprint(ptypes = list(x = 1))
    Condition
      Error in `new_blueprint()`:
      ! `ptypes` must have an element named "predictors".

---

    Code
      new_blueprint(ptypes = list(predictors = "not a tibble", outcomes = "not a tibble"))
    Condition
      Error in `new_blueprint()`:
      ! `ptypes$predictors` must be a tibble, not the string "not a tibble".

---

    Code
      new_blueprint(ptypes = list(predictors = tibble_too_long, outcomes = tibble_too_long))
    Condition
      Error in `new_blueprint()`:
      ! `ptypes$predictors` must be size 0, not size 1.

