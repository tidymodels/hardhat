# errors are thrown if `indicator = 'none'` and factor interactions exist

    Code
      mold(num_1 ~ fac_1:num_2, example_train, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `fac_1:num_2`.

---

    Code
      mold(num_1 ~ fac_1:num_2, example_train, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `fac_1:num_2`.

---

    Code
      mold(num_1 ~ fac_1 * num_2, example_train, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `fac_1 * num_2`.

---

    Code
      mold(num_1 ~ (fac_1 + num_2)^2, example_train, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `(fac_1 + num_2)^2`.

---

    Code
      mold(num_1 ~ fac_1 %in% num_2, example_train, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `fac_1 %in% num_2`.

---

    Code
      mold(~ fac_1:fac_12, example_train2, blueprint = default_formula_blueprint(
        indicators = "none"))
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Interaction terms involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Interactions terms involving factors were detected for "fac_1" in `fac_1:fac_12`.

# errors are thrown if `indicator = 'none'` and factors are used in inline functions

    Code
      mold(~ paste0(fac_1), example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `paste0(fac_1)`.

---

    Code
      mold(~ paste0(fac_1), example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `paste0(fac_1)`.

---

    Code
      mold(~ fac_1 %>% paste0(), example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `fac_1 %>% paste0()`.

---

    Code
      mold(~ paste0(fac_1 + fac_1), example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `fac_1 + fac_1`.

---

    Code
      mold(~ (fac_1) & num_1, example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `(fac_1)`.

---

    Code
      mold(~ (fac_1 & num_1), example_train, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `fac_1 & num_1`.

---

    Code
      mold(~ paste0(fac_1) + paste0(fac_12), example_train2, blueprint = blueprint_no_indicators)
    Condition
      Error in `mold_formula_default_process_predictors()`:
      ! Functions involving factors or characters have been detected on the RHS of `formula`. These are not allowed when `indicators = "none"`.
      i Functions involving factors were detected for "fac_1" in `paste0(fac_1)`.

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

# LHS of the formula cannot contain interactions

    Code
      mold(num_1:num_2 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `num_1:num_2`.

---

    Code
      mold(num_1 * num_2 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `num_1 * num_2`.

---

    Code
      mold(num_1 %in% num_2 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `num_1 %in% num_2`.

---

    Code
      mold((num_1 + num_2)^2 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `(num_1 + num_2)^2`.

---

    Code
      mold(num_1:num_2 + fac_1:num_1 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `num_1:num_2`.

---

    Code
      mold(num_1 / num_2 ~ num_2, example_train)
    Condition
      Error in `mold_formula_default_process_outcomes()`:
      ! Interaction terms can't be specified on the LHS of `formula`.
      i The following interaction term was found: `num_1/num_2`.

# `blueprint` is validated

    Code
      mold(~x, df, blueprint = 1)
    Condition
      Error in `mold()`:
      ! `blueprint` must be a <formula_blueprint>, not the number 1.

