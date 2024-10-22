# validate_outcomes_are_univariate()

    Code
      validate_outcomes_are_univariate(iris)
    Condition
      Error in `validate_outcomes_are_univariate()`:
      ! The outcome must be univariate, but 5 columns were found.

# validate_outcomes_are_numeric()

    Code
      validate_outcomes_are_numeric(iris)
    Condition
      Error in `validate_outcomes_are_numeric()`:
      ! All outcomes must be numeric, but the following are not:
      'Species': 'factor'

---

    Code
      validate_outcomes_are_numeric(x)
    Condition
      Error in `validate_outcomes_are_numeric()`:
      ! All outcomes must be numeric, but the following are not:
      'x': 'POSIXct', 'POSIXt'
      'y': 'factor'

# validate_no_formula_duplication()

    Code
      validate_no_formula_duplication(y ~ y)
    Condition
      Error in `validate_no_formula_duplication()`:
      ! The following terms are duplicated on the left and right hand side of the `formula`: 'y'.

---

    Code
      validate_no_formula_duplication(y ~ log(y), original = TRUE)
    Condition
      Error in `validate_no_formula_duplication()`:
      ! The following terms are duplicated on the left and right hand side of the `formula`: 'y'.

---

    Code
      validate_no_formula_duplication(y + x ~ y + x)
    Condition
      Error in `validate_no_formula_duplication()`:
      ! The following terms are duplicated on the left and right hand side of the `formula`: 'y', 'x'.

---

    Code
      validate_no_formula_duplication(y ~ . + y)
    Condition
      Error in `validate_no_formula_duplication()`:
      ! The following terms are duplicated on the left and right hand side of the `formula`: 'y'.

---

    Code
      validate_no_formula_duplication(y ~ offset(y), original = TRUE)
    Condition
      Error in `validate_no_formula_duplication()`:
      ! The following terms are duplicated on the left and right hand side of the `formula`: 'y'.

# validate_outcomes_are_factors()

    Code
      validate_outcomes_are_factors(x)
    Condition
      Error in `validate_outcomes_are_factors()`:
      ! All outcomes must be factors, but the following are not:
      'x': 'POSIXct', 'POSIXt'
      'y': 'character'

# validate_outcomes_are_binary()

    Code
      validate_outcomes_are_binary(iris)
    Condition
      Error in `validate_outcomes_are_binary()`:
      ! The outcome must be binary, but the following number of levels were found:
      'Sepal.Length': 0
      'Sepal.Width': 0
      'Petal.Length': 0
      'Petal.Width': 0
      'Species': 3

# validate_predictors_are_numeric()

    Code
      validate_predictors_are_numeric(iris)
    Condition
      Error in `validate_predictors_are_numeric()`:
      ! All predictors must be numeric, but the following are not:
      'Species': 'factor'

---

    Code
      validate_predictors_are_numeric(x)
    Condition
      Error in `validate_predictors_are_numeric()`:
      ! All predictors must be numeric, but the following are not:
      'x': 'POSIXct', 'POSIXt'
      'y': 'factor'

# validate_prediction_size()

    Code
      validate_prediction_size(mtcars[1:5, ], mtcars)
    Condition
      Error in `validate_prediction_size()`:
      ! The size of `new_data` (32) must match the size of `pred` (5).

