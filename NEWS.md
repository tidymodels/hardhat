# hardhat (development version)

* `default_recipe_blueprint()` now defaults to prepping recipes with
  `fresh = TRUE`. This is a safer default, and guards the user against
  accidentally skipping this preprocessing step when tuning (#122).

* `model_matrix()` now correctly strips all attributes from the result of the
  internal call to `model.matrix()`.

# hardhat 0.1.1

* `forge()` now works correctly when used with a recipe that has a predictor
  with multiple roles (#120).

* Require recipes 0.1.8 to incorporate an important bug fix with `juice()` and
  0-column selections.

# hardhat 0.1.0

* Added a `NEWS.md` file to track changes to the package.
