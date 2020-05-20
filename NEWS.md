# hardhat (development version)

* Updated to stay current with the latest vctrs 0.3.0 conventions.

* `scream()` is now stricter when checking ordered factor levels in new data
  against the `ptype` used at training time. Ordered factors must now have
  _exactly_ the same set of levels at training and prediction time. See
  `?scream` for a new graphic outlining how factor levels are handled (#132).

* The novel factor level check in `scream()` no longer throws a novel level
  warning on `NA` values (#131).

# hardhat 0.1.2

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
