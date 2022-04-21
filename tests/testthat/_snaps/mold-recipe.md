# `bake_dependent_roles` is validated

    Code
      (expect_error(default_recipe_blueprint(bake_dependent_roles = 1)))
    Output
      <error/rlang_error>
      Error in `glubort()`:
      ! bake_dependent_roles should be a character, not a numeric.
    Code
      (expect_error(default_recipe_blueprint(bake_dependent_roles = c("outcome", "x")))
      )
    Output
      <error/rlang_error>
      Error in `validate_bake_dependent_roles()`:
      ! `bake_dependent_roles` can't be "outcome" or "predictor", as these are already handled.
    Code
      (expect_error(default_recipe_blueprint(bake_dependent_roles = c("predictor",
        "x"))))
    Output
      <error/rlang_error>
      Error in `validate_bake_dependent_roles()`:
      ! `bake_dependent_roles` can't be "outcome" or "predictor", as these are already handled.

