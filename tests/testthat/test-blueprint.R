test_that("check on input to `new_blueprint()`", {
  expect_snapshot(error = TRUE, {
    new_blueprint(same_new_arg = 1, same_new_arg = 2)
  })
})

test_that("checks for updating a blueprint", {
  blueprint <- default_xy_blueprint()

  expect_snapshot(error = TRUE, {
    update_blueprint(blueprint, intercept = TRUE, intercept = FALSE)
  })
  expect_snapshot(error = TRUE, {
    update_blueprint(blueprint, intercpt = TRUE)
  })
})

test_that("checks the ptype", {
  expect_snapshot(error = TRUE, {
    new_blueprint(ptypes = list(x = 1))
  })
  expect_snapshot(error = TRUE, {
    new_blueprint(ptypes = list("predictors" = "not a tibble", outcomes = "not a tibble"))
  })

  tibble_too_long <- tibble::tibble(x =1)
  expect_snapshot(error = TRUE, {
    new_blueprint(ptypes = list("predictors" = tibble_too_long, outcomes = tibble_too_long))
  })
})
