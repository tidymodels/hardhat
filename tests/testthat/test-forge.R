# ------------------------------------------------------------------------------
# run_forge()

test_that("`run_forge()` throws an informative default error", {
  expect_snapshot(error = TRUE, run_forge(1))
})
