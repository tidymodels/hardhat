# ------------------------------------------------------------------------------
# run_mold()

test_that("`run_mold()` throws an informative default error", {
  expect_snapshot(error = TRUE, run_mold(1))
})
