context("test-enforce")

test_that("level recovery - novel levels are moved to the end", {

  df1 <- data.frame(x = ordered(c("a", "b", "c")))
  df2 <- data.frame(x = ordered(c("d", "c", "a"), levels = c("d", "c", "a")))

  # Two warnings
  expect_warning(
    x <- enforce_new_data_level_recovery(df2, get_levels(df1)),
    "restored: 'b'"
  )

  expect_warning(
    x <- enforce_new_data_level_recovery(df2, get_levels(df1)),
    "The following novel levels were detected in 'x': 'd'."
  )

  # 'd' is moved to the end to maintain order of "original levels"
  expect_equal(
    levels(x$x),
    c("a", "b", "c", "d")
  )

  # even if no levels are missing, new ones are moved to the end
  # and the original_level order is restored
  df3 <- data.frame(x = ordered(c("a", "b", "d", "e")))
  df4 <- data.frame(x = ordered(c("a", "b", "c", "d", "e")))

  expect_warning(
    x <- enforce_new_data_level_recovery(df4, get_levels(df3)),
    "The following novel levels were detected in 'x': 'c'."
  )

})

test_that("level recovery - skip past non-factors / non-existant columns", {

  expect_error(
    enforce_new_data_level_recovery(mtcars, get_levels(iris)),
    NA
  )

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  expect_error(
    enforce_new_data_level_recovery(iris2, get_levels(iris)),
    NA
  )

})
