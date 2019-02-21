context("test-enforce")

test_that("novel levels - `drop_novel = FALSE` works", {

  original_levels <- get_levels(iris)

  iris2 <- iris

  iris2$Species <- factor(
    gsub("versicolor", "new_level", iris2$Species),
    levels = c("new_level", levels(iris$Species))
  )

  expect_warning(
    x <- enforce_new_data_novel_levels(iris2, original_levels, drop_novel = FALSE),
    "'new_level'"
  )

  expect_true("new_level" %in% levels(x$Species))

})

test_that("novel levels - skip past non-factors / non-existant columns", {

  expect_error(
    enforce_new_data_novel_levels(mtcars, get_levels(iris)),
    NA
  )

  iris2 <- iris
  iris2$Species <- as.character(iris2$Species)

  expect_error(
    enforce_new_data_novel_levels(iris2, get_levels(iris)),
    NA
  )

})

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

test_that("level order - skip non-ordered-factors and NULL", {

  df <- data.frame(
    x = ordered(c("a", "b", "c"))
  )

  df2 <- data.frame(
    x = factor(c("a", "b"))
  )

  df3 <- data.frame(
    x = ordered(c("a", "b", "c"), c("c", "b", "a"))
  )

  df_cls <- get_data_classes(df)
  df_lvl <- get_levels(df)

  # no ordered factors
  expect_error(
    enforce_new_data_level_order(mtcars, get_data_classes(mtcars), get_levels(mtcars)),
    NA
  )

  # levels supplied but no ordered factors
  expect_error(
    enforce_new_data_level_order(mtcars, get_data_classes(mtcars), get_levels(df)),
    NA
  )

  # ordered factor and levels, but col not there
  expect_error(
    enforce_new_data_level_order(mtcars, df_cls, df_lvl),
    NA
  )

  # factor is there, not ordered factor, so skip
  expect_error(
    enforce_new_data_level_order(df2, df_cls, df_lvl),
    NA
  )

})

test_that("level order - bad order", {

  df <- data.frame(
    x = ordered(c("a", "b", "c"))
  )

  df_cls <- get_data_classes(df)
  df_lvl <- get_levels(df)

  bad_order <- data.frame(
    x = ordered(c("a", "b", "c"), c("c", "b", "a"))
  )

  # original order restored
  expect_warning(
    x <- enforce_new_data_level_order(bad_order, df_cls, df_lvl),
    "order of the levels does not match"
  )

  expect_equal(
    levels(x$x),
    c("a", "b", "c")
  )

})

test_that("level order - too many", {

  df <- data.frame(
    x = ordered(c("a", "b", "c"))
  )

  df_cls <- get_data_classes(df)
  df_lvl <- get_levels(df)

  too_many <- data.frame(
    x = ordered(c("a", "b", "d", "c"), levels = c("a", "b", "d", "c"))
  )

  too_many_wrong_order <- data.frame(
    x = ordered(c("a", "b", "d", "c"), levels = c("b", "a", "d", "c"))
  )

  # novel levels moved to end
  expect_warning(
    too_many_1 <- enforce_new_data_level_order(too_many, df_cls, df_lvl),
    "'x': 'd'"
  )

  expect_equal(
    levels(too_many_1$x),
    c("a", "b", "c", "d")
  )

  # novel levels moved to end, original order restored
  expect_warning(
    too_many_2 <- enforce_new_data_level_order(too_many_wrong_order, df_cls, df_lvl),
    "'x': 'd'"
  )

  expect_equal(
    levels(too_many_2$x),
    c("a", "b", "c", "d")
  )

})

test_that("level order - not enough", {

  df <- data.frame(
    x = ordered(c("a", "b", "c"))
  )

  df_cls <- get_data_classes(df)
  df_lvl <- get_levels(df)

  not_enough <- data.frame(
    x = ordered(c("a", "b"))
  )

  not_enough_wrong_order <- data.frame(
    x = ordered(c("a", "b"), levels = c("b", "a"))
  )

  # missing level added back
  expect_warning(
    not_enough_1 <- enforce_new_data_level_order(not_enough, df_cls, df_lvl),
    "'x': 'c'"
  )

  expect_equal(
    levels(not_enough_1$x),
    c("a", "b", "c")
  )

  # missing level is added, original order is restored
  expect_warning(
    not_enough_2 <- enforce_new_data_level_order(not_enough_wrong_order, df_cls, df_lvl),
    "'x': 'c'"
  )

  expect_equal(
    levels(not_enough_2$x),
    c("a", "b", "c")
  )

})
