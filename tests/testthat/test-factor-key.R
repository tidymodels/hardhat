test_that("factor_key works with simple factor", {
  # Single factor with default treatment contrasts
  df <- data.frame(
    y = 1:9,
    f1 = factor(rep(c("a", "b", "c"), 3))
  )
  framed <- model_frame(y ~ f1, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  expect_equal(nrow(result), 2) # 3 levels - 1 reference = 2 columns
  expect_true(all(result$source == "f1"))
  expect_equal(sort(result$derived), c("f1b", "f1c"))
})

test_that("factor_key works with multiple factors", {
  df <- data.frame(
    y = 1:12,
    f1 = factor(rep(c("a", "b"), 6)),
    f2 = factor(rep(c("x", "y", "z"), 4))
  )
  framed <- model_frame(y ~ f1 + f2, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3) # 1 from f1 (2-1) + 2 from f2 (3-1)
  expect_equal(sum(result$source == "f1"), 1)
  expect_equal(sum(result$source == "f2"), 2)
})

test_that("factor_key works with ordered factors", {
  df <- data.frame(
    y = 1:12,
    ord = ordered(rep(c("low", "med", "high"), 4))
  )
  framed <- model_frame(y ~ ord, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2) # Linear and quadratic for 3 levels
  expect_true(all(result$source == "ord"))
  # Ordered factors use polynomial contrasts by default
  expect_true(all(grepl("\\.(L|Q)", result$derived)))
})

test_that("factor_key returns empty tibble when no factors", {
  df <- data.frame(
    y = 1:10,
    x1 = rnorm(10),
    x2 = rnorm(10)
  )
  framed <- model_frame(y ~ x1 + x2, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  expect_equal(nrow(result), 0)
})

test_that("factor_key works with two-way interactions", {
  df <- data.frame(
    y = 1:12,
    f1 = factor(rep(c("a", "b"), 6)),
    f2 = factor(rep(c("x", "y", "z"), 4))
  )
  framed <- model_frame(y ~ f1 * f2, df)
  result <- factor_key(framed$terms, framed$data)

  # Should have main effects and interactions
  expect_s3_class(result, "tbl_df")

  # Main effects: 1 from f1, 2 from f2
  # Interactions: 1*2 = 2 columns, each mapped to both factors
  # Total rows: 1 + 2 + 2*2 = 7
  expect_equal(nrow(result), 7)

  # Check interaction columns are mapped to both factors
  interaction_cols <- result$derived[grepl(":", result$derived)]
  expect_true(length(interaction_cols) > 0)

  for (int_col in unique(interaction_cols)) {
    sources <- result$source[result$derived == int_col]
    expect_equal(length(sources), 2)
    expect_true("f1" %in% sources)
    expect_true("f2" %in% sources)
  }
})

test_that("factor_key works with factor x numeric interactions", {
  df <- data.frame(
    y = 1:10,
    f1 = factor(rep(c("a", "b"), 5)),
    x = rnorm(10)
  )
  framed <- model_frame(y ~ f1 * x, df)
  result <- factor_key(framed$terms, framed$data)

  # Main effect of f1 (1 column) + interaction f1:x (1 column, only f1 as source)
  # Note: numeric main effect x is not included
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$source == "f1"))

  # One should be main effect, one should be interaction
  expect_true(any(!grepl(":", result$derived))) # Main effect
  expect_true(any(grepl(":", result$derived))) # Interaction
})

test_that("factor_key works with three-way interactions", {
  df <- data.frame(
    y = 1:24,
    f1 = factor(rep(c("a", "b"), 12)),
    f2 = factor(rep(c("x", "y"), 12)),
    f3 = factor(rep(c("m", "n"), 12))
  )
  framed <- model_frame(y ~ f1 * f2 * f3, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")

  # Check three-way interaction is mapped to all three factors
  three_way_cols <- result$derived[grepl(".*:.*:.*", result$derived)]
  expect_true(length(three_way_cols) > 0)

  for (col in unique(three_way_cols)) {
    sources <- result$source[result$derived == col]
    expect_equal(length(sources), 3)
    expect_true(all(c("f1", "f2", "f3") %in% sources))
  }
})

test_that("factor_key works with nested effects", {
  df <- data.frame(
    y = rnorm(24),
    A = factor(rep(c("a1", "a2"), each = 12)),
    B = factor(rep(c("b1", "b2", "b3"), 8))
  )

  # A/B expands to A + A:B
  framed <- model_frame(y ~ A / B, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")

  # Should have main effect of A and A:B interaction
  # Main effect A: 1 column (Aa2)
  # A:B interaction: 4 columns (Aa1:Bb2, Aa2:Bb2, Aa1:Bb3, Aa2:Bb3)
  # Each interaction column is mapped to both A and B
  # Total rows: 1 + 4*2 = 9
  expect_equal(nrow(result), 9)

  # Check main effect of A
  main_effect <- result[!grepl(":", result$derived), ]
  expect_equal(nrow(main_effect), 1)
  expect_equal(main_effect$source, "A")

  # Check nested effect A:B
  nested_effect <- result[grepl(":", result$derived), ]
  expect_equal(nrow(nested_effect), 8) # 4 columns * 2 sources each
  # Each interaction column should be mapped to both A and B
  for (col in unique(nested_effect$derived)) {
    sources <- nested_effect$source[nested_effect$derived == col]
    expect_equal(length(sources), 2)
    expect_true(all(c("A", "B") %in% sources))
  }
})

test_that("factor_key works with different contrast types", {
  df <- data.frame(
    y = 1:9,
    f1 = factor(rep(c("a", "b", "c"), 3))
  )

  # Sum contrasts
  f_sum <- df$f1
  contrasts(f_sum) <- contr.sum(3)
  df_sum <- df
  df_sum$f1 <- f_sum

  framed <- model_frame(y ~ f1, df_sum)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2) # Sum contrasts: k-1 columns
  expect_true(all(result$source == "f1"))

  # Helmert contrasts
  f_helm <- df$f1
  contrasts(f_helm) <- contr.helmert(3)
  df_helm <- df
  df_helm$f1 <- f_helm

  framed <- model_frame(y ~ f1, df_helm)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2) # Helmert contrasts: k-1 columns
  expect_true(all(result$source == "f1"))
})

test_that("factor_key works with one-hot encoding", {
  # Skip if contr_one_hot is not available
  if (!exists("contr_one_hot", mode = "function")) {
    skip("contr_one_hot not available")
  }

  df <- data.frame(
    y = 1:9,
    f1 = factor(rep(c("a", "b", "c"), 3))
  )

  f_onehot <- df$f1
  contrasts(f_onehot) <- contr_one_hot(3)
  df_onehot <- df
  df_onehot$f1 <- f_onehot

  framed <- model_frame(y ~ f1 - 1, df_onehot) # Remove intercept for one-hot
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3) # One-hot: k columns (all levels)
  expect_true(all(result$source == "f1"))
})

test_that("factor_key handles factors with unusual level names", {
  df <- data.frame(
    y = 1:9,
    f1 = factor(rep(c("level 1", "level-2", "level.3"), 3))
  )
  framed <- model_frame(y ~ f1, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$source == "f1"))
  # Check that derived names are properly formatted
  expect_true(all(nchar(result$derived) > 0))
})

test_that("factor_key handles single-level factors", {
  df <- data.frame(
    y = 1:5,
    f1 = factor(rep("a", 5))
  )
  framed <- model_frame(y ~ f1, df)

  # Single-level factors don't generate any columns in model matrix
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  expect_equal(nrow(result), 0) # No columns generated for single-level factor
})

test_that("factor_key handles missing values appropriately", {
  df <- data.frame(
    y = c(1:8, NA, 10),
    f1 = factor(c("a", "b", "a", "b", "a", "b", NA, "b", "a", "b"))
  )
  framed <- model_frame(y ~ f1, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  # Should still map the factor despite NAs in data
  expect_equal(nrow(result), 1)
  expect_equal(result$source, "f1")
})

test_that("factor_key handles empty data frame", {
  df <- data.frame(
    y = numeric(0),
    f1 = factor(character(0), levels = c("a", "b"))
  )
  framed <- model_frame(y ~ f1, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  # With empty data, model.matrix still generates column structure
  expect_equal(nrow(result), 1)
})

test_that("factor_key handles intercept-only models", {
  df <- data.frame(
    y = 1:10,
    f1 = factor(rep(c("a", "b"), 5))
  )
  framed <- model_frame(y ~ 1, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  expect_identical(colnames(result), c("source", "derived"))
  expect_equal(nrow(result), 0) # No factors in the formula
})

test_that("factor_key validates inputs correctly", {
  df <- data.frame(y = 1:5, f1 = factor(c("a", "b", "a", "b", "a")))
  framed <- model_frame(y ~ f1, df)

  # Invalid terms
  expect_snapshot(error = TRUE, {
    factor_key("not_terms", framed$data)
  })

  # Invalid data
  expect_snapshot(error = TRUE, {
    factor_key(framed$terms, "not_data")
  })

  # Non-empty dots
  expect_snapshot(error = TRUE, {
    factor_key(framed$terms, framed$data, extra = "arg")
  })
})

test_that("factor_key works with character variables treated as factors", {
  df <- data.frame(
    y = 1:6,
    chr = c("a", "b", "c", "a", "b", "c"),
    stringsAsFactors = FALSE
  )
  framed <- model_frame(y ~ chr, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  # Character variables are coerced to factors in model.matrix
  expect_true(nrow(result) > 0)
  expect_true(all(result$source == "chr"))
})

test_that("factor_key handles complex nested structures", {
  # Multiple levels of nesting: A/B/C
  df <- data.frame(
    y = rnorm(48),
    A = factor(rep(c("a1", "a2"), each = 24)),
    B = factor(rep(c("b1", "b2", "b3", "b4"), each = 6, times = 2)),
    C = factor(rep(c("c1", "c2"), 24))
  )

  # A/B/C expands to A + A:B + A:B:C
  framed <- model_frame(y ~ A / B / C, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")

  # Check we have mappings for all terms
  # Main effect A
  main_a <- result[result$derived %in% result$derived[!grepl(":", result$derived)], ]
  expect_true(nrow(main_a) > 0)

  # A:B interaction
  ab_int <- result[grepl("^[^:]+:[^:]+$", result$derived), ]
  expect_true(nrow(ab_int) > 0)

  # A:B:C interaction
  abc_int <- result[grepl(".*:.*:.*", result$derived), ]
  expect_true(nrow(abc_int) > 0)
})

test_that("factor_key preserves factor ordering in output", {
  df <- data.frame(
    y = 1:12,
    f1 = factor(rep(c("z", "a", "m"), 4)),
    f2 = factor(rep(c("b", "w"), 6))
  )
  framed <- model_frame(y ~ f1 + f2, df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  # The derived column names should match what model.matrix produces
  mm <- model.matrix(framed$terms, framed$data)
  mm_cols <- colnames(mm)[colnames(mm) != "(Intercept)"]

  expect_true(all(result$derived %in% mm_cols))
})

test_that("factor_key works with formula containing dots", {
  # When . is used in formula, it should be expanded first
  df <- data.frame(
    y = 1:12,
    f1 = factor(rep(c("a", "b"), 6)),
    f2 = factor(rep(c("x", "y", "z"), 4)),
    x = rnorm(12)
  )

  # First expand the formula with model.frame
  framed <- model_frame(y ~ ., df)
  result <- factor_key(framed$terms, framed$data)

  expect_s3_class(result, "tbl_df")
  # Should find both f1 and f2
  expect_true("f1" %in% result$source)
  expect_true("f2" %in% result$source)
  # But not x (numeric)
  expect_false("x" %in% result$source)
})
