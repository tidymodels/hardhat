#' \if{html}{\Sexpr[stage=render,results=rd]{"\U0001f631"}} Scream.
#'
#' @description
#'
#' `scream()` ensures that the structure of `data` is the same as
#' prototype, `ptype`. Under the hood, [vctrs::vec_cast()] is used, which
#' casts each column of `data` to the same type as the corresponding
#' column in `ptype`.
#'
#' This casting enforces a number of important structural checks,
#' including but not limited to:
#'
#' - _Data Classes_ - Checks that the class of each column in `data` is the
#' same as the corresponding column in `ptype`.
#'
#' - _Novel Levels_ - Checks that the factor columns in `data` don't have any
#' _new_ levels when compared with the `ptype` columns. If there are new
#' levels, a warning is issued and they are coerced to `NA`. This check is
#' optional, and can be turned off with `allow_novel_levels = TRUE`.
#'
#' - _Level Recovery_ - Checks that the factor columns in `data` aren't
#' missing any factor levels when compared with the `ptype` columns. If
#' there are missing levels, then they are restored.
#'
#' @details
#'
#' `scream()` is called by [forge()] after [shrink()] but before the
#' actual processing is done. Generally, you don't need to call `scream()`
#' directly, as `forge()` will do it for you.
#'
#' If `scream()` is used as a standalone function, it is good practice to call
#' [shrink()] right before it as there are no checks in `scream()` that ensure
#' that all of the required column names actually exist in `data`. Those
#' checks exist in `shrink()`.
#'
#' @section Factor Levels:
#'
#' `scream()` tries to be helpful by recovering missing factor levels and
#' warning about novel levels. The following graphic outlines how `scream()`
#' handles factor levels when coercing _from_ a column in `data` _to_ a
#' column in `ptype`.
#'
#' \figure{factor-handling.png}
#'
#' Note that ordered factor handing is much stricter than factor handling.
#' Ordered factors in `data` must have _exactly_ the same levels as ordered
#' factors in `ptype`.
#'
#' @param data A data frame containing the new data to check the structure
#' of.
#'
#' @param ptype A data frame prototype to cast `data` to. This is commonly
#' a 0-row slice of the training set.
#'
#' @param allow_novel_levels Should novel factor levels in `data` be allowed?
#' The safest approach is the default, which throws a warning when novel levels
#' are found, and coerces them to `NA` values. Setting this argument to `TRUE`
#' will ignore all novel levels. This argument does not apply to ordered
#' factors. Novel levels are not allowed in ordered factors because the
#' level ordering is a critical part of the type.
#'
#' @return
#'
#' A tibble containing the required columns after any required structural
#' modifications have been made.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100, ]
#' test <- iris[101:150, ]
#'
#' # mold() is run at model fit time
#' # and a formula preprocessing blueprint is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Inside the result of mold() are the prototype tibbles
#' # for the predictors and the outcomes
#' ptype_pred <- x$blueprint$ptypes$predictors
#' ptype_out <- x$blueprint$ptypes$outcomes
#'
#' # ---------------------------------------------------------------------------
#' # shrink() / scream()
#'
#' # Pass the test data, along with a prototype, to
#' # shrink() to extract the prototype columns
#' test_shrunk <- shrink(test, ptype_pred)
#'
#' # Now pass that to scream() to perform validation checks
#' # If no warnings / errors are thrown, the checks were
#' # successful!
#' scream(test_shrunk, ptype_pred)
#'
#' # ---------------------------------------------------------------------------
#' # Outcomes
#'
#' # To also extract the outcomes, use the outcome prototype
#' test_outcome <- shrink(test, ptype_out)
#' scream(test_outcome, ptype_out)
#'
#' # ---------------------------------------------------------------------------
#' # Casting
#'
#' # scream() uses vctrs::vec_cast() to intelligently convert
#' # new data to the prototype automatically. This means
#' # it can automatically perform certain conversions, like
#' # coercing character columns to factors.
#' test2 <- test
#' test2$Species <- as.character(test2$Species)
#'
#' test2_shrunk <- shrink(test2, ptype_pred)
#' scream(test2_shrunk, ptype_pred)
#'
#' # It can also recover missing factor levels.
#' # For example, it is plausible that the test data only had the
#' # "virginica" level
#' test3 <- test
#' test3$Species <- factor(test3$Species, levels = "virginica")
#'
#' test3_shrunk <- shrink(test3, ptype_pred)
#' test3_fixed <- scream(test3_shrunk, ptype_pred)
#'
#' # scream() recovered the missing levels
#' levels(test3_fixed$Species)
#'
#' # ---------------------------------------------------------------------------
#' # Novel levels
#'
#' # When novel levels with any data are present in `data`, the default
#' # is to coerce them to `NA` values with a warning.
#' test4 <- test
#' test4$Species <- as.character(test4$Species)
#' test4$Species[1] <- "new_level"
#'
#' test4$Species <- factor(
#'   test4$Species,
#'   levels = c(levels(test$Species), "new_level")
#' )
#'
#' test4 <- shrink(test4, ptype_pred)
#'
#' # Warning is thrown
#' test4_removed <- scream(test4, ptype_pred)
#'
#' # Novel level is removed
#' levels(test4_removed$Species)
#'
#' # No warning is thrown
#' test4_kept <- scream(test4, ptype_pred, allow_novel_levels = TRUE)
#'
#' # Novel level is kept
#' levels(test4_kept$Species)
#' @export
scream <- function(data, ptype, allow_novel_levels = FALSE) {
  vec_assert(allow_novel_levels, ptype = logical(), size = 1L)

  if (is.null(data)) {
    return(NULL)
  }

  data <- check_is_data_like(data, "data")

  if (allow_novel_levels) {
    ptype <- add_novel_levels_to_ptype(ptype, data)
  } else {
    data <- remove_novel_levels(data, ptype)
  }

  vec_cast(data, ptype)
}

# ------------------------------------------------------------------------------

# vec_cast() throws an error for any lossy cast. This means that novel factor
# levels in the test data throw an error. For most modeling purposes,
# it is better to convert these to `NA` values, with a warning. We handle this
# before handing off to vctrs, checking each factor column to ensure that there
# are no novel levels.

remove_novel_levels <- function(data, ptype) {
  ptype_fct_indicator <- map_lgl(ptype, is_bare_factor)
  ptype_fct_locs <- which(ptype_fct_indicator)

  if (length(ptype_fct_locs) == 0L) {
    return(data)
  }

  fct_names <- names(ptype_fct_locs)

  for (fct_name in fct_names) {
    data[[fct_name]] <- check_novel_levels(
      data[[fct_name]],
      ptype[[fct_name]],
      fct_name
    )
  }

  data
}

check_novel_levels <- function(x, ptype, column) {
  # Allow characters, consider them factors
  if (is.character(x)) {
    x <- factor(x, levels = unique(x))
  }

  # If not a bare factor, then let `vec_cast()` throw an error later.
  # Ordered factors are stricter and do not allow novel levels in any way.
  if (!is_bare_factor(x)) {
    return(x)
  }

  x_lvls <- levels(x)
  ptype_lvls <- levels(ptype)
  new_lvls <- setdiff(x_lvls, ptype_lvls)

  # All good
  if (length(new_lvls) == 0L) {
    return(x)
  }

  new_locs <- which(x %in% new_lvls | is.na(x))

  # There is at least one new level, but none of them are used in the data
  # vctrs will silently handle it for us
  if (length(new_locs) == 0L) {
    return(x)
  }

  # Use the levels from `x`, not `ptype` as we may still be missing levels
  old_lvls <- setdiff(x_lvls, new_lvls)

  warn_novel_levels(new_lvls, column)

  factor(as.character(x), levels = old_lvls)
}

warn_novel_levels <- function(levels, column) {
  message <- glue(
    "Novel levels found in column '{column}': {glue_quote_collapse(levels)}. ",
    "The levels have been removed, and values have been coerced to 'NA'."
  )

  warn(
    message,
    class = "hardhat_warn_novel_levels",
    levels = levels,
    column = column
  )
}

# ------------------------------------------------------------------------------

# There are cases where we want to ignore any novel levels, but otherwise still
# validate a user's `new_data`. The issue with this is that vec_cast() throws an
# error for any lossy cast. This means that novel factor levels in the
# `new_data` throw an error. To handle this, we add the novel levels to the
# `ptype` to prevent vec_cast() from thinking that it is an error.

add_novel_levels_to_ptype <- function(ptype, data) {
  ptype_fct_indicator <- map_lgl(ptype, is_bare_factor)
  ptype_fct_locs <- which(ptype_fct_indicator)

  if (length(ptype_fct_locs) == 0L) {
    return(ptype)
  }

  fct_names <- names(ptype_fct_locs)

  for (fct_name in fct_names) {
    ptype[[fct_name]] <- add_novel_levels(
      data[[fct_name]],
      ptype[[fct_name]]
    )
  }

  ptype
}

add_novel_levels <- function(x, ptype) {
  # Allow characters, consider them factors
  if (is.character(x)) {
    x <- factor(x, levels = unique(x))
  }

  # If not a bare factor, then let `vec_cast()` throw an error later.
  # Ordered factors are stricter and do not allow novel levels in any way.
  if (!is_bare_factor(x)) {
    return(ptype)
  }

  x_lvls <- levels(x)
  ptype_lvls <- levels(ptype)

  # Ensure that `x_lvls` is first, so order is maintained
  new_ptype_lvls <- union(x_lvls, ptype_lvls)

  factor(
    as.character(ptype),
    levels = new_ptype_lvls
  )
}

# ------------------------------------------------------------------------------

is_bare_factor <- function(x) {
  inherits_only(x, "factor")
}
