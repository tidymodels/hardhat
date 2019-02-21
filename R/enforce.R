#' Recover original factor levels
#'
#' A factor column of `new_data` might have a subset of the original factor
#' levels for some reason. This function checks for that, and recodes the
#' new factor to have the original levels, with a warning.
#'
#' If this function is used in your package, it is a good idea to call
#' [enforce_new_data_novel_levels()] first, as that will take care of any _new_
#' levels if `drop_novel = TRUE`. This will ensure that `new_data` factor
#' columns can only have either exactly the right levels, or a subset of
#' the correct levels.
#'
#' If `enforce_new_data_novel_levels()` was not run, or `drop_novel = FALSE`,
#' then `enforce_new_data_level_recovery()` follows the following heuristic:
#' if any novel levels are detected, then the novel levels are moved to the
#' end, and the remaining levels are reordered to match the order in
#' `original_levels`. This only has an affect for ordered factors, but is a
#' warning worth throwing.
#'
#' @param new_data A data frame of new predictors and possibly outcomes.
#'
#' @param original_levels A named list of the original levels of either the
#' outcomes or predictors. The names match the factor column names in
#' `new_data`, and the values are character vectors of the required levels.
#'
#' @template section-validation
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' iris <- tibble::as_tibble(iris)
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Use with get_levels()
#'
#' # If rolling your package, get_levels() can be useful alongside this check
#' original_levels <- get_levels(train)
#'
#' # All good!
#' enforce_new_data_level_recovery(test, original_levels)
#'
#' # ---------------------------------------------------------------------------
#' # Internally, forge() uses this check like so:
#'
#' x <- mold(Sepal.Length ~ Species, train)
#'
#' # No problems here!
#' enforce_new_data_level_recovery(test, x$preprocessor$info$predictors$levels)
#'
#' # Missing 2 levels
#' bad_test <- test
#' bad_test$Species <- droplevels(bad_test$Species)
#'
#' # Restores the levels with a warning
#' enforce_new_data_level_recovery(bad_test, x$preprocessor$info$predictors$levels)
#'
#' # ---------------------------------------------------------------------------
#' # Novel levels and ordered factors
#'
#' # Ideally, one would run enforce_new_data_novel_levels() before
#' # running the level recovery function to catch novel levels. If
#' # this is not the case, then the original levels take priority
#' # and then any "novel" levels are appended to the end in the order
#' # they appear
#'
#' # df2 is:
#' # - missing level 'b'
#' # - has an additional level, 'd'
#' df1 <- data.frame(x = ordered(c("a", "b", "c")))
#' df2 <- data.frame(x = ordered(c("d", "c", "a"), levels = c("d", "c", "a")))
#'
#' # 'b' is recovered with a warning
#' df2_recovered <- enforce_new_data_level_recovery(df2, get_levels(df1))
#'
#' # 'd' is kept around, but is shifted to the end as the
#' # order of the original levels takes priority
#' df2_recovered$x
#'
#' @family enforce functions
#' @export
enforce_new_data_level_recovery <- function(new_data, original_levels) {

  new_data <- check_is_data_like(new_data)
  validate_levels_list(original_levels, "original_levels")

  required_column_names <- names(original_levels)

  for(required_column_name in required_column_names) {

    ok <- TRUE

    # Could be ordered or factor
    # but we don't care about order in this check
    new_nominal <- new_data[[required_column_name]]

    new_levels <- levels(new_nominal)
    old_levels <- original_levels[[required_column_name]]

    has_old_levels <- old_levels %in% new_levels
    is_missing_levels <- !all(has_old_levels)

    if (is_missing_levels) {

      ok <- FALSE

      missing_old_levels <- old_levels[!has_old_levels]

      missing_old_levels <- glue_quote_collapse(missing_old_levels)

      rlang::warn(glue::glue(
        "The following original factor levels are missing ",
        "for column, '{required_column_name}', and have been restored: ",
        "{missing_old_levels}."
      ))

    }

    new_levels_not_in_old_levels <- new_levels[!(new_levels %in% old_levels)]
    has_any_novel_levels <- length(new_levels_not_in_old_levels) > 0

    if (has_any_novel_levels) {

      ok <- FALSE

      # this is the most rationale way to preserve order for ordered factors.
      # original_levels take priority, then any "novel" levels
      # are appended to the end in the order they appear
      old_levels <- c(old_levels, new_levels_not_in_old_levels)

      new_levels_not_in_old_levels <- glue_quote_collapse(new_levels_not_in_old_levels)

      rlang::warn(glue::glue(
        "The following novel levels were detected in ",
        "'{required_column_name}': {new_levels_not_in_old_levels}. ",
        "The order of the `original_levels` has been restored, and the novel ",
        "levels have been moved to the end."
      ))

    }

    if (!ok) {

      new_data[[required_column_name]] <- factor(
        x = new_nominal,
        levels = old_levels,
        ordered = is.ordered(new_nominal)
      )

    }

  }

  new_data
}

# ------------------------------------------------------------------------------

#' Check for new factor levels
#'
#' A factor column of `new_data` might have _new_ factor levels when compared
#' to the original levels used in training. These new levels are undefined, and
#' are caught when preprocessing the `new_data` and converted to `NA` with
#' a warning.
#'
#' @inheritParams enforce_new_data_level_recovery
#'
#' @param drop_novel A logical. Should novel levels be dropped? If `TRUE`, a
#' warning is thrown if novel levels are detected and they are coerced to `NA`.
#' If `FALSE`, a warning is still thrown for new levels, but they are
#' left unchanged.
#'
#' @template section-validation
#'
#' @examples
#'
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' iris <- tibble::as_tibble(iris)
#' train <- iris[1:75,]
#' test <- iris[76:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Use with get_levels()
#'
#' # If rolling your package, get_levels() can be useful alongside this check
#' original_levels <- get_levels(train)
#'
#' # All good!
#' enforce_new_data_novel_levels(test, original_levels)
#'
#' # New level! Coerced to NA
#' bad_test <- test
#'
#' bad_test$Species <- factor(
#'   gsub("versicolor", "new_level", bad_test$Species),
#'   levels = c("new_level", levels(test$Species))
#' )
#'
#' levels(bad_test$Species)
#'
#' # 'new_level' is forced to NA
#' enforce_new_data_novel_levels(bad_test, original_levels)
#'
#' # Warn about 'new_level' but don't force it to NA
#' enforce_new_data_novel_levels(bad_test, original_levels, drop_novel = FALSE)
#'
#' @family enforce functions
#' @export
enforce_new_data_novel_levels <- function(new_data, original_levels,
                                          drop_novel = TRUE) {

  new_data <- check_is_data_like(new_data)
  validate_levels_list(original_levels, "original_levels")
  validate_is_bool(drop_novel, "drop_novel")

  required_column_names <- names(original_levels)

  for(required_column_name in required_column_names) {

    new_nominal <- new_data[[required_column_name]]

    new_levels <- levels(new_nominal)
    old_levels <- original_levels[[required_column_name]]

    unseen_levels <- setdiff(new_levels, old_levels)

    if (length(unseen_levels) > 0) {

      if (drop_novel) {

        na_coerce <- ", and have been coerced to `NA` "

        # In this order so ordered factors are correct
        # Order is determined by `new_levels`, which is
        # what we want in this check
        seen_lvls <- intersect(new_levels, old_levels)

        new_data[[required_column_name]] <- factor(
          x = new_nominal,
          levels = seen_lvls,
          ordered = is.ordered(new_nominal)
        )

      }
      else{

        na_coerce <- ""

      }

      unseen_levels <- glue_quote_collapse(unseen_levels)

      rlang::warn(glue(
        "The following factor levels are new ",
        "for column, `{required_column_name}`",
        na_coerce,
        ": {unseen_levels}."
      ))

    }

  }

  new_data
}
