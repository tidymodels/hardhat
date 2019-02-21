
#' Recover original factor levels
#'
#' A factor column of `new_data` might have a subset of the original factor
#' levels for some reason. This function checks for that, and recodes the
#' new factor to have the original levels, with a warning.
#'
#' Note that _new_ factor levels have already been taken care of by now, so
#' `new_data` factor columns can only have either exactly the right levels, or
#' a subset of the correct levels.
#'
#' @param new_data A data frame of new predictors and maybe outcomes.
#' @param original_levels A named list of the original levels of either the
#' outcomes or predictors. These are stored in the preprocessor slots
#' `$predictors$levels` and `$outcomes$levels`.
#'
#' @keywords internal
enforce_new_data_level_recovery <- function(new_data, original_levels) {

  required_column_names <- names(original_levels)

  for(required_column_name in required_column_names) {

    # Could be ordered or factor
    new_nominal <- new_data[[required_column_name]]

    .ordered <- is.ordered(new_nominal)

    new_levels <- levels(new_nominal)
    old_levels <- original_levels[[required_column_name]]

    # If its not an ordered factor, then the order isn't important
    # for uniqueness, so we sort before checking if identical()
    if (!.ordered) {
      new_levels <- sort(new_levels)
      old_levels <- sort(old_levels)
    }

    # Because we ran a check for novel levels already,
    # we know there are either exactly the same levels,
    # or not enough levels
    ok <- identical(old_levels, new_levels)

    if (!ok) {

      missing_levels <- setdiff(old_levels, new_levels)

      strictly_misordered <- length(missing_levels) == 0L

      if (strictly_misordered) {

        rlang::warn(glue::glue(
          "Column, '{required_column_name}', is an ordered factor, but the ",
          "levels in `new_data` are misordered and have been reordered."
        ))

      }

      if (!strictly_misordered) {

        missing_levels <- glue::glue_collapse(
          glue::single_quote(missing_levels),
          sep = ", "
        )

        rlang::warn(glue::glue(
          "The following original factor levels are missing ",
          "for column, '{required_column_name}', and have been restored: ",
          "{missing_levels}."
        ))

      }

      new_data[[required_column_name]] <- factor(
        x = new_nominal,
        levels = old_levels,
        ordered = .ordered
      )

    }

  }

  new_data
}

#' Check for new factor levels
#'
#' A factor column of `new_data` might have _new_ factor levels when compared
#' to the original levels used in training. These new levels are undefined, and
#' are caught when preprocessing the `new_data` and are converted to `NA` with
#' a warning.
#'
#' @inheritParams enforce_new_data_level_recovery
#'
#' @keywords internal
enforce_new_data_novel_levels <- function(new_data, original_levels) {

  required_column_names <- names(original_levels)

  for(required_column_name in required_column_names) {

    new_nominal <- new_data[[required_column_name]]

    new_levels <- levels(new_nominal)
    old_levels <- original_levels[[required_column_name]]

    unseen_levels <- setdiff(new_levels, old_levels)

    if (length(unseen_levels) > 0) {

      unseen_levels <- glue::glue_collapse(
        glue::single_quote(unseen_levels),
        sep = ", "
      )

      rlang::warn(glue(
        "The following factor levels are new ",
        "for column, `{required_column_name}`, ",
        "and have been coerced to `NA`: {unseen_levels}."
      ))

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

  }

  new_data
}
