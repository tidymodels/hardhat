#' Ensure that the outcome is univariate
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `outcomes` must have 1 column. Atomic vectors are treated as
#' 1 column matrices.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `n_cols` A single numeric. The actual number of columns.
#'
#' @param outcomes An object to check.
#'
#' @return
#'
#' `validate_outcomes_are_univariate()` returns `outcomes` invisibly.
#'
#' `check_outcomes_are_univariate()` returns a named list of two components,
#' `ok` and `n_cols`.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$outcomes` element of the result of a call to [mold()].
#'
#' @examples
#' validate_outcomes_are_univariate(data.frame(x = 1))
#'
#' try(validate_outcomes_are_univariate(mtcars))
#' @family validation functions
#' @export
validate_outcomes_are_univariate <- function(outcomes) {
  check <- check_outcomes_are_univariate(outcomes)

  if (!check$ok) {
    cli::cli_abort(
      "The outcome must be univariate, but {check$n_cols} columns were found."
    )
  }

  invisible(outcomes)
}

#' @rdname validate_outcomes_are_univariate
#' @export
check_outcomes_are_univariate <- function(outcomes) {
  if (!is_vector(outcomes)) {
    n_cols <- 0L
  } else {
    n_cols <- NCOL(outcomes) %||% 0L
  }

  ok <- (n_cols == 1L)

  check <- new_check_result(ok = ok, n_cols = n_cols)

  check
}

# ------------------------------------------------------------------------------

#' Ensure outcomes are all numeric
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `outcomes` must have numeric columns.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `bad_classes` A named list. The names are the names of problematic columns,
#' and the values are the classes of the matching column.
#'
#' @param outcomes An object to check.
#'
#' @return
#'
#' `validate_outcomes_are_numeric()` returns `outcomes` invisibly.
#'
#' `check_outcomes_are_numeric()` returns a named list of two components,
#' `ok` and `bad_classes`.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$outcomes` element of the result of a call to [mold()].
#'
#' @examples
#' # All good
#' check_outcomes_are_numeric(mtcars)
#'
#' # Species is not numeric
#' check_outcomes_are_numeric(iris)
#'
#' # This gives an intelligent error message
#' try(validate_outcomes_are_numeric(iris))
#' @family validation functions
#' @export
validate_outcomes_are_numeric <- function(outcomes) {
  check <- check_outcomes_are_numeric(outcomes)

  if (!check$ok) {
    bad_msg <- style_bad_classes(check$bad_classes)

    cli::cli_abort(c(
      "All outcomes must be numeric.",
      "i" = "{cli::qty(length(check$bad_classes))}The following {?is/are} not:",
      bad_msg
    ))
  }

  invisible(outcomes)
}

style_bad_classes <- function(bad_classes){
  bad_col <- map(names(bad_classes), ~ cli::format_inline("{.val {.x}}"))
  bad_class <- map(bad_classes, ~ cli::format_inline("{.cls {.x}}"))

  glue::glue("{bad_col}: {bad_class}")
}

#' @rdname validate_outcomes_are_numeric
#' @export
check_outcomes_are_numeric <- function(outcomes) {
  check_data_frame_or_matrix(outcomes)
  outcomes <- coerce_to_tibble(outcomes)

  where_numeric <- map_lgl(outcomes, is.numeric)

  ok <- all(where_numeric)

  if (!ok) {
    bad_classes <- get_data_classes(outcomes[, !where_numeric])
  } else {
    bad_classes <- list()
  }

  new_check_result(ok = ok, bad_classes = bad_classes)
}

# ------------------------------------------------------------------------------

#' Ensure that the outcome has only factor columns
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `outcomes` must have factor columns.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `bad_classes` A named list. The names are the names of problematic columns,
#' and the values are the classes of the matching column.
#'
#' @param outcomes An object to check.
#'
#' @return
#'
#' `validate_outcomes_are_factors()` returns `outcomes` invisibly.
#'
#' `check_outcomes_are_factors()` returns a named list of two components,
#' `ok` and `bad_classes`.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$outcomes` element of the result of a call to [mold()].
#'
#' @examples
#' # Not a factor column.
#' check_outcomes_are_factors(data.frame(x = 1))
#'
#' # All good
#' check_outcomes_are_factors(data.frame(x = factor(c("A", "B"))))
#' @family validation functions
#' @export
validate_outcomes_are_factors <- function(outcomes) {
  check <- check_outcomes_are_factors(outcomes)

  if (!check$ok) {
    bad_msg <- style_bad_classes(check$bad_classes)

    cli::cli_abort(c(
      "All outcomes must be factors.",
      "i" = "{cli::qty(length(check$bad_classes))}The following {?is/are} not:",
      bad_msg
    ))
  }

  invisible(outcomes)
}

#' @rdname validate_outcomes_are_factors
#' @export
check_outcomes_are_factors <- function(outcomes) {
  check_data_frame_or_matrix(outcomes)
  outcomes <- coerce_to_tibble(outcomes)

  where_factor <- map_lgl(outcomes, is.factor)

  ok <- all(where_factor)

  if (!ok) {
    bad_classes <- get_data_classes(outcomes[, !where_factor])
  } else {
    bad_classes <- list()
  }

  new_check_result(ok = ok, bad_classes = bad_classes)
}

# ------------------------------------------------------------------------------

#' Ensure that the outcome has binary factors
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `outcomes` must have binary factor columns.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `bad_cols` A character vector. The names of the columns with problems.
#'
#' - `num_levels` An integer vector. The actual number of levels of the columns
#' with problems.
#'
#' @param outcomes An object to check.
#'
#' @return
#'
#' `validate_outcomes_are_binary()` returns `outcomes` invisibly.
#'
#' `check_outcomes_are_binary()` returns a named list of three components,
#' `ok`, `bad_cols`, and `num_levels`.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$outcomes` element of the result of a call to [mold()].
#'
#' @examples
#' # Not a binary factor. 0 levels
#' check_outcomes_are_binary(data.frame(x = 1))
#'
#' # Not a binary factor. 1 level
#' check_outcomes_are_binary(data.frame(x = factor("A")))
#'
#' # All good
#' check_outcomes_are_binary(data.frame(x = factor(c("A", "B"))))
#' @family validation functions
#' @export
validate_outcomes_are_binary <- function(outcomes) {
  check <- check_outcomes_are_binary(outcomes)

  if (!check$ok) {
    bad_col <- map(check$bad_cols, ~ cli::format_inline("{.val {.x}}"))

    bad_msg <- glue::glue("{bad_col}: {check$num_levels}")

    cli::cli_abort(c(
      "The outcome must be binary.",
      "i" = "{cli::qty(length(bad_col))}The following number of levels
             {?was/were} found:",
      bad_msg
    ))
  }

  invisible(outcomes)
}

#' @rdname validate_outcomes_are_binary
#' @export
check_outcomes_are_binary <- function(outcomes) {
  check_data_frame_or_matrix(outcomes)
  outcomes <- coerce_to_tibble(outcomes)

  outcomes_levels <- map(outcomes, levels)

  pos_binary_factors <- map_lgl(outcomes_levels, is_binary)

  ok <- all(pos_binary_factors)

  if (!ok) {
    non_binary_levels <- outcomes_levels[!pos_binary_factors]
    num_levels <- map_int(non_binary_levels, length)
    bad_cols <- names(num_levels)
    num_levels <- unname(num_levels)
  } else {
    num_levels <- integer()
    bad_cols <- character()
  }

  new_check_result(ok = ok, bad_cols = bad_cols, num_levels = num_levels)
}

is_binary <- function(x) {
  length(x) == 2L
}

# ------------------------------------------------------------------------------

#' Ensure predictors are all numeric
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `predictors` must have numeric columns.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `bad_classes` A named list. The names are the names of problematic columns,
#' and the values are the classes of the matching column.
#'
#' @param predictors An object to check.
#'
#' @return
#'
#' `validate_predictors_are_numeric()` returns `predictors` invisibly.
#'
#' `check_predictors_are_numeric()` returns a named list of two components,
#' `ok`, and `bad_classes`.
#'
#' @template section-validation
#'
#' @details
#'
#' The expected way to use this validation function is to supply it the
#' `$predictors` element of the result of a call to [mold()].
#'
#' @examples
#' # All good
#' check_predictors_are_numeric(mtcars)
#'
#' # Species is not numeric
#' check_predictors_are_numeric(iris)
#'
#' # This gives an intelligent error message
#' try(validate_predictors_are_numeric(iris))
#' @family validation functions
#' @export
validate_predictors_are_numeric <- function(predictors) {
  check <- check_predictors_are_numeric(predictors)

  if (!check$ok) {
    bad_msg <- style_bad_classes(check$bad_classes)

    cli::cli_abort(c(
      "All predictors must be numeric.",
      "i" = "{cli::qty(length(check$bad_classes))}The following {?is/are} not:",
      bad_msg
    ))
  }

  invisible(predictors)
}

#' @rdname validate_predictors_are_numeric
#' @export
check_predictors_are_numeric <- function(predictors) {
  check_data_frame_or_matrix(predictors)
  predictors <- coerce_to_tibble(predictors)

  where_numeric <- map_lgl(predictors, is.numeric)

  ok <- all(where_numeric)

  if (!ok) {
    bad_classes <- get_data_classes(predictors[, !where_numeric])
  } else {
    bad_classes <- list()
  }

  new_check_result(ok = ok, bad_classes = bad_classes)
}

# ------------------------------------------------------------------------------

#' Ensure that `data` contains required column names
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - The column names of `data` must contain all `original_names`.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `missing_names` A character vector. The missing column names.
#'
#' @details
#'
#' A special error is thrown if the missing column is named `".outcome"`. This
#' only happens in the case where [mold()] is called using the xy-method, and
#' a _vector_ `y` value is supplied rather than a data frame or matrix. In that
#' case, `y` is coerced to a data frame, and the automatic name `".outcome"` is
#' added, and this is what is looked for in [forge()]. If this happens, and the
#' user tries to request outcomes using `forge(..., outcomes = TRUE)` but
#' the supplied `new_data` does not contain the required `".outcome"` column,
#' a special error is thrown telling them what to do. See the examples!
#'
#' @param data A data frame to check.
#'
#' @param original_names A character vector. The original column names.
#'
#' @return
#'
#' `validate_column_names()` returns `data` invisibly.
#'
#' `check_column_names()` returns a named list of two components,
#' `ok`, and `missing_names`.
#'
#' @template section-validation
#'
#' @examples
#' # ---------------------------------------------------------------------------
#'
#' original_names <- colnames(mtcars)
#'
#' test <- mtcars
#' bad_test <- test[, -c(3, 4)]
#'
#' # All good
#' check_column_names(test, original_names)
#'
#' # Missing 2 columns
#' check_column_names(bad_test, original_names)
#'
#' # Will error
#' try(validate_column_names(bad_test, original_names))
#'
#' # ---------------------------------------------------------------------------
#' # Special error when `.outcome` is missing
#'
#' train <- iris[1:100, ]
#' test <- iris[101:150, ]
#'
#' train_x <- subset(train, select = -Species)
#' train_y <- train$Species
#'
#' # Here, y is a vector
#' processed <- mold(train_x, train_y)
#'
#' # So the default column name is `".outcome"`
#' processed$outcomes
#'
#' # It doesn't affect forge() normally
#' forge(test, processed$blueprint)
#'
#' # But if the outcome is requested, and `".outcome"`
#' # is not present in `new_data`, an error is thrown
#' # with very specific instructions
#' try(forge(test, processed$blueprint, outcomes = TRUE))
#'
#' # To get this to work, just create an .outcome column in new_data
#' test$.outcome <- test$Species
#'
#' forge(test, processed$blueprint, outcomes = TRUE)
#' @family validation functions
#' @export
validate_column_names <- function(data, original_names) {
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  check <- check_column_names(data, original_names)

  if (!check$ok) {
    validate_missing_name_isnt_.outcome(check$missing_names)

    cli::cli_abort("The required column{?s} {.arg {check$missing_names}} {?is/are} missing.")
  }

  invisible(data)
}

#' @rdname validate_column_names
#' @export
check_column_names <- function(data, original_names) {
  check_character(original_names)

  new_names <- colnames(data)

  has_names <- original_names %in% new_names

  ok <- all(has_names)

  if (!ok) {
    missing_names <- original_names[!has_names]
  } else {
    missing_names <- character()
  }

  new_check_result(ok = ok, missing_names = missing_names)
}

validate_missing_name_isnt_.outcome <- function(missing_names) {
  not_ok <- ".outcome" %in% missing_names

  if (not_ok) {
    cli::cli_abort(
      c(
        "The following required columns are missing: {.val {missing_names}}.",
        "i" = "This indicates that {.fn mold} was called with a vector for {.arg y}.",
        "i" = "When this is the case, and the outcome columns are requested in
           {.fn forge}, {.arg new_data} must include a column with the
           automatically generated name, {.code .outcome}, containing the outcome."
      )
    )
  }

  invisible(missing_names)
}

# ------------------------------------------------------------------------------

#' Ensure that predictions have the correct number of rows
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - The size of `pred` must be the same as the size of `new_data`.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `size_new_data` A single numeric. The size of `new_data`.
#'
#' - `size_pred` A single numeric. The size of `pred`.
#'
#' @param pred A tibble. The predictions to return from any prediction
#' `type`. This is often created using one of the spruce functions, like
#' [spruce_numeric()].
#'
#' @param new_data A data frame of new predictors and possibly outcomes.
#'
#' @return
#'
#' `validate_prediction_size()` returns `pred` invisibly.
#'
#' `check_prediction_size()` returns a named list of three components,
#' `ok`, `size_new_data`, and `size_pred`.
#'
#' @details
#'
#' This validation function is one that is more developer focused rather than
#' user focused. It is a final check to be used right before a value is
#' returned from your specific `predict()` method, and is mainly a "good
#' practice" sanity check to ensure that your prediction blueprint always returns
#' the same number of rows as `new_data`, which is one of the modeling
#' conventions this package tries to promote.
#'
#' @template section-validation
#'
#' @examples
#' # Say new_data has 5 rows
#' new_data <- mtcars[1:5, ]
#'
#' # And somehow you generate predictions
#' # for those 5 rows
#' pred_vec <- 1:5
#'
#' # Then you use `spruce_numeric()` to clean
#' # up these numeric predictions
#' pred <- spruce_numeric(pred_vec)
#'
#' pred
#'
#' # Use this check to ensure that
#' # the number of rows or pred match new_data
#' check_prediction_size(pred, new_data)
#'
#' # An informative error message is thrown
#' # if the rows are different
#' try(validate_prediction_size(spruce_numeric(1:4), new_data))
#' @family validation functions
#' @export
validate_prediction_size <- function(pred, new_data) {
  check <- check_prediction_size(pred, new_data)

  if (!check$ok) {
    cli::cli_abort(
      "The size of {.arg new_data} ({check$size_new_data}) must match the
      size of {.arg pred} ({check$size_pred})."
    )
  }

  invisible(pred)
}

#' @rdname validate_prediction_size
#' @export
check_prediction_size <- function(pred, new_data) {
  check_data_frame_or_matrix(new_data)
  new_data <- coerce_to_tibble(new_data)

  size_new_data <- vec_size(new_data)
  size_pred <- vec_size(pred)

  ok <- size_pred == size_new_data

  new_check_result(ok = ok, size_new_data = size_new_data, size_pred = size_pred)
}

# ------------------------------------------------------------------------------

#' Ensure no duplicate terms appear in `formula`
#'
#' @description
#'
#' validate - asserts the following:
#'
#' - `formula` must not have duplicates terms on the left and right hand
#' side of the formula.
#'
#' check - returns the following:
#'
#' - `ok` A logical. Does the check pass?
#'
#' - `duplicates` A character vector. The duplicate terms.
#'
#' @param formula A formula to check.
#'
#' @param original A logical. Should the original names be checked, or should
#' the names after processing be used? If `FALSE`, `y ~ log(y)` is allowed
#' because the names are `"y"` and `"log(y)"`, if `TRUE`, `y ~ log(y)` is not
#' allowed because the original names are both `"y"`.
#'
#' @return
#'
#' `validate_no_formula_duplication()` returns `formula` invisibly.
#'
#' `check_no_formula_duplication()` returns a named list of two components,
#' `ok` and `duplicates`.
#'
#' @template section-validation
#'
#' @examples
#' # All good
#' check_no_formula_duplication(y ~ x)
#'
#' # Not good!
#' check_no_formula_duplication(y ~ y)
#'
#' # This is generally okay
#' check_no_formula_duplication(y ~ log(y))
#'
#' # But you can be more strict
#' check_no_formula_duplication(y ~ log(y), original = TRUE)
#'
#' # This would throw an error
#' try(validate_no_formula_duplication(log(y) ~ log(y)))
#' @family validation functions
#' @export
validate_no_formula_duplication <- function(formula, original = FALSE) {
  check <- check_no_formula_duplication(formula, original)

  if (!check$ok) {
    cli::cli_abort(
      c(
        "Terms must not be duplicated on the left- and right-hand side
     of the {.arg formula}.",
        "i" = "The following duplicated term{?s} {?was/were} found:
              {.val {check$duplicates}}"
      )
    )
  }

  invisible(formula)
}

#' @rdname validate_no_formula_duplication
#' @export
check_no_formula_duplication <- function(formula, original = FALSE) {
  check_formula(formula)
  check_bool(original)

  # Only required to expand any `.` values so terms() can be called
  # The `.` is designed to never contain duplicates, so we just expand
  # it to this column name that we hope never exists
  dummy_data <- data.frame(`...dummy...` = 1)
  formula <- expand_formula_dot_notation(formula, data = dummy_data)

  formula_predictors <- get_predictors_formula(formula)
  formula_outcomes <- get_outcomes_formula(formula)

  if (original) {
    predictors <- all.vars(formula_predictors)
    outcomes <- all.vars(formula_outcomes)
  } else {
    predictors <- attr(terms(formula_predictors), "term.labels")
    outcomes <- attr(terms(formula_outcomes), "term.labels")
  }

  duplicates <- intersect(predictors, outcomes)

  ok <- length(duplicates) == 0L

  new_check_result(ok = ok, duplicates = duplicates)
}

# ------------------------------------------------------------------------------

# ok = bool
# ... = extra info when not ok
new_check_result <- function(ok = TRUE, ...) {
  check_bool(ok)
  elems <- list2(...)

  c(list(ok = ok), elems)
}
