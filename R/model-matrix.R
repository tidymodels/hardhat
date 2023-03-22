#' Construct a design matrix
#'
#' `model_matrix()` is a stricter version of [stats::model.matrix()]. Notably,
#' `model_matrix()` will _never_ drop rows, and the result will be a tibble.
#'
#' @param terms A terms object to construct a model matrix with. This is
#' typically the terms object returned from the corresponding call to
#' [model_frame()].
#'
#' @param data A tibble to construct the design matrix with. This is
#' typically the tibble returned from the corresponding call to
#' [model_frame()].
#'
#' @details
#'
#' The following explains the rationale for some of the difference in arguments
#' compared to [stats::model.matrix()]:
#'
#' - `contrasts.arg`: Set the contrasts argument, `options("contrasts")`
#' globally, or assign a contrast to the factor of interest directly using
#' [stats::contrasts()]. See the examples section.
#'
#' - `xlev`: Not allowed because `model.frame()` is never called, so it is
#' unnecessary.
#'
#' - `...`: Not allowed because the default method of `model.matrix()` does
#' not use it, and the `lm` method uses it to pass potential offsets and
#' weights through, which are handled differently in hardhat.
#'
#' @return
#'
#' A tibble containing the design matrix.
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Example usage
#'
#' framed <- model_frame(Sepal.Width ~ Species, iris)
#'
#' model_matrix(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Missing values never result in dropped rows
#'
#' iris2 <- iris
#' iris2$Species[1] <- NA
#'
#' framed2 <- model_frame(Sepal.Width ~ Species, iris2)
#'
#' model_matrix(framed2$terms, framed2$data)
#'
#' # ---------------------------------------------------------------------------
#' # Contrasts
#'
#' # Default contrasts
#' y <- factor(c("a", "b"))
#' x <- data.frame(y = y)
#' framed <- model_frame(~y, x)
#'
#' # Setting contrasts directly
#' y_with_contrast <- y
#' contrasts(y_with_contrast) <- contr.sum(2)
#' x2 <- data.frame(y = y_with_contrast)
#' framed2 <- model_frame(~y, x2)
#'
#' # Compare!
#' model_matrix(framed$terms, framed$data)
#' model_matrix(framed2$terms, framed2$data)
#'
#' # Also, can set the contrasts globally
#' global_override <- c(unordered = "contr.sum", ordered = "contr.poly")
#'
#' rlang::with_options(
#'   .expr = {
#'     model_matrix(framed$terms, framed$data)
#'   },
#'   contrasts = global_override
#' )
#' @export
model_matrix <- function(terms, data) {
  check_terms(terms)
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  # otherwise model.matrix() will try and run model.frame() for us on data
  # but we definitely don't want this, as we have already done it and it can
  # actually error out if we don't prevent it from running
  attr(data, "terms") <- terms

  predictors <- with_options(
    model.matrix(object = terms, data = data),
    na.action = "na.pass"
  )

  predictors <- strip_model_matrix(predictors)

  tibble::as_tibble(predictors)
}

strip_model_matrix <- function(x) {
  colnames <- colnames(x)
  dimnames <- list(NULL, colnames)

  dim <- dim(x)

  attrs <- list(dim = dim, dimnames = dimnames)

  attributes(x) <- attrs

  x
}

check_terms <- function(x,
                        ...,
                        allow_null = FALSE,
                        arg = caller_arg(x),
                        call = caller_env()) {
  check_inherits(
    x = x,
    what = "terms",
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

# ------------------------------------------------------------------------------

model_matrix_one_hot <- function(terms, data) {
  check_terms(terms)
  check_data_frame_or_matrix(data)
  data <- coerce_to_tibble(data)

  n_cols <- length(data)

  # Convert character to factor ahead of time
  # so we can apply the one hot contrast
  for (i in seq_len(n_cols)) {
    col <- data[[i]]

    if (is.character(col)) {
      data[[i]] <- factor(col)
    }
  }

  # Locate unordered factors only
  indicator_unordered_factors <- vapply(data, is_unordered_factor, logical(1))

  names <- names(data)
  names <- names[indicator_unordered_factors]

  # Pre-assign the `contrasts<-` of each unordered factor using
  # `contr_one_hot()` so `model.matrix()` doesn't overwrite them with the
  # default that comes from `getOption("contrasts")`
  for (name in names) {
    col <- data[[name]]
    lvls <- levels(col)
    n <- length(lvls)
    contrasts <- contr_one_hot(lvls)
    data[[name]] <- assign_contrasts(col, n, contrasts)
  }

  model_matrix(terms, data)
}

#' Contrast function for one-hot encodings
#'
#' This contrast function produces a model matrix that has indicator columns for
#' each level of each factor.
#'
#' @param n A vector of character factor levels or the number of unique levels.
#' @param contrasts This argument is for backwards compatibility and only the
#'   default of `TRUE` is supported.
#' @param sparse This argument is for backwards compatibility and only the
#'   default of `FALSE` is supported.
#'
#' @return A diagonal matrix that is `n`-by-`n`.
#'
#' @keywords internal
contr_one_hot <- function(n, contrasts = TRUE, sparse = FALSE) {
  if (sparse) {
    warn("`sparse = TRUE` not implemented for `contr_one_hot()`.")
  }

  if (!contrasts) {
    warn("`contrasts = FALSE` not implemented for `contr_one_hot()`.")
  }

  if (is.character(n)) {
    names <- n
    n <- length(names)
  } else if (is.numeric(n)) {
    n <- as.integer(n)

    if (length(n) != 1L) {
      abort("`n` must have length 1 when an integer is provided.")
    }

    names <- as.character(seq_len(n))
  } else {
    abort("`n` must be a character vector or an integer of size 1.")
  }

  out <- diag(n)

  rownames(out) <- names
  colnames(out) <- names

  out
}

is_unordered_factor <- function(x) {
  inherits(x, "factor") && !inherits(x, "ordered")
}

assign_contrasts <- function(x, how_many, value) {
  stats::`contrasts<-`(x, how_many, value)
}
