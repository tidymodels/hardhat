#' Importance weights
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `importance_weights()` creates a vector of importance weights which allow you
#' to apply a context dependent weight to your observations. Importance weights
#' are supplied as a non-negative double vector, where fractional values are
#' allowed.
#'
#' @param x A double vector.
#'
#' @return A new importance weights vector.
#'
#' @seealso
#' [frequency_weights()]
#'
#' @export
#' @examples
#' importance_weights(c(1.5, 2.3, 10))
importance_weights <- function(x) {
  x <- vec_cast_named(x, to = double(), x_arg = "x")

  if (any(x < 0, na.rm = TRUE)) {
    abort("`x` can't contain negative weights.")
  }

  new_importance_weights(x)
}

#' Construct an importance weights vector
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `new_importance_weights()` is a developer oriented function for constructing
#' a new importance weights vector. Generally, you should use
#' [importance_weights()] instead.
#'
#' @inheritParams vctrs::new_vctr
#'
#' @param x A double vector.
#'
#' @return A new importance weights vector.
#'
#' @export
#' @examples
#' new_importance_weights()
#' new_importance_weights(c(1.5, 2.3, 10))
new_importance_weights <- function(x = double(), ..., class = character()) {
  if (!is.double(x)) {
    abort("`x` must be a double vector.")
  }

  new_case_weights(
    x = x,
    ...,
    class = c(class, "hardhat_importance_weights")
  )
}

#' Is `x` an importance weights vector?
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `is_importance_weights()` checks if `x` inherits from
#' `"hardhat_importance_weights"`.
#'
#' @param x An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' is_importance_weights(1)
#' is_importance_weights(frequency_weights(1))
#' is_importance_weights(importance_weights(1))
is_importance_weights <- function(x) {
  inherits(x, "hardhat_importance_weights")
}

#' @export
vec_ptype2.hardhat_importance_weights.hardhat_importance_weights <- function(x, y, ...) {
  x
}

#' @export
vec_cast.hardhat_importance_weights.hardhat_importance_weights <- function(x, to, ...) {
  x
}

#' @export
vec_cast.double.hardhat_importance_weights <- function(x, to, ...) {
  unstructure(x)
}

#' @export
vec_ptype_full.hardhat_importance_weights <- function(x, ...) {
  "importance_weights"
}

#' @export
vec_ptype_abbr.hardhat_importance_weights <- function(x, ...) {
  "imp_wts"
}

# ------------------------------------------------------------------------------

#' Frequency weights
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `frequency_weights()` creates a vector of frequency weights which allow you
#' to compactly repeat an observation a set number of times. Frequency weights
#' are supplied as a non-negative integer vector, where only whole numbers are
#' allowed.
#'
#' @param x An integer vector.
#'
#' @return A new frequency weights vector.
#'
#' @seealso
#' [importance_weights()]
#'
#' @export
#' @examples
#' # Record that the first observation has 10 replicates, the second has 12
#' # replicates, and so on
#' frequency_weights(c(10, 12, 2, 1))
#'
#' # Fractional values are not allowed
#' try(frequency_weights(c(1.5, 2.3, 10)))
frequency_weights <- function(x) {
  x <- vec_cast_named(x, to = integer(), x_arg = "x")

  if (any(x < 0L, na.rm = TRUE)) {
    abort("`x` can't contain negative weights.")
  }

  new_frequency_weights(x)
}

#' Construct a frequency weights vector
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `new_frequency_weights()` is a developer oriented function for constructing
#' a new frequency weights vector. Generally, you should use
#' [frequency_weights()] instead.
#'
#' @inheritParams vctrs::new_vctr
#'
#' @param x An integer vector.
#'
#' @return A new frequency weights vector.
#'
#' @export
#' @examples
#' new_frequency_weights()
#' new_frequency_weights(1:5)
new_frequency_weights <- function(x = integer(), ..., class = character()) {
  if (!is.integer(x)) {
    abort("`x` must be an integer vector.")
  }

  new_case_weights(
    x = x,
    ...,
    class = c(class, "hardhat_frequency_weights")
  )
}

#' Is `x` a frequency weights vector?
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `is_frequency_weights()` checks if `x` inherits from
#' `"hardhat_frequency_weights"`.
#'
#' @param x An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' is_frequency_weights(1)
#' is_frequency_weights(frequency_weights(1))
#' is_frequency_weights(importance_weights(1))
is_frequency_weights <- function(x) {
  inherits(x, "hardhat_frequency_weights")
}

#' @export
vec_ptype2.hardhat_frequency_weights.hardhat_frequency_weights <- function(x, y, ...) {
  x
}

#' @export
vec_cast.hardhat_frequency_weights.hardhat_frequency_weights <- function(x, to, ...) {
  x
}

#' @export
vec_cast.integer.hardhat_frequency_weights <- function(x, to, ...) {
  unstructure(x)
}

#' @export
vec_cast.double.hardhat_frequency_weights <- function(x, to, ...) {
  x <- unstructure(x)
  vec_cast_named(x, to = double(), ...)
}

#' @export
vec_ptype_full.hardhat_frequency_weights <- function(x, ...) {
  "frequency_weights"
}

#' @export
vec_ptype_abbr.hardhat_frequency_weights <- function(x, ...) {
  "freq_wts"
}

# ------------------------------------------------------------------------------

#' Extend case weights
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `new_case_weights()` is a developer oriented function for constructing a new
#' case weights type. The `<case_weights>` type itself is an _abstract_ type
#' with very little functionality. Because of this, `class` is a required
#' argument.
#'
#' @inheritParams vctrs::new_vctr
#'
#' @param x An integer or double vector.
#'
#' @return A new subclassed case weights vector.
#'
#' @export
#' @examples
#' new_case_weights(1:5, class = "my_weights")
new_case_weights <- function(x, ..., class) {
  if (!is.integer(x) && !is.double(x)) {
    abort("`x` must be an integer or double vector.")
  }

  new_vctr(
    .data = x,
    ...,
    class = c(class, "hardhat_case_weights"),
    inherit_base_type = FALSE
  )
}

#' Is `x` a case weights vector?
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `is_case_weights()` checks if `x` inherits from `"hardhat_case_weights"`.
#'
#' @param x An object.
#'
#' @return A single `TRUE` or `FALSE`.
#'
#' @export
#' @examples
#' is_case_weights(1)
#' is_case_weights(frequency_weights(1))
is_case_weights <- function(x) {
  inherits(x, "hardhat_case_weights")
}

# ------------------------------------------------------------------------------

unstructure <- function(x) {
  attributes(x) <- list(names = vec_names(x))
  x
}
