#' Extract the outcome
#' 
#' Most of the time, the input to a model should be flexible enough to capture
#' a number of different input types from the user. `extract_outcome()` captures
#' that flexibility for you, and converts user input of various types into a 
#' common output type based on the `mode` and `variateness` of your model.
#' 
#' @param y The outcome.
#' @param mode Either `"classification"` or `"regression"`.
#' @param variateness Either `"univariate"` or `"multivariate"` depending
#' on whether or not you have >1 outcome per observation.
#' 
#' 
#' @section Univariate Regression:
#' 
#' _Returns:_
#' 
#' * A single numeric vector
#' 
#' _Accepts:_
#' 
#' * A numeric vector
#' 
#' * A data frame with 1 numeric column
#' 
#' * A numeric matrix with 1 column
#' 
#' @section Multivariate Regression:
#' 
#' _Returns:_
#' 
#' * A numeric matrix with as many columns as there are outcomes
#' 
#' _Accepts:_
#' 
#' * A data frame with >1 numeric columns
#' 
#' * A numeric matrix with >1 columns
#' 
#' @section Univariate Classification:
#' 
#' _Returns:_
#' 
#' * A factor
#' 
#' _Accepts:_
#' 
#' * A factor
#' 
#' * A data frame with 1 factor column
#' 
#' @section Multivariate Classification:
#' 
#' _Returns:_
#' 
#' * A data frame with as many factor columns as there are outcomes
#' 
#' _Accepts:_
#' 
#' * A data frame with >1 factor columns
#' 
#' @export
#' 
extract_outcome <- function(y, mode, variateness) {
  validate_mode(mode)
  validate_variateness(variateness)
  
  type <- glue("{variateness}_{mode}")
  type <- dispatchify(type)
  
  extract_outcome_impl(y, type)
}

extract_outcome_impl <- function(y, type) {
  UseMethod("extract_outcome_impl", y)
}

# ------------------------------------------------------------------------------
# Default outcome error

extract_outcome_impl.default <- function(y, type) {
  cls <- class(y)[1]
  abort("Unknown `y` class: {cls}")
}

# ------------------------------------------------------------------------------
# Factor outcome

extract_outcome_impl.factor <- function(y, type) {
  UseMethod("extract_outcome_impl.factor", type)
}

extract_outcome_impl.factor.default <- function(y, type) {
  abort_unknown_type(type)
}

extract_outcome_impl.factor.univariate_regression <- function(y, type) {
  abort_univariate_regression(y)
}

extract_outcome_impl.factor.univariate_classification <- function(y, type) {
  y
}

extract_outcome_impl.factor.multivariate_regression <- function(y, type) {
  abort_multivariate_regression(y)
}

extract_outcome_impl.factor.multivariate_classification <- function(y, type) {
  abort_multivariate_classification(y)
}

# ------------------------------------------------------------------------------
# Numeric outcome

extract_outcome_impl.numeric <- function(y, type) {
  UseMethod("extract_outcome_impl.numeric", type)
}

extract_outcome_impl.numeric.default <- function(y, type) {
  abort_unknown_type(type)
}

extract_outcome_impl.numeric.univariate_regression <- function(y, type) {
  y
}

extract_outcome_impl.numeric.univariate_classification <- function(y, type) {
  abort_univariate_classification(y)
}

extract_outcome_impl.numeric.multivariate_regression <- function(y, type) {
  abort_multivariate_regression(y)
}

extract_outcome_impl.numeric.multivariate_classification <- function(y, type) {
  abort_multivariate_classification(y)
}

# ------------------------------------------------------------------------------
# Data frame outcome

extract_outcome_impl.data.frame <- function(y, type) {
  UseMethod("extract_outcome_impl.data.frame", type)
}

extract_outcome_impl.data.frame.default <- function(y, type) {
  abort_unknown_type(type)
}

extract_outcome_impl.data.frame.univariate_regression <- function(y, type) {
  
  ok <- is_univariate(y) && all_numeric(y)
  
  if (!ok) {
    abort_univariate_regression(y)
  }
  
  y[[1]]
}

extract_outcome_impl.data.frame.univariate_classification <- function(y, type) {
  
  ok <- is_univariate(y) && all_factor(y)
  
  if (!ok) {
    abort_univariate_classification(y)
  }
  
  y[[1]]
}

extract_outcome_impl.data.frame.multivariate_regression <- function(y, type) {
  
  ok <- is_multivariate(y) && all_numeric(y)
  
  if (!ok) {
    abort_multivariate_regression(y)
  }
  
  as.matrix(y)
}

extract_outcome_impl.data.frame.multivariate_classification <- function(y, type) {
  
  ok <- is_multivariate(y) && all_factor(y)
  
  if (!ok) {
    abort_multivariate_classification(y)
  }
  
  y
}

# ------------------------------------------------------------------------------
# Matrix outcome

extract_outcome_impl.matrix <- function(y, type) {
  UseMethod("extract_outcome_impl.matrix", type)
}

extract_outcome_impl.matrix.default <- function(y, type) {
  abort_unknown_type(type)
}

extract_outcome_impl.matrix.univariate_regression <- function(y, type) {
  
  ok <- is_univariate(y) && is.numeric(y)
  
  if (!ok) {
    abort_univariate_regression(y)
  }
  
  as.vector(y)
}

extract_outcome_impl.matrix.univariate_classification <- function(y, type) {
  abort_univariate_classification(y)
}

extract_outcome_impl.matrix.multivariate_regression <- function(y, type) {
  
  ok <- is_multivariate(y) && is.numeric(y)
  
  if (!ok) {
    abort_multivariate_regression(y)
  }
  
  y
}

extract_outcome_impl.matrix.multivariate_classification <- function(y, type) {
  abort_multivariate_classification(y)
}

# ------------------------------------------------------------------------------
# Utility

abort_unknown_type <- function(type) {
  glubort("Unknown type: {type}.")
}

abort_univariate_regression <- function(y) {
  bad_class <- first_class(y)
  glubort(
    "\n
    `y` should be one of the following:
       * A numeric vector
       * A numeric 1 column data frame
       * A numeric 1 column matrix
    Instead, a {bad_class} was provided that does not 
    fulfill the above requirements."
  )
} 

abort_multivariate_regression <- function(y) {
  bad_class <- first_class(y)
  glubort(
    "\n
    `y` should be one of the following:
       * A numeric data frame with >1 column
       * A numeric matrix with >1 column
    Instead, a {bad_class} was provided that does not 
    fulfill the above requirements."
  )
} 

abort_univariate_classification <- function(y) {
  bad_class <- first_class(y)
  glubort(
    "\n
    `y` should be one of the following:
       * A factor
       * A 1 column data frame containing a factor
    Instead, a {bad_class} was provided that does not 
    fulfill the above requirements."
  )
} 

abort_multivariate_classification <- function(y) {
  bad_class <- first_class(y)
  glubort(
    "\n
    `y` should be one of the following:
       * A data frame containing >1 factor columns
    Instead, a {bad_class} was provided that does not 
    fulfill the above requirements."
  )
} 

first_class <- function(x) {
  class(x)[1]
}

is_univariate <- function(x) {
  NCOL(x) == 1L
} 

is_multivariate <- function(x) {
  NCOL(x) > 1
}

all_numeric <- function(x) {
  all(vapply(x, is.numeric, logical(1)))
}

all_factor <- function(x) {
  all(vapply(x, is.factor, logical(1)))
}
