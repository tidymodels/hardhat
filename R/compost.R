# This will eventually live in recipes
# https://github.com/tidymodels/recipes/issues/268

compost <- function(object) {

  if (!recipes::is_trained(object)) {
    return(object)
  }

  object$template <- NULL
  object$retained <- FALSE

  object
}
