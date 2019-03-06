#' \if{html}{\Sexpr[stage=render,results=rd]{"\U0001f631"}} Scream.
#'
#' @description
#'
#' `scream()` performs a number of validation checks on `new_data`, and yells
#' loudly if anything is wrong. `scream()` performs the following validation:
#'
#' - `validate_new_data_classes()` - Checks that the class of each
#' required predictor in `new_data` is the same as the class used
#' during training.
#'
#' - `enforce_new_data_novel_levels()` - Checks that all `new_data` factor
#' columns don't have any _new_ levels when compared with the original data
#' used in training. If there are new levels, a warning is issued and
#' if `drop_novel = TRUE` then they are coerced to `NA`.
#'
#' - `enforce_new_data_level_recovery()` -  Checks that all `new_data` factor
#' columns aren't missing any factor levels when compared with the original data
#' used in training. If there are missing levels, then they are restored
#' and a warning is issued.
#'
#' @details
#'
#' `scream()` is called by [forge()] after [shrink()] but before the
#' actual processing is done. Generally, you don't need to call `scream()`
#' directly, as `forge()` will do it for you.
#'
#' If `outcomes = TRUE`, then the validation steps are performed on the known
#' outcome columns as well. If [mold()] was called with the XY interface,
#' then no preprocessing was done to `y` and `outcomes` will have no effect
#' (if a vector was passed as `y` during the fit, `scream()` has no
#' way of knowing what column in `new_data` corresponds to the outcome).
#'
#' If `scream()` is used as a standalone function, it is good practice to call
#' [shrink()] right before it as there are no checks in `scream()` that ensure
#' that all of the required column names actually exist in `new_data`. Those
#' checks exist in `shrink()`.
#'
#' @param new_data A data frame containing the new data to check the structure
#' of. This should contain the predictors, and potentially the outcomes if
#' `outcomes = TRUE`.
#'
#' @param engine A preprocessing `engine` returned from a call to [mold()].
#'
#' @param outcomes A logical. Should the outcomes be checked as well?
#'
#' @return
#'
#' A tibble containing the required predictors (and potentially the
#' outcomes) after any required structural modifications have been made.
#'
#' @examples
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # mold() is run at model fit time
#' # and a formula preprocessing engine is recorded
#' x <- mold(log(Sepal.Width) ~ Species, train)
#'
#' # Pass that engine to shrink(), along with new_data
#' # to get a tibble of required predictors back
#' test_shrunk <- shrink(test, x$engine)
#'
#' # Now pass that to scream() to perform validation checks
#' # Silence is key!
#' scream(test_shrunk, x$engine)
#'
#' # If `outcomes = TRUE` is used with shrink(),
#' # it should also be used with scream()
#' test_outcome <- shrink(test, x$engine, outcomes = TRUE)
#' scream(test_outcome, x$engine, outcomes = TRUE)
#'
#' # scream() validates that the classes of `new_data`
#' # are the same as the ones used in mold(). The below call
#' # to scream() will fail with an informative error.
#' test2 <- test
#' test2$Species <- as.character(test2$Species)
#'
#' \dontrun{
#' scream(test2, x$engine)
#' }
#'
#' @export
scream <- function(new_data, engine, outcomes = FALSE) {

  new_data <- tibble::as_tibble(new_data)

  original_predictor_classes <- get_data_classes(engine$info$predictors)
  original_predictor_levels <- get_levels(engine$info$predictors)

  if (outcomes) {

    original_outcome_classes <- get_data_classes(engine$info$outcomes)
    original_outcome_levels <- get_levels(engine$info$outcomes)

  }

  new_data
}
