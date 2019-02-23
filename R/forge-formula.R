#' Forge - Formula Method
#'
#' @description
#'
#' For the terms preprocessor, `forge()` does the following:
#'
#' - Calls [shrink()] to trim `new_data` to only the required columns and
#' coerce `new_data` to a tibble.
#'
#' - Calls [scream()] to perform validation on the structure of the columns
#' of `new_data`.
#'
#' - Predictors
#'
#'    - Runs [stats::model.frame()] on `new_data` using the stored terms
#'    object corresponding to the _predictors_.
#'
#'    - If, in the original [mold()] call, `indicators = TRUE` was set, it
#'    then runs [stats::model.matrix()] on the result.
#'
#'    - If, in the original [mold()] call, `indicators = FALSE` was set, it
#'    runs [stats::model.matrix()] on the result without the factor columns,
#'    and then adds them on afterwards.
#'
#'    - If any offsets are present from using `offset()` in the original call
#'    to [mold()], then they are extracted with [model_offset()].
#'
#'    - If `intercept = TRUE` in the original call to [mold()], then an
#'    intecept column is added.
#'
#'    - Coerces the result of the above steps to a tibble.
#'
#'  - Outcomes
#'
#'    - Runs [stats::model.frame()] on `new_data` using the stored terms object
#'    corresponding to the _outcomes_.
#'
#'    - Coerces the result to a tibble.
#'
#' @inheritParams forge
#'
#' @param preprocessor A `"terms_preprocessor"`.
#'
#' @inherit forge return
#'
#' @examples
#' # ---------------------------------------------------------------------------
#' # Setup
#'
#' train <- iris[1:100,]
#' test <- iris[101:150,]
#'
#' # ---------------------------------------------------------------------------
#' # Formula Example
#'
#' # Call mold() with the training data
#' processed <- mold(
#'   log(Sepal.Length) ~ Sepal.Length + Species,
#'   train,
#'   intercept = TRUE
#' )
#'
#' # Then, call forge() with the preprocessor and the test data
#' # to have it preprocess the test data in the same way
#' forge(processed$preprocessor, test)
#'
#' # Use `outcomes = TRUE` to also extract the preprocessed outcome
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Dummy variables
#'
#' # If factors are not expanded in mold()...
#' processed <- mold(
#'   Sepal.Width ~ Species + Petal.Length:Petal.Width,
#'   train,
#'   indicators = FALSE
#' )
#'
#' # ...then they aren't expanded in forge() either
#' forge(processed$preprocessor, test)
#'
#' # ---------------------------------------------------------------------------
#' # Multivariate outcomes
#'
#' # Multivariate formulas specified in mold()
#' # carry over into forge()
#' processed <- mold(Sepal.Width + log(Sepal.Length) ~ Species, train)
#'
#' forge(processed$preprocessor, test, outcomes = TRUE)
#'
#' # ---------------------------------------------------------------------------
#' # Offsets
#'
#' # Offsets specified in mold() are computed in forge() as well,
#' # and are placed in the `$offset` slot of the result
#' processed <- mold(
#'   Sepal.Width ~ Species + offset(Sepal.Length) + offset(Petal.Width),
#'   train
#' )
#'
#' forge(processed$preprocessor, test)
#'
#' @rdname forge-formula
NULL

# TODO fix this documentation

forge_impl.formula_engine <- function(engine, new_data, outcomes) {

  c(engine, new_data) %<-% engine$forge$clean(engine, new_data, outcomes)
  c(engine, predictors, outcomes) %<-% engine$forge$process(engine, new_data, outcomes)

  forge_list(
    predictors = predictors$data,
    outcomes = outcomes$data,
    offset = predictors$offset
    # extras = extras # maybe this would be useful?
  )
}
