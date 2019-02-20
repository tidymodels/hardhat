# - Preprocessing should _never_ removes rows
# with incomplete data. Setting the na.action
# to na.pass will retain the NA values through
# the preprocessing

# Most options aren't great, so only allow `original_levels` to pass through
# can get something of this form from `get_levels()`

# We split the terms and data frame from each other and then return them
# as a named list because you pretty much always separate them anyways. Personally
# I dont think they should have ever been together.

# Can't simplify terms env here, sometimes we need it to exist

model_frame <- function(formula, data, original_levels = NULL) {

  frame <- rlang::with_options(
    stats::model.frame(formula, data = data, xlev = original_levels),
    na.action = "na.pass"
  )

  terms <- terms(frame)

  attr(frame, "terms") <- NULL
  data <- tibble::as_tibble(frame)

  list(
    data = data,
    terms = terms
  )

}
