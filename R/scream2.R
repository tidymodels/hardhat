# works for predictors or outcomes
scream2 <- function(data, reference) {

  if (is.null(data)) {
    return(NULL)
  }

  data <- check_is_data_like(data, "data")

  # can imagine this catching and rethrowing errors / warnings related to:
  # - factor recovery
  #   - silently recover missing levels
  #   - novel levels dropped. warning only if its actually used in the data and the data becomes NA
  # - ordered factor recovery
  #   - order is always recovered
  #   - same as factor otherwise
  # - errors in casting individual columns to the reference type
  #   - will silently do it if possible

  # NOT caring about too many columns / missing columns, as that was taken care of by shrink
  # and the vctrs behavior (adding NA columns) isn't great here

  # TODO thoughts on vctrs novel level behavior? we mentioned one time
  # having the option to keep new levels

  # TODO waiting on https://github.com/r-lib/vctrs/issues/225 thoughts

  vctrs::vec_cast(data, reference)

}
