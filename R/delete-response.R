#' Delete the response from a terms object
#'
#' `delete_response()` is exactly the same as `delete.response()`, except
#' that it fixes a long standing bug by also removing the part of the
#' `"dataClasses"` attribute corresponding to the response, if it exists.
#'
#' @param terms A terms object.
#'
#' @return
#'
#' `terms` with the response sections removed.
#'
#' @details
#'
#' The bug is described here:
#'
#' \url{https://stat.ethz.ch/pipermail/r-devel/2012-January/062942.html}
#'
#' @examples
#'
#' framed <- model_frame(Species ~ Sepal.Width, iris)
#'
#' attr(delete.response(framed$terms), "dataClasses")
#'
#' attr(delete_response(framed$terms), "dataClasses")
#' @export
delete_response <- function(terms) {
  check_terms(terms)

  resp <- attr(terms, "response")
  data_class <- attr(terms, "dataClasses")

  response_exists <- !(is.null(resp) || (resp == 0L))
  data_class_exists <- !is.null(data_class)

  # Remove dataClass corresponding to y if it exists
  if (response_exists & data_class_exists) {
    attr(terms, "dataClasses") <- data_class[-resp]
  }

  delete.response(terms)
}
