# similar to delete.response()
# but also removes the dataClasses element
# corresponding to y if it exists
# http://r.789695.n4.nabble.com/delete-response-leaves-response-in-attribute-dataClasses-td4266902.html

# TODO - export this?

#' @rdname utilities
delete_response <- function(x) {
  resp <- attr(x, "response")
  data_class <- attr(x, "dataClasses")

  # Remove dataClass corresponding to y
  # if it exists
  if (!is.null(resp)) {
    if (!is.null(data_class)) {
      attr(x, "dataClasses") <- data_class[-resp]
    }
  }

  delete.response(x)
}
