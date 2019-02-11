# TODO: How useful is this now?

#' Update the fields of an existing model
#'
#' `update_fields()` is a generic function for altering the existing fields
#' of an existing model object. The default method should be enough to perform
#' the update, but an  S3 method for your model might be necessary to ensure
#' that the _types_ of the new fields are valid.
#'
#' `update_fields()` is particularly useful in the formula and recipes methods
#' of the model fit function. There, a preprocessor can be added to the
#' `custom_model` you get back from calling the core fit engine.
#'
#' @param x A model object.
#' @param ... Named fields to update. These names must already exist in `x`.
#'
#' @keywords internal
update_fields <- function(x, ...) {
  UseMethod("update_fields")
}

update_fields.default <- function(x, ...) {

  new_fields <- list(...)

  # Ensure new fields are named
  validate_has_unique_names(new_fields, "...")

  new_field_names <- names(new_fields)
  old_field_names <- names(x)

  # Check existance of new_fields in x
  fields_exist <- new_field_names %in% old_field_names

  if (!all(fields_exist)) {
    dont_exist <- new_field_names[!fields_exist]
    dont_exist <- glue::glue_collapse(dont_exist, ", ")
    glubort(
      "The following fields passed to `...` don't exist in `x`: {dont_exist}."
    )
  }

  # Update fields
  for(nm in new_field_names) {
    x[[nm]] <- new_fields[[nm]]
  }

  x
}
