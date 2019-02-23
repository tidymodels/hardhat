#' Create a new preprocessing engine
#'
#' This is the base class for a preprocessing engine. All other engines
#' subclass this one.
#'
#' @param mold A named list with two elements:
#'
#' - `clean`: A function that performs initial cleaning of the user's input
#' data to be used in the model.
#'
#' - `process`: A function that performs the actual preprocessing of the data.
#'
#' Both `engine$mold$clean()` and `engine$mold$process()` will be called,
#' in order, from [mold()] and will be passed the `engine` itself, and the
#' data (`x` and `y` for the xy method, but otherwise `data`).
#'
#' @param forge A named list with two elements:
#'
#' - `clean`: A function that performs initial cleaning of the `new_data` that
#' is typically used in generating predictions.
#'
#' - `process`: A function that performs the preprocessing of the `new_data`.
#'
#' Both `engine$forge$clean()` and `engine$forge$process()` will be called,
#' in order, from [mold()] and will be passed the `engine` itself,
#' and the `new_data`.
#'
#' @param intercept A logical. Should an intercept be included in the
#' processed data? This information is used by the `process` function
#' in the `mold` and `forge` function list.
#'
#' @param info Either `NULL`, or a named list with 2 elements:
#'
#' - `predictors`: A named list with the following 3 elements:
#'
#'    - `names`: A character vector of the original predictor column names.
#'
#'    - `classes`: A named list of the original predictor classes, or `NULL`.
#'
#'    - `levels`: A named list of the original predictor levels for any factor
#'    columns, or `NULL`.
#'
#' - `outcomes`: A named list with the following 3 elements:
#'
#'    - `names`: A character vector of the original outcome column names.
#'
#'    - `classes`: A named list of the original outcome classes, or `NULL`.
#'
#'    - `levels`: A named list of the original outcome levels for any factor
#'    columns, or `NULL`.
#'
#' `info` is set at [mold()] time, and is used in [forge()] to validate
#' `new_data`.
#'
#' @param ... Name-value pairs for additional elements of engines that
#' subclass this engine.
#'
#' @param subclass A character vector. The subclasses of this engine.
#'
#' @export
new_engine <- function(mold,
                       forge,
                       intercept = FALSE,
                       info = NULL,
                       ...,
                       subclass = character()) {

  validate_is_function_set(mold)
  validate_is_function_set(forge)
  validate_is_bool(intercept)
  validate_is_info_list_or_null(info)
  validate_is_character(subclass, "subclass")

  elems <- list(
    mold = mold,
    forge = forge,
    intercept = intercept,
    info = info
  )

  new_elems <- list(...)

  validate_has_unique_names(new_elems, "...")

  elems <- c(elems, new_elems)

  structure(elems, class = c(subclass, "hardhat_engine"))

}

# ------------------------------------------------------------------------------

refresh_engine <- function(engine) {
  UseMethod("refresh_engine")
}

refresh_engine.hardhat_engine <- function(engine) {
  do.call(new_engine, as.list(engine))
}

# ------------------------------------------------------------------------------

update_engine <- function(engine, ...) {

  changes <- rlang::list2(...)

  if (!has_unique_names(changes)) {
    glubort("`...` must have unique names.")
  }

  new_nms <- names(changes)
  old_nms <- names(engine)

  for (nm in new_nms) {

    if (!(nm %in% old_nms)) {
      glubort(
        "All elements to change must already exist. `{nm}` is a new field."
      )
    }

    # this nukes elements if we set them to NULL
    engine[[nm]] <- changes[[nm]]
  }

  refresh_engine(engine)
}

# ------------------------------------------------------------------------------

is_engine <- function(x) {
  inherits(x, "hardhat_engine")
}

# ------------------------------------------------------------------------------

# helper for new_engine()$mold and $forge elements
engine_function_set <- function(clean, process) {

  validate_is(clean, rlang::is_function, "function")
  validate_is(process, rlang::is_function, "function")

  list(
    clean = clean,
    process = process
  )
}

mold_predictors_set <- function(data, offset = NULL) {
  list(
    data = data,
    offset = offset
  )
}

mold_outcomes_set <- function(data) {
  list(
    data = data
  )
}

# ------------------------------------------------------------------------------

info_lst <- function(predictors = predictors_info(),
                     outcomes = outcomes_info()) {

  list(
    predictors = predictors,
    outcomes = outcomes
  )

}

predictors_info <- function(names = character(),
                            classes = NULL,
                            levels = NULL) {
  list(
    names = names,
    classes = classes,
    levels = levels
  )
}

outcomes_info <- function(names = character(),
                          classes = NULL,
                          levels = NULL) {
  list(
    names = names,
    classes = classes,
    levels = levels
  )
}

# ------------------------------------------------------------------------------

validate_is_or_null <- function(.x, .f, .expected, .x_nm, .note = "") {

  # capture name first
  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  if (is.null(.x)) {
    return(invisible(.x))
  }

  validate_is(.x, .f, .expected, .x_nm, .note)
}

validate_is_bool_or_null <- function(.x, .x_nm) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  validate_is_or_null(.x, is_bool, "bool", .x_nm = .x_nm, .note = "'TRUE' / 'FALSE'")
}

validate_is_function_set <- function(.x, .x_nm) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  validate_has_function_set_structure(.x, .x_nm)

  validate_is(.x$clean, rlang::is_function, "function", .x_nm = glue("{.x_nm}$clean"))
  validate_is(.x$process, rlang::is_function, "function", .x_nm = glue("{.x_nm}$process"))

  invisible(.x)
}

validate_has_function_set_structure <- function(.x, .x_nm) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  validate_is(.x, rlang::is_list, "list")

  validate_has_name(.x, .x_nm, "clean")
  validate_has_name(.x, .x_nm, "process")

  invisible(.x)
}

validate_is_info_list_or_null <- function(.x, .x_nm) {

  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  if (is.null(.x)) {
    return(invisible(.x))
  }

  validate_has_name(.x, .x_nm, "predictors")
  validate_has_name(.x, .x_nm, "outcomes")

  validate_is_terms_info_list(.x$predictors, glue("{.x_nm}$predictors"))
  validate_is_terms_info_list(.x$outcomes, glue("{.x_nm}$outcomes"))

  invisible(.x)
}


validate_is_terms_info_list <- function(.x, .x_nm) {

  validate_has_name(.x, .x_nm, "names")
  validate_has_name(.x, .x_nm, "classes")
  validate_has_name(.x, .x_nm, "levels")

  validate_is_character(.x$names, glue("{.x_nm}$names"))
  validate_classes_list(.x$classes, glue("{.x_nm}$classes"))
  validate_levels_list(.x$levels, glue("{.x_nm}$levels"))

  invisible(.x)
}

validate_has_name <- function(.x, .x_nm, .nm) {
  if (!tibble::has_name(.x, .nm)) {
    glubort("`{.x_nm}` must have an element named '{.nm}'.")
  }
  invisible(.x)
}

validate_is_character <- function(.x, .x_nm) {
  validate_is(
    .x,
    rlang::is_character,
    "character",
    .x_nm
  )
}

validate_levels_list <- function(lst, lst_nm) {

  valid_levels_obj <- function(x) {

    if (is.null(x)) {
      return(TRUE)
    }

    if (!is.list(x)) {
      return(FALSE)
    }

    ok <- vapply(x, rlang::is_character, logical(1))

    all(ok)
  }

  validate_has_unique_names(lst, lst_nm)

  if (!valid_levels_obj(lst)) {
    glubort("`{lst_nm}` must be a list of character vectors, or `NULL`.")
  }

  invisible(lst)
}

# Just happen to be the same structure
validate_classes_list <- validate_levels_list
