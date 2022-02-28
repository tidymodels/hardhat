#' Create a new preprocessing blueprint
#'
#' @description
#'
#' These are the base classes for creating new preprocessing blueprints. All
#' blueprints inherit from the one created by `new_blueprint()`, and the default
#' method specific blueprints inherit from the other three here.
#'
#' If you want to create your own processing blueprint for a specific method,
#' generally you will subclass one of the method specific blueprints here. If
#' you want to create a completely new preprocessing blueprint for a totally new
#' preprocessing method (i.e. not the formula, xy, or recipe method) then
#' you should subclass `new_blueprint()`.
#'
#' @param mold A named list with two elements, `clean` and `process`, see
#' the [new_blueprint()] section, Mold Functions, for details.
#'
#' @param forge A named list with two elements, `clean` and `process`, see
#' the [new_blueprint()] section, Forge Functions, for details.
#'
#' @param intercept A logical. Should an intercept be included in the
#' processed data? This information is used by the `process` function
#' in the `mold` and `forge` function list.
#'
#' @param allow_novel_levels A logical. Should novel factor levels be allowed at
#' prediction time? This information is used by the `clean` function in the
#' `forge` function list, and is passed on to [scream()].
#'
#' @param composition Either "tibble", "matrix", or "dgCMatrix" for the format
#' of the processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
#' the predictors must be numeric after the preprocessing method has been
#' applied; otherwise an error is thrown.
#'
#' @param ptypes Either `NULL`, or a named list with 2 elements, `predictors`
#' and `outcomes`, both of which are 0-row tibbles. `ptypes` is generated
#' automatically at [mold()] time and is used to validate `new_data` at
#' prediction time. At [mold()] time, the information found in
#' `blueprint$mold$process()$ptype` is used to set `ptypes` for the `blueprint`.
#'
#' @param ... Name-value pairs for additional elements of blueprints that
#' subclass this blueprint.
#'
#' @param subclass A character vector. The subclasses of this blueprint.
#'
#' @return
#'
#' A preprocessing blueprint, which is a list containing the inputs used as
#' arguments to the function, along with a class specific to the type
#' of blueprint being created.
#'
#' @template section-mold-functions
#' @template section-forge-functions
#'
#' @name new-blueprint
#' @export
new_blueprint <- function(mold,
                          forge,
                          intercept = FALSE,
                          allow_novel_levels = FALSE,
                          composition = "tibble",
                          ptypes = NULL,
                          ...,
                          subclass = character()) {
  validate_is_function_set(mold)
  validate_is_function_set(forge)
  validate_is_bool(intercept)
  validate_is_bool(allow_novel_levels)
  validate_composition(composition)
  validate_is_ptype_list_or_null(ptypes)
  validate_is_character(subclass, "subclass")

  # Can't validate mold() args here
  # as they differ per blueprint
  validate_forge_args(forge)

  elems <- list(
    mold = mold,
    forge = forge,
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    composition = composition,
    ptypes = ptypes
  )

  new_elems <- list(...)

  validate_has_unique_names(new_elems, "...")

  elems <- c(elems, new_elems)

  structure(elems, class = c(subclass, "hardhat_blueprint"))
}

# ------------------------------------------------------------------------------

#' Refresh a preprocessing blueprint
#'
#' `refresh_blueprint()` is a developer facing generic function that is called
#' at the end of [update_blueprint()]. It simply is a wrapper around the
#' method specific `new_*_blueprint()` function that runs the updated blueprint
#' through the constructor again to ensure that all of the elements of the
#' blueprint are still valid after the update.
#'
#' If you implement your own custom `blueprint`, you should export a
#' `refresh_blueprint()` method that just calls the constructor for your blueprint
#' and passes through all of the elements of the blueprint to the constructor.
#'
#' @param blueprint A preprocessing blueprint.
#'
#' @return
#'
#' `blueprint` is returned after a call to the corresponding constructor.
#'
#' @examples
#'
#' blueprint <- default_xy_blueprint()
#'
#' # This should never be done manually, but is essentially
#' # what `update_blueprint(blueprint, intercept = TRUE)` does for you
#' blueprint$intercept <- TRUE
#'
#' # Then update_blueprint() will call refresh_blueprint()
#' # to ensure that the structure is correct
#' refresh_blueprint(blueprint)
#'
#' # So you can't do something like...
#' blueprint_bad <- blueprint
#' blueprint_bad$intercept <- 1
#'
#' # ...because the constructor will catch it
#' try(refresh_blueprint(blueprint_bad))
#'
#' # And update_blueprint() catches this automatically
#' try(update_blueprint(blueprint, intercept = 1))
#' @export
refresh_blueprint <- function(blueprint) {
  UseMethod("refresh_blueprint")
}

#' @export
refresh_blueprint.hardhat_blueprint <- function(blueprint) {
  do.call(new_blueprint, as.list(blueprint))
}

# ------------------------------------------------------------------------------

#' Update a preprocessing blueprint
#'
#' @description
#'
#' `update_blueprint()` is the correct way to alter elements of an existing
#' `blueprint` object. It has two benefits over just doing
#' `blueprint$elem <- new_elem`.
#'
#' - The name you are updating _must_ already exist in the blueprint. This prevents
#' you from accidentally updating non-existent elements.
#'
#' - The constructor for the blueprint is automatically run after the update by
#' `refresh_blueprint()` to ensure that the blueprint is still valid.
#'
#' @inheritParams refresh_blueprint
#'
#' @param ... Name-value pairs of _existing_ elements in `blueprint` that should
#' be updated.
#'
#' @examples
#'
#' blueprint <- default_xy_blueprint()
#'
#' # `intercept` defaults to FALSE
#' blueprint
#'
#' update_blueprint(blueprint, intercept = TRUE)
#'
#' # Can't update non-existent elements
#' try(update_blueprint(blueprint, intercpt = TRUE))
#'
#' # Can't add non-valid elements
#' try(update_blueprint(blueprint, intercept = 1))
#' @export
update_blueprint <- function(blueprint, ...) {
  validate_is_blueprint(blueprint)

  changes <- rlang::list2(...)

  if (!has_unique_names(changes)) {
    glubort("`...` must have unique names.")
  }

  new_nms <- names(changes)
  old_nms <- names(blueprint)

  for (nm in new_nms) {
    if (!(nm %in% old_nms)) {
      glubort(
        "All elements to change must already exist. `{nm}` is a new field."
      )
    }

    # this nukes elements if we set them to NULL
    blueprint[[nm]] <- changes[[nm]]
  }

  refresh_blueprint(blueprint)
}

# ------------------------------------------------------------------------------

#' Is `x` a preprocessing blueprint?
#'
#' `is_blueprint()` checks if `x` inherits from `"hardhat_blueprint"`.
#'
#' @param x An object.
#'
#' @examples
#' is_blueprint(default_xy_blueprint())
#' @export
is_blueprint <- function(x) {
  inherits(x, "hardhat_blueprint")
}

# ------------------------------------------------------------------------------

# helper for new_blueprint()$mold and $forge elements
blueprint_function_set <- function(clean, process) {
  validate_is(clean, rlang::is_function, "function")
  validate_is(process, rlang::is_function, "function")

  list(
    clean = clean,
    process = process
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

validate_is_ptype_list_or_null <- function(.x, .x_nm) {
  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }

  if (is.null(.x)) {
    return(invisible(.x))
  }

  validate_has_name(.x, .x_nm, "predictors")
  validate_has_name(.x, .x_nm, "outcomes")

  validate_is_0_row_tibble(.x$predictors, glue("{.x_nm}$predictors"))
  validate_is_0_row_tibble(.x$outcomes, glue("{.x_nm}$outcomes"))

  invisible(.x)
}


validate_is_0_row_tibble <- function(.x, .x_nm) {
  validate_is(.x, tibble::is_tibble, "tibble", .x_nm)

  .n <- nrow(.x)

  if (.n != 0) {
    glubort("`{.x_nm}` must be a tibble of size 0, not {.n}.")
  }

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

validate_forge_args <- function(forge) {
  required_clean_args <- c("blueprint", "new_data", "outcomes")

  actual_clean_args <- rlang::fn_fmls_names(forge$clean)

  if (!identical(actual_clean_args, required_clean_args)) {
    required_clean_args <- glue_quote_collapse(required_clean_args)

    glubort(
      "`forge$clean()` must have the following arguments: {required_clean_args}."
    )
  }

  required_process_args <- c("blueprint", "predictors", "outcomes", "extras")

  actual_process_args <- rlang::fn_fmls_names(forge$process)

  if (!identical(required_process_args, actual_process_args)) {
    required_process_args <- glue_quote_collapse(required_process_args)

    glubort(
      "`forge$process()` must have the following arguments: {required_process_args}."
    )
  }

  invisible(forge)
}
