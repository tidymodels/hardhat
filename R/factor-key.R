#' Map factor variables to their numeric features
#'
#' @description
#' `factor_key()` creates a mapping between original factor variables and
#' their corresponding numeric features (e.g., binary indicator columns, etc.)
#' generated in a model matrix.
#'
#' @param x An object. For the default method, a terms object.
#' @param ... Arguments passed to methods.
#'
#' @return
#' A tibble with two columns:
#' \describe{
#'   \item{`source`}{The name of the original factor variable}
#'   \item{`derived`}{The name of the generated model matrix column}
#' }
#'
#' For interaction terms, multiple rows are returned with each source
#' factor mapped to the interaction column.
#'
#' Returns an empty tibble with the correct columns if no factors are present
#' in the terms.
#'
#' @details
#'
#' This function helps you understand how R's model matrix construction
#' converts factor variables into multiple features. It handles:
#'
#' - **Contrast types**: Different contrast methods (treatment, sum, helmert,
#'   polynomial, one-hot) produce different column naming patterns
#' - **Ordered factors**: These typically use polynomial contrasts, creating
#'   columns with `.L` (linear), `.Q` (quadratic), `.C` (cubic) suffixes
#' - **Interactions**: Interaction terms like `A:B` will have multiple rows in
#'   the output, one for each contributing factor
#' - **Nested effects**: Terms like `A/B` (which expands to `A + A:B`) are
#'   properly handled, with the nested interaction mapped to both source factors
#'
#' The function uses the same model matrix generation as [model_matrix()],
#' ensuring consistency with how your models will actually be fit.
#'
#' @examples
#' if (rlang::is_installed("modeldata")) {
#'   library(modeldata)
#'
#'   # ---------------------------------------------------------------------------
#'   # Simple factor with treatment contrasts (default)
#'
#'   framed <- model_frame(bill_length_mm ~ species, modeldata::penguins)
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # Multiple factors
#'
#'   framed <- model_frame(Income ~ Home + Job, modeldata::credit_data)
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # Interaction between two factors
#'
#'   framed <- model_frame(bill_length_mm ~ species * island, modeldata::penguins)
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # Nested effects (Job nested within Home)
#'
#'   framed <- model_frame(Income ~ Home / Job, modeldata::credit_data)
#'   # This expands to: Income ~ Home + Home:Job
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # No factors returns empty tibble
#'
#'   framed <- model_frame(compressive_strength ~ cement + water, modeldata::concrete)
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # Custom contrasts
#'
#'   penguins2 <- modeldata::penguins
#'   species_sum <- penguins2$species
#'   contrasts(species_sum) <- contr.sum(3)
#'   penguins2$species <- species_sum
#'
#'   framed <- model_frame(bill_length_mm ~ species, penguins2)
#'   factor_key(framed$terms, framed$data)
#'
#'   # ---------------------------------------------------------------------------
#'   # Using with blueprints from mold()
#'
#'   # Formula blueprint
#'   molded <- mold(bill_length_mm ~ species + island, penguins)
#'   factor_key(molded$blueprint, penguins)
#'
#'   # XY blueprint (returns empty tibble since no terms/factors)
#'   bp_xy <- default_xy_blueprint()
#'   molded_xy <- mold(modeldata::penguins[c("species", "island")],
#'                     modeldata::penguins["bill_length_mm"],
#'                     blueprint = bp_xy)
#'   factor_key(molded_xy$blueprint)
#' }
#'
#' # ---------------------------------------------------------------------------
#' # Ordered factor with polynomial contrasts
#'
#' data(mtcars)
#' mtcars2 <- mtcars
#' mtcars2$gear_ord <- ordered(mtcars2$gear)
#' framed <- model_frame(mpg ~ gear_ord, mtcars2)
#' factor_key(framed$terms, framed$data)
#'
#' @seealso
#' - [model_matrix()] for generating the design matrix
#' - [get_levels()] for extracting factor levels
#' - [stats::contrasts()] for setting contrast methods
#'
#' @export
factor_key <- function(x, ...) {
  UseMethod("factor_key")
}

#' @param data A data frame or tibble containing the variables in `x`.
#' @inheritParams validate_column_names
#'
#' @rdname factor_key
#' @export
factor_key.terms <- function(x, data, ..., call = current_env()) {
  check_dots_empty0(...)
  check_terms(x, call = call)
  check_data_frame_or_matrix(data, call = call)
  data <- coerce_to_tibble(data)

  # Get the data classes from the terms to identify factors
  data_classes <- attr(x, "dataClasses")

  # If no dataClasses attribute, try to infer from the data
  if (is.null(data_classes)) {
    # Get variable names from the terms
    term_vars <- all.vars(x)
    data_classes <- vapply(
      term_vars,
      get_first_class,
      character(1),
      data = data
    )
    names(data_classes) <- term_vars
  }

  # Identify factor and ordered variables
  # Note: "ordered" is a subclass of "factor" but stored separately in dataClasses
  factor_vars <- names(data_classes)[
    data_classes %in% c("factor", "ordered", "character")
  ]

  # If no factors, return empty tibble with correct structure
  if (length(factor_vars) == 0) {
    return(tibble::tibble(source = character(), derived = character()))
  }

  # Generate the model matrix to get actual column names
  # Use with_na_pass to handle missing values properly
  # Also wrap in tryCatch to handle single-level factors gracefully
  mm <- tryCatch(
    {
      with_na_pass(model.matrix(x, data))
    },
    error = function(e) {
      # Check if it's the single-level factor error
      if (
        grepl(
          "contrasts can be applied only to factors with 2 or more levels",
          e$message
        )
      ) {
        # Return NULL to indicate no model matrix could be created
        return(NULL)
      }
      # Re-throw other errors
      stop(e)
    }
  )

  # If model matrix couldn't be created (e.g., single-level factors), return empty tibble
  if (is.null(mm)) {
    return(tibble::tibble(source = character(), derived = character()))
  }

  # Get the assign attribute which maps columns to term indices
  assign_attr <- attr(mm, "assign")
  mm_colnames <- colnames(mm)

  # Get the factors matrix from terms (shows which variables contribute to each term)
  factors_matrix <- attr(x, "factors")

  # If no factors matrix (e.g., intercept-only model), return empty tibble
  if (is.null(factors_matrix)) {
    return(tibble::tibble(source = character(), derived = character()))
  }

  # Get term labels
  term_labels <- attr(x, "term.labels")

  # Build mapping data
  mapping_list <- list()

  for (i in seq_along(mm_colnames)) {
    col_name <- mm_colnames[i]
    term_index <- assign_attr[i]

    # Skip intercept (term_index == 0)
    if (term_index == 0) {
      next
    }

    # Get the term label
    term_label <- term_labels[term_index]

    # Find which variables contribute to this term
    contributing_vars <- rownames(factors_matrix)[
      factors_matrix[, term_index] > 0
    ]

    # Filter to only factor variables
    factor_contributors <- intersect(contributing_vars, factor_vars)

    # If this column has factor contributors, add to mapping
    if (length(factor_contributors) > 0) {
      for (factor_var in factor_contributors) {
        mapping_list[[length(mapping_list) + 1]] <- data.frame(
          source = factor_var,
          derived = col_name,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # Combine all mappings into a single data frame
  if (length(mapping_list) > 0) {
    result <- do.call(rbind, mapping_list)
    result <- tibble::as_tibble(result)
  } else {
    result <- tibble::tibble(source = character(), derived = character())
  }

  result
}

# Helper function to get the first class of a variable
get_first_class <- function(var, data) {
  if (var %in% names(data)) {
    return(class(data[[var]])[1])
  } else {
    return("unknown")
  }
}

# ------------------------------------------------------------------------------
# Blueprint methods

#' @param data A data frame or tibble containing the variables used during
#'   model fitting. Required for blueprint methods since blueprints don't
#'   store the original data.
#'
#' @rdname factor_key
#' @export
factor_key.default_formula_blueprint <- function(
  x,
  data,
  ...,
  call = current_env()
) {
  check_dots_empty0(...)

  # Extract the predictors terms from the blueprint
  terms_obj <- x$terms$predictors

  if (is.null(terms_obj)) {
    cli::cli_abort(
      "Blueprint does not contain terms for predictors.",
      call = call
    )
  }

  # Call the terms method
  factor_key.terms(terms_obj, data, call = call)
}

#' @rdname factor_key
#' @export
factor_key.formula_blueprint <- function(x, data, ..., call = current_env()) {
  # Fallback for non-default formula blueprints
  # Try to extract terms if available, otherwise error
  check_dots_empty0(...)

  if (!is.null(x$terms) && !is.null(x$terms$predictors)) {
    factor_key.terms(x$terms$predictors, data, call = call)
  } else {
    cli::cli_abort(
      "Cannot extract factor mappings from this formula blueprint type.",
      call = call
    )
  }
}

#' @rdname factor_key
#' @export
factor_key.default_recipe_blueprint <- function(
  x,
  data = NULL,
  ...,
  call = current_env()
) {
  check_dots_empty0(...)

  # Recipes handle factors differently - they may encode them during prep
  # For now, return a message indicating this isn't directly supported
  cli::cli_abort(
    c(
      "factor_key() is not yet implemented for recipe blueprints.",
      "i" = "Recipes handle factor encoding during the prep step.",
      "i" = "Consider using a formula blueprint if you need factor mappings."
    ),
    call = call
  )
}

#' @rdname factor_key
#' @export
factor_key.recipe_blueprint <- function(
  x,
  data = NULL,
  ...,
  call = current_env()
) {
  # Fallback for non-default recipe blueprints
  factor_key.default_recipe_blueprint(x, data, ..., call = call)
}

#' @rdname factor_key
#' @export
factor_key.default_xy_blueprint <- function(
  x,
  data = NULL,
  ...,
  call = current_env()
) {
  check_dots_empty0(...)

  # XY blueprints don't use terms or formulas, so there's no factor encoding to map
  # Return empty tibble to be consistent with no-factors case
  tibble::tibble(source = character(), derived = character())
}

#' @rdname factor_key
#' @export
factor_key.xy_blueprint <- function(x, data = NULL, ..., call = current_env()) {
  # Fallback for non-default XY blueprints
  factor_key.default_xy_blueprint(x, data, ..., call = call)
}
