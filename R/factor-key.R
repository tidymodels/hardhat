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
#' # ---------------------------------------------------------------------------
#' # Simple factor with treatment contrasts (default)
#'
#' framed <- model_frame(Sepal.Width ~ Species, iris)
#' factor_key(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Ordered factor with polynomial contrasts
#'
#' mtcars2 <- mtcars
#' mtcars2$gear_ord <- ordered(mtcars2$gear)
#' framed <- model_frame(mpg ~ gear_ord, mtcars2)
#' factor_key(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Interaction between two factors
#'
#' mtcars2 <- mtcars
#' mtcars2$cyl_fct <- factor(mtcars2$cyl)
#' mtcars2$am_fct <- factor(mtcars2$am)
#' framed <- model_frame(mpg ~ cyl_fct * am_fct, mtcars2)
#' factor_key(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Nested effects (gear nested within cyl)
#'
#' mtcars2 <- mtcars
#' mtcars2$cyl_fct <- factor(mtcars2$cyl)
#' mtcars2$gear_fct <- factor(mtcars2$gear)
#' framed <- model_frame(mpg ~ cyl_fct / gear_fct, mtcars2)
#' # This expands to: mpg ~ cyl_fct + cyl_fct:gear_fct
#' factor_key(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # No factors returns empty tibble
#'
#' framed <- model_frame(Sepal.Width ~ Sepal.Length + Petal.Width, iris)
#' factor_key(framed$terms, framed$data)
#'
#' # ---------------------------------------------------------------------------
#' # Custom contrasts
#'
#' species_sum <- iris$Species
#' contrasts(species_sum) <- contr.sum(3)
#' iris2 <- iris
#' iris2$Species <- species_sum
#'
#' framed <- model_frame(Sepal.Width ~ Species, iris2)
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
  factor_vars <- names(data_classes)[data_classes %in% c("factor", "ordered", "character")]

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
      if (grepl("contrasts can be applied only to factors with 2 or more levels", e$message)) {
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
    contributing_vars <- rownames(factors_matrix)[factors_matrix[, term_index] > 0]

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
