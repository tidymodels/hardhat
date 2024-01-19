#' Get names of predictors
#'
#' These methods can be used to compute the column names for data that were
#' given to a function (i.e., "original predictors") or columns that result
#' after a function has been executed ("actual" predictors).
#' @param x An object
#' @param include_intercept A logical: should any intercept column be included?
#' @param data Original data to be used in a model.
#' @param ... Only used in formula methods and are passed to [stats::terms()]
#' for original names and [stats::model.matrix()] for actual names.
#' @return A sorted character string of column names.
#' @name predictor-names
#' @export
.get_predictor_names_actual <- function(x, ...) {
  UseMethod(".get_predictor_names_actual")

}

#' @export
#' @rdname predictor-names
.get_predictor_names_original <- function(x, ...) {
  UseMethod(".get_predictor_names_original")
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname predictor-names
.get_predictor_names_actual.workflow <- function(x, include_intercept = FALSE, ...) {
  check_wflow_fit(x)
  res <- names(x$pre$mold$predictors)
  if (!include_intercept) {
    res <- res[res != "(Intercept)"] # TODO generalize this with doubles
  }
  sort(res)
}

#' @export
#' @rdname predictor-names
.get_predictor_names_original.workflow <- function(x, ...) {
  check_wflow_fit(x)
  .get_predictor_names_original(x$pre$mold$blueprint)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname predictor-names
.get_predictor_names_actual.recipe <- function(x, ...) {
  check_recipe_fit(x)
  var_info <- x$last_term_info
  max_steps <- max(var_info$number)
  var_info <- var_info[var_info$number == max_steps, c("variable", "role")]
  var_info <- var_info[is_predictor_role(var_info), ]
  sort(var_info$variable)
}

#' @export
#' @rdname predictor-names
.get_predictor_names_original.recipe <- function(x, ...) {
  check_recipe_fit(x)
  var_info <- x$last_term_info
  var_info <- var_info[var_info$source == "original", c("variable", "role")]
  var_info <- var_info[is_predictor_role(var_info), ]
  sort(var_info$variable)
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname predictor-names
.get_predictor_names_actual.formula <- function(x, include_intercept = FALSE, data, ...) {
  mod_mat <- stats::model.matrix(x, data = data[0,], ...)
  res <- colnames(mod_mat)
  if (!include_intercept) {
    res <- res[res != "(Intercept)"] # TODO generalize this with doubles
  }
  sort(res)
}

#' @export
#' @rdname predictor-names
.get_predictor_names_original.formula <- function(x, data, ...) {
  trms <- stats::terms(x, data = data[0,])
  sort(colnames(attr(trms,"factors")))
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname predictor-names
.get_predictor_names_original.hardhat_blueprint <- function(x, ...) {
  sort(names(x$ptypes$predictors))
}

# ------------------------------------------------------------------------------

check_wflow_fit <- function(x, call = rlang::env_parent()) {
  is_trained <- workflows::is_trained_workflow(x)
  if (!is_trained) {
    cli::cli_abort("The workflow should be trainined.", call = call)
  }
  invisible(NULL)
}

is_predictor_role <- function(x) {
  vapply(x$role, function(x) any(x == "predictor"), logical(1))
}

check_recipe_fit <- function(x, call = rlang::env_parent()) {
  is_trained <- vapply(x$steps, function(x) x$trained, logical(1))
  if (!all(is_trained)) {
    cli::cli_abort("All recipe steps should be trainined.", call = call)
  }
  invisible(NULL)
}
