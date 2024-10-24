#' Create a modeling package
#'
#' @description
#'
#' `create_modeling_package()` will:
#'    - Call `usethis::create_package()` to set up a new R package.
#'    - Call `use_modeling_deps()`.
#'    - Call `use_modeling_files()`.
#'
#' `use_modeling_deps()` will:
#'    - Add hardhat, rlang, and stats to Imports
#'    - Add recipes to Suggests
#'    - If roxygen2 is available, use roxygen markdown
#'
#' `use_modeling_files()` will:
#'    - Add a package documentation file
#'    - Generate and populate 3 files in `R/`:
#'       - `{{model}}-constructor.R`
#'       - `{{model}}-fit.R`
#'       - `{{model}}-predict.R`
#'
#' @param model A string. The name of the high level modeling function that
#' users will call. For example, `"linear_regression"`. This will be used to
#' populate the skeleton. Spaces are not allowed.
#'
#' @param path A path. If it exists, it is used. If it does not exist,
#' it is created, provided that the parent path exists.
#'
#' @param fields A named list of fields to add to DESCRIPTION,
#' potentially overriding default values. See `usethis::use_description()` for
#' how you can set personalized defaults using package options.
#'
#' @param open If TRUE, activates the new project:
#'   - If RStudio desktop, the package is opened in a new session.
#'   - If on RStudio server, the current RStudio project is activated.
#'   - Otherwise, the working directory and active project is changed.
#'
#' @return
#'
#' `create_modeling_package()` returns the project path invisibly.
#'
#' `use_modeling_deps()` returns invisibly.
#'
#' `use_modeling_files()` return `model` invisibly.
#'
#' @name modeling-usethis
#' @export
create_modeling_package <- function(path,
                                    model,
                                    fields = NULL,
                                    open = interactive()) {
  check_required(path)
  check_required(model)

  check_installed("usethis")
  check_installed("roxygen2")
  check_installed("devtools")
  check_installed("recipes")
  check_installed("withr")

  # Avoid creating files if a bad model is supplied
  check_string(model)

  if (has_spaces(model)) {
    cli::cli_abort("{.arg model} must not contain any spaces.")
  }

  usethis::create_package(path, fields, open = FALSE)

  # copied from create_package()
  usethis::local_project(path, force = TRUE)
  ui_blank_line()

  use_modeling_deps()
  use_modeling_files_impl(model, prompt_document = FALSE)

  # Use the same option as used by the usethis `ui_*()` family
  quiet <- getOption("usethis.quiet", default = FALSE)

  # Only auto-document when creating _new_ packages
  # Must explicitly set the pkg path
  usethis::ui_info("Running {usethis::ui_code('devtools::document()')}")
  devtools::document(pkg = usethis::proj_get(), quiet = quiet)
  ui_blank_line()

  # copied from create_package()
  if (open) {
    if (usethis::proj_activate(path)) {
      withr::deferred_clear()
    }
  }

  invisible(usethis::proj_get())
}

#' @rdname modeling-usethis
#' @export
use_modeling_deps <- function() {
  check_installed("usethis")
  check_installed("roxygen2")
  check_installed("devtools")
  check_installed("recipes")

  usethis::ui_info("Adding required packages to the DESCRIPTION")
  usethis::use_package("hardhat", type = "Imports")
  usethis::use_package("rlang", type = "Imports")
  usethis::use_package("stats", type = "Imports")
  usethis::use_package("recipes", type = "Suggests")
  ui_blank_line()

  usethis::ui_info("Setting up roxygen")
  usethis::use_roxygen_md()
  ui_blank_line()

  invisible()
}

#' @rdname modeling-usethis
#' @export
use_modeling_files <- function(model) {
  use_modeling_files_impl(model)
}

use_modeling_files_impl <- function(model, prompt_document = TRUE) {
  check_installed("usethis")

  check_string(model)

  if (has_spaces(model)) {
    cli::cli_abort("{.arg model} must not contain any spaces.")
  }

  data <- list(model = model)

  use_hardhat_template <- function(template, save_as) {
    usethis::use_template(
      template = template,
      save_as = save_as,
      data = data,
      package = "hardhat"
    )
  }

  path_constructor <- glue::glue("R/{model}-constructor.R")
  path_fit <- glue::glue("R/{model}-fit.R")
  path_predict <- glue::glue("R/{model}-predict.R")

  usethis::ui_info("Writing skeleton files")
  usethis::use_package_doc(open = FALSE)
  use_hardhat_template("R/constructor.R", path_constructor)
  use_hardhat_template("R/fit.R", path_fit)
  use_hardhat_template("R/predict.R", path_predict)

  if (prompt_document) {
    usethis::ui_todo("Run {usethis::ui_code('devtools::document()')}")
  } else {
    ui_blank_line()
  }

  invisible(model)
}

# ------------------------------------------------------------------------------

has_spaces <- function(x) {
  grepl("\\s", x)
}

ui_blank_line <- function() {
  check_installed("usethis")
  usethis::ui_line("")
}
