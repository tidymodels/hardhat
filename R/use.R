#' Create a modeling package
#'
#' @description
#'
#' `create_modeling_package()` will:
#'    - Call `usethis::create_package()` to set up a new R package.
#'    - Call `use_modeling_package()` to add a modeling package skeleton to it.
#'
#' `use_modeling_package()` will:
#'    - Add hardhat and rlang to Imports
#'    - Add recipes to Suggests
#'    - If roxygen2 is available, uses roxygen markdown
#'    - Add a package documentation file
#'    - Generate and populate 7 files in `R/`:
#'       - `constructor.R`
#'       - `fit-bridge.R`
#'       - `fit-implementation.R`
#'       - `fit-interface.R`
#'       - `predict-bridge.R`
#'       - `predict-implementation.R`
#'       - `predict-interface.R`
#'
#' @param model A string. The name of the high level modeling function
#' that users will call. For example, `"linear_regression"`. This will be used
#' to populate the skeleton.
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
#' @export
use_modeling_package <- function(model) {
  validate_installed("usethis")
  validate_installed("roxygen2")
  validate_installed("devtools")

  if (!is_string(model)) {
    abort("`model` must be a string.")
  }

  if (grepl("\\s", model)) {
    abort("`model` must not contain any spaces.")
  }

  usethis::ui_info("Adding required packages to the DESCRIPTION")
  usethis::use_package("hardhat", type = "Imports")
  usethis::use_package("rlang", type = "Imports")
  usethis::use_package("recipes", type = "Suggests")
  ui_blank_line()

  usethis::ui_info("Setting up roxygen")
  usethis::use_roxygen_md()
  ui_blank_line()

  data <- list(model = model)

  use_hardhat_template <- function(template) {
    usethis::use_template(template, data = data, package = "hardhat")
  }

  usethis::ui_info("Writing skeleton files")
  usethis::use_package_doc()
  use_hardhat_template("R/constructor.R")
  use_hardhat_template("R/fit-bridge.R")
  use_hardhat_template("R/fit-implementation.R")
  use_hardhat_template("R/fit-interface.R")
  use_hardhat_template("R/predict-bridge.R")
  use_hardhat_template("R/predict-implementation.R")
  use_hardhat_template("R/predict-interface.R")
  ui_blank_line()

  invisible(model)
}

#' @rdname use_modeling_package
#' @export
create_modeling_package <- function(path,
                                    model,
                                    fields = NULL,
                                    open = interactive()) {

  validate_installed("usethis")
  validate_installed("roxygen2")
  validate_installed("devtools")

  usethis::create_package(path, fields, open = FALSE)
  ui_blank_line()

  # copied from create_package()
  old_project <- usethis::proj_set(path, force = TRUE)
  on.exit(usethis::proj_set(old_project), add = TRUE)
  ui_blank_line()

  use_modeling_package(model)

  # Only auto-document when creating _new_ packages
  # Must explicitly set the pkg path
  usethis::ui_info("Running {usethis::ui_code('devtools::document()')}")
  devtools::document(pkg = usethis::proj_get())
  ui_blank_line()

  # copied from create_package()
  if (open) {
    if (usethis::proj_activate(path)) {
      on.exit()
    }
  }

  invisible(usethis::proj_get())
}

is_string <- function (x) {
  length(x) == 1 && is.character(x)
}

validate_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    abort(paste0("The `", pkg, "` package must be installed for this functionality."))
  }
}

ui_blank_line <- function() {
  validate_installed("usethis")
  usethis::ui_line("")
}
