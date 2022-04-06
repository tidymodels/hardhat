test_that("can create a modeling package", {
  skip_on_cran()
  local_options(usethis.quiet = TRUE)

  dir_base <- tempdir()
  dir_pkg <- file.path(dir_base, "model")
  dir.create(dir_pkg)
  on.exit(unlink(dir_pkg, recursive = TRUE), add = TRUE)

  model <- "linear_regression"

  # `usethis.quiet = TRUE` silences most of the messages, but there is an
  # unavoidable `i Loading model` that we get from devtools if we don't do this
  suppressMessages({
    create_modeling_package(dir_pkg, model, open = FALSE)
  })

  top_level_files <- list.files(dir_pkg)
  script_files <- list.files(file.path(dir_pkg, "R"))

  expect_true("DESCRIPTION" %in% top_level_files)
  expect_true("R" %in% top_level_files)
  expect_true("man" %in% top_level_files)
  expect_true("NAMESPACE" %in% top_level_files)

  expect_true(glue::glue("{model}-constructor.R") %in% script_files)
  expect_true(glue::glue("{model}-fit.R") %in% script_files)
  expect_true(glue::glue("{model}-predict.R") %in% script_files)
})

test_that("can add a second model to a modeling package", {
  skip_on_cran()
  local_options(usethis.quiet = TRUE)

  dir_base <- tempdir()
  dir_pkg <- file.path(dir_base, "model")
  dir.create(dir_pkg)
  on.exit(unlink(dir_pkg, recursive = TRUE), add = TRUE)

  model1 <- "linear_regression"
  model2 <- "random_forest"

  # `usethis.quiet = TRUE` silences most of the messages, but there is an
  # unavoidable `i Loading model` that we get from devtools if we don't do this
  suppressMessages({
    create_modeling_package(dir_pkg, model1, open = FALSE)
  })

  with_dir <- function(new, code) {
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
  }

  with_dir(dir_pkg, use_modeling_files(model2))

  script_files <- list.files(file.path(dir_pkg, "R"))

  expect_true(glue::glue("{model1}-constructor.R") %in% script_files)
  expect_true(glue::glue("{model1}-fit.R") %in% script_files)
  expect_true(glue::glue("{model1}-predict.R") %in% script_files)

  expect_true(glue::glue("{model2}-constructor.R") %in% script_files)
  expect_true(glue::glue("{model2}-fit.R") %in% script_files)
  expect_true(glue::glue("{model2}-predict.R") %in% script_files)
})

test_that("no `model` aborts normally", {
  expect_snapshot(error = TRUE, create_modeling_package(path = "my/path"))
})

test_that("no `path` aborts normally", {
  expect_snapshot(error = TRUE, create_modeling_package(model = "my_model"))
})

test_that("`model` can only be a single string", {
  expect_snapshot(error = TRUE, create_modeling_package(path = "my/path", model = c("model1", "model2")))
  expect_snapshot(error = TRUE, create_modeling_package(path = "my/path", model = 1))
  expect_snapshot(error = TRUE, create_modeling_package(path = "my/path", model = "model with space"))
})
