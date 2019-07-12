context("test-package-creation")

pkg_path <- tempfile("fruit")
fruits <- c("cherry", "grape")

test_that("create package", {

  expect_output(
    expect_error(
      create_modeling_package(pkg_path, fruits, open = FALSE),
      regex = NA
    )
  )

  top_lvl_files <- list.files(pkg_path)

  expect_true(sum(top_lvl_files == "DESCRIPTION") == 1)
  expect_true(sum(top_lvl_files == "R") == 1)
  expect_true(sum(top_lvl_files == "man") == 1)
  expect_true(sum(top_lvl_files == "NAMESPACE") == 1)

  r_files <- list.files(file.path(pkg_path, "R"))

  expect_true(sum(grepl("-package.R", r_files, fixed = TRUE)) == 1)
  for (i in fruits) {
    expect_true(sum(grepl(i, r_files)) == 3)
    expect_true(sum(r_files == paste0(i, "-constructor.R")) == 1)
    expect_true(sum(r_files == paste0(i, "-fit.R")) == 1)
    expect_true(sum(r_files == paste0(i, "-predict.R")) == 1)
  }

})


test_that("bad input", {
  expect_error(
    create_modeling_package(model = c("cherry", "grape")),
    regex = 'argument "path" is missing, with no default'
  )
  expect_output(
    expect_error(
      create_modeling_package(tempfile("nuts"), model = c("black cherry", "grape")),
      regex = '`model` must not contain strings with spaces'
    )
  )
})
