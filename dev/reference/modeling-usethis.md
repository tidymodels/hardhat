# Create a modeling package

`create_modeling_package()` will:

- Call
  [`usethis::create_package()`](https://usethis.r-lib.org/reference/create_package.html)
  to set up a new R package.

- Call `use_modeling_deps()`.

- Call `use_modeling_files()`.

`use_modeling_deps()` will:

- Add hardhat, rlang, and stats to Imports

- Add recipes to Suggests

- If roxygen2 is available, use roxygen markdown

`use_modeling_files()` will:

- Add a package documentation file

- Generate and populate 3 files in `R/`:

  - `{{model}}-constructor.R`

  - `{{model}}-fit.R`

  - `{{model}}-predict.R`

## Usage

``` r
create_modeling_package(path, model, fields = NULL, open = interactive())

use_modeling_deps()

use_modeling_files(model)
```

## Arguments

- path:

  A path. If it exists, it is used. If it does not exist, it is created,
  provided that the parent path exists.

- model:

  A string. The name of the high level modeling function that users will
  call. For example, `"linear_regression"`. This will be used to
  populate the skeleton. Spaces are not allowed.

- fields:

  A named list of fields to add to DESCRIPTION, potentially overriding
  default values. See
  [`usethis::use_description()`](https://usethis.r-lib.org/reference/use_description.html)
  for how you can set personalized defaults using package options.

- open:

  If TRUE, activates the new project:

  - If RStudio desktop, the package is opened in a new session.

  - If on RStudio server, the current RStudio project is activated.

  - Otherwise, the working directory and active project is changed.

## Value

`create_modeling_package()` returns the project path invisibly.

`use_modeling_deps()` returns invisibly.

`use_modeling_files()` return `model` invisibly.
