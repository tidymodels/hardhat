# hardhat

## Introduction

hardhat is a *developer focused* package designed to ease the creation
of new modeling packages, while simultaneously promoting good R modeling
package standards as laid out by the set of opinionated [Conventions for
R Modeling
Packages](https://tidymodels.github.io/model-implementation-principles/).

hardhat has four main goals:

- Easily, consistently, and robustly preprocess data at fit time and
  prediction time with
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) and
  [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).

- Provide one source of truth for common input validation functions,
  such as checking if new data at prediction time contains the same
  required columns used at fit time.

- Provide extra utility functions for additional common tasks, such as
  adding intercept columns, standardizing
  [`predict()`](https://rdrr.io/r/stats/predict.html) output, and
  extracting valuable class and factor level information from the
  predictors.

- Reimagine the base R preprocessing infrastructure of
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
  and [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)
  using the stricter approaches found in
  [`model_matrix()`](https://hardhat.tidymodels.org/dev/reference/model_matrix.md)
  and
  [`model_frame()`](https://hardhat.tidymodels.org/dev/reference/model_frame.md).

The idea is to reduce the burden of creating a good modeling interface
as much as possible, and instead let the package developer focus on
writing the core implementation of their new model. This benefits not
only the developer, but also the user of the modeling package, as the
standardization allows users to build a set of “expectations” around
what any modeling function should return, and how they should interact
with it.

## Installation

You can install the released version of hardhat from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hardhat")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("tidymodels/hardhat")
```

## Learning more

To learn about how to use hardhat, check out the vignettes:

- [`vignette("mold", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/mold.md):
  Learn how to preprocess data at fit time with
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md).

- [`vignette("forge", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/forge.md):
  Learn how to preprocess new data at prediction time with
  [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md).

- [`vignette("package", "hardhat")`](https://hardhat.tidymodels.org/dev/articles/package.md):
  Learn how to use
  [`mold()`](https://hardhat.tidymodels.org/dev/reference/mold.md) and
  [`forge()`](https://hardhat.tidymodels.org/dev/reference/forge.md) to
  help in creating a new modeling package.

You can also watch [Max Kuhn](https://github.com/topepo) discuss how to
use hardhat to build a new modeling package from scratch at the XI
Jornadas de Usuarios de R conference
[here](https://canal.uned.es/video/5dd25b9f5578f275e407dd88).

[![](https://i.imgur.com/XKIZfWd.png)](https://canal.uned.es/video/5dd25b9f5578f275e407dd88)

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on RStudio
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/hardhat/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
