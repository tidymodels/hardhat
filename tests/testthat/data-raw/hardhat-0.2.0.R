# Objects used for backwards compatibility testing.
# Objects created with hardhat 0.2.0.
# devtools::install_version("hardhat", "0.2.0")

# ------------------------------------------------------------------------------
# Testing compatibility of `mold()` and a basic recipe

dir <- here::here("tests", "testthat", "data")
file <- fs::path(dir, "hardhat-0.2.0-pre-mold-recipe.rds")

data <- tibble::tibble(y = 1:5, x = 6:10)

blueprint <- hardhat::default_recipe_blueprint()

object <- list(data = data, blueprint = blueprint)

saveRDS(
  object,
  file = file,
  version = 2
)

# ------------------------------------------------------------------------------
# Testing compatibility of `forge()` and a basic recipe

dir <- here::here("tests", "testthat", "data")
file <- fs::path(dir, "hardhat-0.2.0-post-mold-recipe.rds")

data <- tibble::tibble(y = 1:5, x = 6:10)
new_data <- tibble::tibble(y = 6:10, x = 11:15)

rec <- recipes::recipe(y ~ ., data = data)
rec <- recipes::step_mutate(rec, z = 1)

blueprint <- hardhat::default_recipe_blueprint()
mold <- hardhat::mold(rec, data = data, blueprint = blueprint)
blueprint <- mold$blueprint

object <- list(new_data = new_data, blueprint = blueprint)

saveRDS(
  object,
  file = file,
  version = 2
)

# ------------------------------------------------------------------------------
# Testing compatibility of `forge()` and a recipe with a nonstandard role

dir <- here::here("tests", "testthat", "data")
file <- fs::path(dir, "hardhat-0.2.0-post-mold-recipe-nonstandard-role.rds")

data <- tibble::tibble(y = 1:5, x = 6:10, id = 1:5)
new_data <- tibble::tibble(y = 6:10, x = 11:15, id = 6:10)

rec <- recipes::recipe(y ~ ., data = data)
rec <- recipes::update_role(rec, id, new_role = "id")
rec <- recipes::step_mutate(rec, z = 1)

blueprint <- hardhat::default_recipe_blueprint()
mold <- hardhat::mold(rec, data = data, blueprint = blueprint)
blueprint <- mold$blueprint

object <- list(new_data = new_data, blueprint = blueprint)

saveRDS(
  object,
  file = file,
  version = 2
)

# ------------------------------------------------------------------------------
