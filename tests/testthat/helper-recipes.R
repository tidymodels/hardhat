skip_if_no_update_role_requirements <- function() {
  skip_if(
    condition = utils::packageVersion("recipes") < "0.2.0.9002",
    "Doesn't have `recipes::update_role_requirements()`"
  )
}
