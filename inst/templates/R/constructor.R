new_{{model}}_model <- function(coefs, blueprint) {
  hardhat::new_model(coefs = coefs, blueprint = blueprint, class = "{{model}}")
}
