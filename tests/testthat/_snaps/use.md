# no `model` aborts early

    Code
      create_modeling_package("my/path")
    Condition
      Error in `is_string()`:
      ! argument "model" is missing, with no default

# no `path` aborts normally

    Code
      create_modeling_package(model = "my_model")
    Condition
      Error in `path_expand()`:
      ! argument "path" is missing, with no default

# `model` can only be a single string

    Code
      create_modeling_package(model = c("model1", "model2"))
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string.

---

    Code
      create_modeling_package(model = 1)
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string.

---

    Code
      create_modeling_package(model = "model with space")
    Condition
      Error in `create_modeling_package()`:
      ! `model` must not contain any spaces.

