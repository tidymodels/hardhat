# no `model` aborts normally

    Code
      create_modeling_package(path = "my/path")
    Condition
      Error in `force()`:
      ! argument "model" is missing, with no default

# no `path` aborts normally

    Code
      create_modeling_package(model = "my_model")
    Condition
      Error in `force()`:
      ! argument "path" is missing, with no default

# `model` can only be a single string

    Code
      create_modeling_package(path = "my/path", model = c("model1", "model2"))
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string.

---

    Code
      create_modeling_package(path = "my/path", model = 1)
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string.

---

    Code
      create_modeling_package(path = "my/path", model = "model with space")
    Condition
      Error in `create_modeling_package()`:
      ! `model` must not contain any spaces.

