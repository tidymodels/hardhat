# no `model` aborts normally

    Code
      create_modeling_package(path = "my/path")
    Condition
      Error in `create_modeling_package()`:
      ! `model` is absent but must be supplied.

# no `path` aborts normally

    Code
      create_modeling_package(model = "my_model")
    Condition
      Error in `create_modeling_package()`:
      ! `path` is absent but must be supplied.

# `model` can only be a single string

    Code
      create_modeling_package(path = "my/path", model = c("model1", "model2"))
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string, not a character vector.

---

    Code
      create_modeling_package(path = "my/path", model = 1)
    Condition
      Error in `create_modeling_package()`:
      ! `model` must be a single string, not the number 1.

---

    Code
      create_modeling_package(path = "my/path", model = "model with space")
    Condition
      Error in `create_modeling_package()`:
      ! `model` must not contain any spaces.

