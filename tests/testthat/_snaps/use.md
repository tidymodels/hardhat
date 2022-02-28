# no `model` aborts early

    Code
      create_modeling_package("my/path")
    Error <simpleError>
      argument "model" is missing, with no default

# no `path` aborts normally

    Code
      create_modeling_package(model = "my_model")
    Error <simpleError>
      argument "path" is missing, with no default

# `model` can only be a single string

    Code
      create_modeling_package(model = c("model1", "model2"))
    Error <rlang_error>
      `model` must be a single string.

---

    Code
      create_modeling_package(model = 1)
    Error <rlang_error>
      `model` must be a single string.

---

    Code
      create_modeling_package(model = "model with space")
    Error <rlang_error>
      `model` must not contain any spaces.

