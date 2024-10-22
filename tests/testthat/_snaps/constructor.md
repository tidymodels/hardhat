# print - hardhat_model

    Code
      new_model()
    Output
      <hardhat_model> 
      named list()
    Code
      new_model(class = "custom_class")
    Output
      <custom_class> 
      named list()
    Code
      new_model(x = 4, y = "hi", class = "custom_class")
    Output
      <custom_class> 
      $x
      [1] 4
      
      $y
      [1] "hi"
      

# must use a valid blueprint

    Code
      new_model(blueprint = 1, class = "custom")
    Condition
      Error in `new_model()`:
      ! `blueprint` must be a <hardhat_blueprint>, not the number 1.

# `new_scalar()` must have elements

    Code
      new_scalar(list())
    Condition
      Error in `check_elems()`:
      ! `elems` must be a list of length 1 or greater.

# `new_scalar()` must have unique names

    Code
      new_scalar(list(x = 1, x = 2))
    Condition
      Error in `check_elems()`:
      ! `elems` must have unique names.

# `new_scalar()` must have no extra attributes

    Code
      new_scalar(x)
    Condition
      Error in `check_elems()`:
      ! `elems` must have no attributes (apart from names).

