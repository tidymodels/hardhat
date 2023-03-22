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

