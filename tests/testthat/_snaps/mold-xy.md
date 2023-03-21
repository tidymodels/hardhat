# unknown mold() inputs throw an error

    Code
      mold("hi")
    Condition
      Error in `mold()`:
      ! `x` must be a data frame, matrix, recipe, or formula, not the string "hi".

# cannot pass anything in the dots

    Code
      mold(iris[, "Sepal.Length", drop = FALSE], iris$Species, z = "in the dots")
    Condition
      Error in `mold()`:
      ! `...` must be empty.
      x Problematic argument:
      * z = "in the dots"

---

    Code
      mold(iris[, "Sepal.Length", drop = FALSE], iris$Species, blueprint = default_xy_blueprint(
        composition = "dgCMatrix"), z = "in the dots")
    Condition
      Error in `mold()`:
      ! `...` must be empty.
      x Problematic argument:
      * z = "in the dots"

