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

