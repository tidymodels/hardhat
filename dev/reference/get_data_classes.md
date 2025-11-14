# Extract data classes from a data frame or matrix

When predicting from a model, it is often important for the `new_data`
to have the same classes as the original data used to fit the model.
`get_data_classes()` extracts the classes from the original training
data.

## Usage

``` r
get_data_classes(data, ..., call = current_env())
```

## Arguments

- data:

  A data frame or matrix.

- ...:

  These dots are for future extensions and must be empty.

- call:

  The call used for errors and warnings.

## Value

A named list. The names are the column names of `data` and the values
are character vectors containing the class of that column.

## Examples

``` r
get_data_classes(iris)
#> $Sepal.Length
#> [1] "numeric"
#> 
#> $Sepal.Width
#> [1] "numeric"
#> 
#> $Petal.Length
#> [1] "numeric"
#> 
#> $Petal.Width
#> [1] "numeric"
#> 
#> $Species
#> [1] "factor"
#> 

get_data_classes(as.matrix(mtcars))
#> $mpg
#> [1] "numeric"
#> 
#> $cyl
#> [1] "numeric"
#> 
#> $disp
#> [1] "numeric"
#> 
#> $hp
#> [1] "numeric"
#> 
#> $drat
#> [1] "numeric"
#> 
#> $wt
#> [1] "numeric"
#> 
#> $qsec
#> [1] "numeric"
#> 
#> $vs
#> [1] "numeric"
#> 
#> $am
#> [1] "numeric"
#> 
#> $gear
#> [1] "numeric"
#> 
#> $carb
#> [1] "numeric"
#> 

# Unlike .MFclass(), the full class
# vector is returned
data <- data.frame(col = ordered(c("a", "b")))

.MFclass(data$col)
#> [1] "ordered"

get_data_classes(data)
#> $col
#> [1] "ordered" "factor" 
#> 
```
