# Delete the response from a terms object

`delete_response()` is exactly the same as
[`delete.response()`](https://rdrr.io/r/stats/delete.response.html),
except that it fixes a long standing bug by also removing the part of
the `"dataClasses"` attribute corresponding to the response, if it
exists.

## Usage

``` r
delete_response(terms)
```

## Arguments

- terms:

  A terms object.

## Value

`terms` with the response sections removed.

## Details

The bug is described here:

<https://stat.ethz.ch/pipermail/r-devel/2012-January/062942.html>

## Examples

``` r
framed <- model_frame(Species ~ Sepal.Width, iris)

attr(delete.response(framed$terms), "dataClasses")
#>     Species Sepal.Width 
#>    "factor"   "numeric" 

attr(delete_response(framed$terms), "dataClasses")
#> Sepal.Width 
#>   "numeric" 
```
