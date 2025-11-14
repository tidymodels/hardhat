# Extend case weights

**\[experimental\]**

`new_case_weights()` is a developer oriented function for constructing a
new case weights type. The `<case_weights>` type itself is an *abstract*
type with very little functionality. Because of this, `class` is a
required argument.

## Usage

``` r
new_case_weights(x, ..., class)
```

## Arguments

- x:

  An integer or double vector.

- ...:

  Name-value pairs defining attributes

- class:

  Name of subclass.

## Value

A new subclassed case weights vector.

## Examples

``` r
new_case_weights(1:5, class = "my_weights")
#> <my_weights[5]>
#> [1] 1 2 3 4 5
```
