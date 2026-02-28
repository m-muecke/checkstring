# Check if an argument is a base64 string

Check if an argument is a base64 string

## Usage

``` r
is_base64(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid base64 string, `FALSE` otherwise.

## Examples

``` r
is_base64("SGVsbG8gV29ybGQ=")
#> [1] TRUE
```
