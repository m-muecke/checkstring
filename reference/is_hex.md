# Check if an argument is a hexadecimal string

Check if an argument is a hexadecimal string

## Usage

``` r
is_hex(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid non-empty hexadecimal string, `FALSE`
otherwise.

## Examples

``` r
is_hex("deadbeef")
#> [1] TRUE
```
