# Check if an argument is a valid SEDOL string

Validates SEDOL format including weighted check digit verification.

## Usage

``` r
is_sedol(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid SEDOL string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/SEDOL>

## Examples

``` r
is_sedol("0263494")
#> [1] TRUE
```
