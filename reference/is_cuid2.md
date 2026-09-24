# Check if an argument is a CUID2 string

Validates CUID2 strings of 24 (the default length) to 32 (the maximum
length) characters.

## Usage

``` r
is_cuid2(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid CUID2 string, `FALSE` otherwise.

## References

<https://github.com/paralleldrive/cuid2>

## Examples

``` r
is_cuid2("ckopqwooh000001la8mbi2im9")
#> [1] TRUE
```
