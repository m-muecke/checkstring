# Check if an argument is a valid ISIN string

Validates ISIN (International Securities Identification Number) format
including Luhn check digit verification.

## Usage

``` r
is_isin(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid ISIN string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/International_Securities_Identification_Number>

## Examples

``` r
is_isin("US0378331005")
#> [1] TRUE
```
