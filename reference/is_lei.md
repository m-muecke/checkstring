# Check if an argument is a valid LEI string

Validates LEI (Legal Entity Identifier) format including MOD-97-10 check
digit verification (ISO/IEC 7064).

## Usage

``` r
is_lei(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid LEI string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/Legal_Entity_Identifier>
<https://www.govinfo.gov/content/pkg/CFR-2016-title12-vol8/xml/CFR-2016-title12-vol8-part1003-appC.xml>

## Examples

``` r
is_lei("7H6GLXDRUGQFU57RNE97")
#> [1] TRUE
```
