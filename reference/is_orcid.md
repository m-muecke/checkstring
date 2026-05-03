# Check if an argument is an ORCID string

Validates ORCID format including ISO/IEC 7064:2003, MOD 11-2 check digit
verification.

## Usage

``` r
is_orcid(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid ORCID string, `FALSE` otherwise.

## References

<https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier>

## Examples

``` r
is_orcid("0000-0002-1825-0097")
#> [1] TRUE
```
