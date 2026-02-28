# Check if an argument is a DOI string

Check if an argument is a DOI string

## Usage

``` r
is_doi(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid DOI string, `FALSE` otherwise.

## References

<https://www.doi.org/the-identifier/what-is-a-doi/>

## Examples

``` r
is_doi("10.1038/nphys1170")
#> [1] TRUE
```
