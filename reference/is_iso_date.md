# Check if an argument is an ISO 8601 date string

Validates the `YYYY-MM-DD` format with calendar correctness, including
leap year handling.

## Usage

``` r
is_iso_date(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid ISO 8601 date string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/ISO_8601>

## Examples

``` r
is_iso_date("2024-01-15")
#> [1] TRUE
is_iso_date("2024-02-30")
#> [1] FALSE
```
