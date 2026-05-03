# Check if an argument is an ISO 8601 datetime string

Validates the `YYYY-MM-DDTHH:MM:SS[.sss]` format with calendar
correctness, including leap year handling. The timezone designator is
optional and can be `Z` or an offset like `+05:30`.

## Usage

``` r
is_iso_datetime(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid ISO 8601 datetime string, `FALSE` otherwise.

## References

<https://en.wikipedia.org/wiki/ISO_8601>

## Examples

``` r
is_iso_datetime("2024-01-15T12:00:00Z")
#> [1] TRUE
is_iso_datetime("2024-01-15T12:00:00+05:30")
#> [1] TRUE
is_iso_datetime("2024-01-15T12:00:00")
#> [1] TRUE
```
