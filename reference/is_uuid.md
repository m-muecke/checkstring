# Check if an argument is an UUID string

Check if an argument is an UUID string

## Usage

``` r
is_uuid(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid UUID string, `FALSE` otherwise.

## Examples

``` r
is_uuid("550e8400-e29b-41d4-a716-446655440000")
#> [1] TRUE
```
