# Check if an argument is a hex color string

Validates hex color codes in `#RGB`, `#RGBA`, `#RRGGBB`, or `#RRGGBBAA`
format.

## Usage

``` r
is_color_hex(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid hex color string, `FALSE` otherwise.

## References

<https://www.w3.org/TR/css-color-4/#hex-notation>

## Examples

``` r
is_color_hex("#FF5733")
#> [1] TRUE
is_color_hex("#fff")
#> [1] TRUE
```
