# Check if an argument is a SHA-256 hash string

Check if an argument is a SHA-256 hash string

## Usage

``` r
is_sha256(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid SHA-256 hash string, `FALSE` otherwise.

## Examples

``` r
is_sha256("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
#> [1] TRUE
```
