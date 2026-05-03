# Check if an argument is a semantic versioning string

Validates semantic versioning 2.0.0 format, including optional
pre-release and build metadata. Uses the official recommended regex from
<https://semver.org/>.

## Usage

``` r
is_semver(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid semver string, `FALSE` otherwise.

## References

<https://semver.org/>

## Examples

``` r
is_semver("1.0.0")
#> [1] TRUE
```
