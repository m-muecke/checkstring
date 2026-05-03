# Check if an argument is a MIME type string

Validates the `type/subtype` format where the top-level type must be one
of the IANA-registered types: `application`, `audio`, `font`, `image`,
`message`, `model`, `multipart`, `text`, or `video`.

## Usage

``` r
is_mime(x)
```

## Arguments

- x:

  (`any`)  
  Object to check.

## Value

`TRUE` if `x` is a valid MIME type string, `FALSE` otherwise.

## References

<https://www.iana.org/assignments/media-types/media-types.xhtml>

## Examples

``` r
is_mime("application/json")
#> [1] TRUE
is_mime("text/plain")
#> [1] TRUE
```
